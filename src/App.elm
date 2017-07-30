module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Scheme
import Material.Textfield as Textfield
import Material.Options as Options exposing (css, cs)
import Material.Button as Button
import Material.Icon as Icon
import Material.List as List
import DictList exposing (..)
import Date exposing (Date)
import Task
import Date.Format
import GraphRequests
import CommonTypes exposing (..)


-- MODEL


type TextFieldDataType
    = InputValue
    | ComputedValue


type alias TextFieldData =
    { mdlId : List Int
    , name : String
    , type_ : TextFieldDataType
    , value : Float
    , label : String
    }


type alias Model =
    { textFieldsData : DictList String TextFieldData
    , history : History
    , tempHistoryItem : Maybe HistoryItem
    , mdl : Material.Model
    }


model : Model
model =
    { textFieldsData =
        DictList.fromList <|
            List.map (\data -> ( data.name, data ))
                [ TextFieldData [ 0, 0 ] "mysaMurtaRozdil" InputValue 2000 "Mýša, Murťa rozdíl"
                , TextFieldData [ 0, 1 ] "internet" InputValue 499 "Internet"
                , TextFieldData [ 0, 2 ] "majitelka" InputValue 8000 "Majitelka"
                , TextFieldData [ 0, 3 ] "najem" InputValue 6483 "Nájem (součet všech píčovin)"
                , TextFieldData [ 0, 4 ] "murtaPosleMysovi" ComputedValue 0 "Murťa pošle Mýšovi"
                , TextFieldData [ 0, 5 ] "mysaPosleZaNet" ComputedValue 0 "Mýša pošle za net"
                , TextFieldData [ 0, 6 ] "mysaPosleMajitelce" ComputedValue 0 "Mýša pošle majitelce"
                , TextFieldData [ 0, 7 ] "mysaCelkem" ComputedValue 0 "Mýša celkem"
                , TextFieldData [ 0, 8 ] "nakladyCelkem" ComputedValue 0 "Náklady celkem"
                ]
    , history =
        DictList.empty
        {-
           DictList.fromList <|
               [ ( 1
                 , HistoryItem 1
                       "2015-02-03"
                       [ ( "mysaMurtaRozdil", 1500 )
                       , ( "internet", 455 )
                       , ( "majitelka", 7500 )
                       , ( "najem", 5000 )
                       ]
                 )
               ]
        -}
    , tempHistoryItem = Nothing
    , mdl = Material.model
    }



-- ACTION, UPDATE


type Msg
    = Recalculate TextFieldData String
    | Mdl (Material.Msg Msg)
    | Save
    | DoSave Date
    | RemoveHistoryItem HistoryItem
    | LoadHistoryItem HistoryItem
    | ReceiveQueryResponse GraphRequests.HistoryResponse
    | ReceiveSaveHistoryItemResponse GraphRequests.SaveHistoryItemResponse
    | ReceiveDeleteHistoryItemResponse GraphRequests.DeleteHistoryItemResponse


setValue : String -> Float -> Model -> Model
setValue name value model =
    let
        newTextFieldsData =
            DictList.update name
                (\data ->
                    case data of
                        Maybe.Just data ->
                            Maybe.Just { data | value = value }

                        Maybe.Nothing ->
                            Maybe.Nothing
                )
                model.textFieldsData
    in
        { model | textFieldsData = newTextFieldsData }


getValue : String -> Model -> Float
getValue name model =
    case DictList.get name model.textFieldsData of
        Maybe.Just textFieldData ->
            textFieldData.value

        Maybe.Nothing ->
            0


recalculateTextFields : Model -> Model
recalculateTextFields model =
    model
        |> setValue "murtaPosleMysovi"
            (((getValue "najem" model) + (getValue "internet" model) + (getValue "majitelka" model) + (getValue "mysaMurtaRozdil" model)) / 2)
        --
        |>
            setValue "mysaPosleZaNet"
                (getValue "internet" model)
        --
        |>
            setValue "mysaPosleMajitelce"
                ((getValue "majitelka" model) + (getValue "najem" model))
        --
        |>
            setValue "mysaCelkem"
                (((getValue "najem" model) + (getValue "internet" model) + (getValue "majitelka" model) - (getValue "mysaMurtaRozdil" model)) / 2)
        --
        |>
            setValue "nakladyCelkem"
                ((getValue "najem" model) + (getValue "internet" model) + (getValue "majitelka" model))



-- getNextItemId : DictList Int a -> Int
-- getNextItemId list =
--     list
--         |> DictList.keys
--         |> List.maximum
--         |> Maybe.withDefault 0
--         |> (+) 1
--         |> Debug.log "ID"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Recalculate textFieldData value ->
            let
                floatValue =
                    String.toFloat value |> Result.withDefault 0

                newModel =
                    model
                        |> setValue textFieldData.name floatValue
                        |> recalculateTextFields
            in
                ( newModel, Cmd.none )

        DoSave date ->
            let
                newHistoryItem =
                    HistoryItem -1
                        (Date.Format.format "%Y-%m-%d" date)
                        [ ( "mysaMurtaRozdil", (getValue "mysaMurtaRozdil" model) )
                        , ( "internet", (getValue "internet" model) )
                        , ( "majitelka", (getValue "majitelka" model) )
                        , ( "najem", (getValue "najem" model) )
                        ]
            in
                ( { model | tempHistoryItem = Just newHistoryItem }, GraphRequests.sendSaveHistoryItemMutation ReceiveSaveHistoryItemResponse newHistoryItem )

        Save ->
            ( model, Task.perform DoSave Date.now )

        LoadHistoryItem historyItem ->
            let
                data =
                    historyItem.data

                newModel =
                    List.foldl (\( key, value ) m -> setValue key value m) model data

                {--
                    foreach ($data as $key => $value) {
                        $model['textFieldsData'][$key]['value'] = $value;
                    }

                    http://stackoverflow.com/questions/9329446/for-each-over-an-array-in-javascript
                --}
            in
                ( newModel |> recalculateTextFields, Cmd.none )

        RemoveHistoryItem historyItem ->
            let
                newHistory =
                    DictList.remove historyItem.id model.history
            in
                ( { model | history = newHistory }, GraphRequests.sendDeleteHistoryItemMutation ReceiveDeleteHistoryItemResponse historyItem )

        ReceiveQueryResponse response ->
            case response of
                Ok data ->
                    let
                        newHistory =
                            GraphRequests.transformResponseData data
                    in
                        ( { model | history = newHistory }, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "RequestError" error
                    in
                        ( model, Cmd.none )

        ReceiveSaveHistoryItemResponse response ->
            case response of
                Ok id ->
                    let
                        _ =
                            Debug.log "SaveRequestOK" id

                        tempHistoryItem =
                            model.tempHistoryItem

                        newHistoryItem =
                            case tempHistoryItem of
                                Just historyItem ->
                                    Just { historyItem | id = id }

                                Nothing ->
                                    Nothing

                        newHistory =
                            case newHistoryItem of
                                Just historyItem ->
                                    DictList.cons historyItem.id historyItem model.history

                                Nothing ->
                                    model.history
                    in
                        ( { model
                            | history = newHistory
                            , tempHistoryItem = Nothing
                          }
                        , Cmd.none
                        )

                Err error ->
                    let
                        _ =
                            Debug.log "SaveRequestError" error
                    in
                        ( model, Cmd.none )

        ReceiveDeleteHistoryItemResponse response ->
            case response of
                Ok data ->
                    let
                        _ =
                            Debug.log "DeleteRequestOK" data
                    in
                        ( model, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "DeleteRequestError" error
                    in
                        ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    div
        [ style [ ( "max-width", "500px" ) ]
        , class "mdl-grid"
        ]
        [ renderColumn 6
            ((model.textFieldsData
                |> DictList.filter (\_ textFieldData -> textFieldData.type_ == InputValue)
                |> DictList.map (\_ textFieldData -> renderTextfield model textFieldData)
                |> DictList.values
             )
                ++ [ Button.render Mdl
                        [ 1, 0 ]
                        model.mdl
                        [ Button.fab
                        , Button.colored
                        , Button.ripple
                        , Options.onClick Save
                        ]
                        [ Icon.i "add" ]
                   ]
            )
        , renderColumn 6
            (model.textFieldsData
                |> DictList.filter (\_ textFieldData -> textFieldData.type_ == ComputedValue)
                |> DictList.map (\_ textFieldData -> renderTextfield model textFieldData)
                |> DictList.values
            )
        , renderColumn 6
            [ renderHistory model ]
        ]
        |> Material.Scheme.top


flippedComparisonBy : (a -> comparable) -> a -> a -> Order
flippedComparisonBy f a b =
    case compare (f a) (f b) of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


renderHistory : Model -> Html Msg
renderHistory model =
    List.ul []
        (model.history
            |> DictList.sortWith (flippedComparisonBy .date)
            |> DictList.map (\_ historyItem -> renderHistoryItem model historyItem)
            |> DictList.values
        )


renderHistoryItem : Model -> HistoryItem -> Html Msg
renderHistoryItem model historyItem =
    let
        loadButton model k =
            Button.render Mdl
                k
                model.mdl
                [ Button.icon
                , Button.accent |> Options.when True
                , Options.onClick (LoadHistoryItem historyItem)
                ]
                [ Icon.i "eject" ]

        removeButton model k =
            Button.render Mdl
                k
                model.mdl
                [ Button.icon
                , Button.accent |> Options.when False
                , Options.onClick (RemoveHistoryItem historyItem)
                ]
                [ Icon.i "delete" ]
    in
        List.li []
            [ loadButton model [ 1, 0, historyItem.id ]
            , List.content [] [ text historyItem.date ]
            , removeButton model [ 1, 1, historyItem.id ]
            ]


renderColumn : Int -> List (Html Msg) -> Html Msg
renderColumn colNum children =
    div
        [ class ("mdl-cell mdl-cell--" ++ (toString colNum) ++ "-col") ]
        children


renderTextfield : Model -> TextFieldData -> Html Msg
renderTextfield model textFieldData =
    Textfield.render Mdl
        textFieldData.mdlId
        model.mdl
        [ Textfield.label textFieldData.label
        , Textfield.floatingLabel
        , Textfield.value (toString textFieldData.value)
        , Textfield.text_
        , Options.onInput (Recalculate textFieldData)
        , Options.attribute <| Html.Attributes.readonly (textFieldData.type_ == ComputedValue)
        , if textFieldData.name == "najem" then
            Textfield.autofocus
          else
            Options.nop
        ]
        []


init : ( Model, Cmd Msg )
init =
    ( recalculateTextFields model, (GraphRequests.sendHistoryQuery ReceiveQueryResponse) )



-- Load Google Mdl CSS. You'll likely want to do that not in code as we
-- do here, but rather in your master .html file. See the documentation
-- for the `Material` module for details.


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
