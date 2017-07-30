module GraphRequests exposing (..)

import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Client.Http as GraphQLClient
import Task exposing (Task)
import CommonTypes exposing (..)
import DictList exposing (..)


url : String
url =
    -- "http://localhost:4000"
    -- "https://api.graphcms.com/simple/v1/cj1pegshah4dy0170"
    "https://payments-be.example.com/"


inspectRequest : String -> Request operationType result -> Request operationType result
inspectRequest title request =
    let
        _ =
            Debug.log title (requestBody request)
    in
        request


sendQueryRequest : Request Query a -> Task GraphQLClient.Error a
sendQueryRequest request =
    let
        _ =
            inspectRequest "QueryDocument" request
    in
        GraphQLClient.sendQuery url request


sendMutationRequest : Request Mutation a -> Task GraphQLClient.Error a
sendMutationRequest request =
    let
        _ =
            inspectRequest "QueryDocument" request
    in
        GraphQLClient.sendMutation url request



------ GET HISTORY ------
{-
     {
       historyItems {
         id
         date
         historyItemFields {
           name
           value
         }
       }
     }


   {
    "data": {
      "historyItems": [
        {
          "id": 1,
          "historyItemFields": [
            {
              "value": 1234,
              "name": "mysaMurtaRozdil"
            },
            {
              "value": 563,
              "name": "internet"
            },
            {
              "value": 7563,
              "name": "majitelka"
            },
            {
              "value": 6200,
              "name": "najem"
            }
          ],
          "date": "2015-02-03"
        }
      ]
     }
    }
-}


historyRequest : Request Query (List HistoryItem)
historyRequest =
    let
        historyItemField =
            object (,)
                |> with (field "name" [] string)
                |> with (field "value" [] float)

        historyItem =
            object HistoryItem
                |> with (field "id" [] int)
                |> with (field "date" [] string)
                |> with (field "historyItemFields" [] (list historyItemField))

        queryRoot =
            (field "historyItems" [] (list historyItem))
    in
        extract queryRoot
            |> queryDocument
            |> request {}


transformResponseData : List HistoryItem -> History
transformResponseData list =
    let
        historyItemsList =
            List.map (\historyItem -> ( historyItem.id, historyItem )) list

        newHistory =
            DictList.fromList historyItemsList
    in
        newHistory


type alias HistoryResponse =
    Result GraphQLClient.Error (List HistoryItem)


sendHistoryQuery : (HistoryResponse -> msg) -> Cmd msg
sendHistoryQuery msg =
    sendQueryRequest historyRequest
        |> Task.attempt msg



------ //GET HISTORY -------
------ SET HISTORY ITEM ----
{-
   mutation ($date: String!, $historyItemFields: [HistoryItemFieldInput!]!)
   {
     createHistoryItem(date: $date, historyItemFields: $historyItemFields)
     {
       id
     }
   }


     {
       "date":"2017-04-23",
       "historyItemFields":[
           {"name":"mysaMurtaRozdil","value":2000},
           {"name":"internet","value":499},
           {"name":"majitelka","value":8000},
           {"name":"najem","value":6483}]
       }


     {
       "data": {
           "createHistoryItem": {
           "id": 15
           }
       }
     }
-}


saveHistoryItemRequest : HistoryItem -> Request Mutation Int
saveHistoryItemRequest historyItem =
    let
        -- historyItemExample =
        --     HistoryItem
        --         "2015-02-03"
        --         [ ( "mysaMurtaRozdil", 1500 )
        --         , ( "internet", 455 )
        --         , ( "majitelka", 7500 )
        --         , ( "najem", 5000 )
        --         ]
        historyItemField =
            Var.object "HistoryItemFieldInput"
                [ Var.field "name" Tuple.first Var.string
                , Var.field "value" Tuple.second Var.float
                ]

        dateVar =
            Var.required "date" .date Var.string

        fieldsList =
            Var.required "historyItemFields" .data (Var.list historyItemField)
    in
        extract
            (field "createHistoryItem"
                [ ( "date", Arg.variable dateVar )
                , ( "historyItemFields", Arg.variable fieldsList )
                ]
                (extract (field "id" [] int))
            )
            |> mutationDocument
            |> request historyItem


type alias SaveHistoryItemResponse =
    Result GraphQLClient.Error Int


sendSaveHistoryItemMutation : (SaveHistoryItemResponse -> msg) -> HistoryItem -> Cmd msg
sendSaveHistoryItemMutation msg historyItem =
    sendMutationRequest (saveHistoryItemRequest historyItem)
        |> Task.attempt msg



------ //SET HISTORY ITEM ----
------ DELETE HISTORY ITEM ----
{-
   mutation ($id: Int!)
    {
        deleteHistoryItem(id: $id)
        {
            id
        }
    }


    {
        "id":8
    }


    {
        "data": {
            "deleteHistoryItem": {
            "id": 11
            }
        }
    }
-}


deleteHistoryItemRequest : HistoryItem -> Request Mutation Int
deleteHistoryItemRequest historyItem =
    let
        idVar =
            Var.required "id" .id Var.int
    in
        extract
            (field "deleteHistoryItem"
                [ ( "id", Arg.variable idVar )
                ]
                (extract (field "id" [] int))
            )
            |> mutationDocument
            |> request historyItem


type alias DeleteHistoryItemResponse =
    Result GraphQLClient.Error Int


sendDeleteHistoryItemMutation : (DeleteHistoryItemResponse -> msg) -> HistoryItem -> Cmd msg
sendDeleteHistoryItemMutation msg historyItem =
    sendMutationRequest (deleteHistoryItemRequest historyItem)
        |> Task.attempt msg



------ //DELETE HISTORY ITEM ----
