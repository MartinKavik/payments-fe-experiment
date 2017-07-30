module CommonTypes exposing (..)

import DictList exposing (..)


type alias HistoryItem =
    { id : Int
    , date : String
    , data : List ( String, Float )
    }


type alias History =
    DictList Int HistoryItem
