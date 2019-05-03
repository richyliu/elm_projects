module Item exposing (Item, ItemNoId, blankItem, blankItemNoId, decoder, encode, encodeNoId)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Shared exposing (..)


type alias Item =
    { id : String
    , name : String
    , owner : String
    , img : String
    , description : String
    }


type alias ItemNoId =
    { name : String
    , owner : String
    , img : String
    , description : String
    }


blankItem : Item
blankItem =
    Item "" "" "" "" ""


blankItemNoId : ItemNoId
blankItemNoId =
    ItemNoId "" "" "" ""



-- JSON


decoder : Decode.Decoder Item
decoder =
    Decode.map5 Item
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "owner" Decode.string)
        (Decode.field "img" Decode.string)
        (Decode.field "description" Decode.string)


encode : Item -> Encode.Value
encode item =
    Encode.object
        [ ( "id", Encode.string item.id )
        , ( "name", Encode.string item.name )
        , ( "owner", Encode.string item.name )
        , ( "img", Encode.string item.img )
        , ( "description", Encode.string item.description )
        ]


encodeNoId : ItemNoId -> Encode.Value
encodeNoId item =
    Encode.object
        [ ( "name", Encode.string item.name )
        , ( "owner", Encode.string item.name )
        , ( "img", Encode.string item.img )
        , ( "description", Encode.string item.description )
        ]
