module Item exposing (Item, decoder, encode)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Shared exposing (..)


type alias Item =
    { id : String
    , name : String
    , img : String
    , description : String
    }



-- JSON


decoder : Decode.Decoder Item
decoder =
    Decode.map4 Item
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "img" Decode.string)
        (Decode.field "description" Decode.string)


encode : Item -> Encode.Value
encode item =
    Encode.object
        [ ( "id", Encode.string item.id )
        , ( "name", Encode.string item.name )
        , ( "img", Encode.string item.img )
        , ( "description", Encode.string item.description )
        ]
