module Item exposing (Item, itemDecoder, itemsDecoder)

import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode


type alias Item =
    { name : String
    , owner : String
    , img : String
    , description : String
    }



-- SERIALIZATION


itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "name" string
        |> required "owner" string
        |> required "img" string
        |> required "description" string


itemsDecoder : Decoder (List Item)
itemsDecoder =
    Decode.succeed identity
        |> required "items" (list itemDecoder)
