module Girl exposing (Girl, girlDecoder, girlsDecoder, makeGirl)

{- This isn't called a `model` like elsewhere (i.e. the node backend code) in
   order to prevent confusion with Elm's model
-}

import Id exposing (Id)
import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Girl =
    { name : String
    , id : Id
    }


makeGirl : String -> String -> Girl
makeGirl id name =
    { id = Id.makeId id
    , name = name
    }



-- SERIALIZATION


girlBaseDecoder : Decoder Girl
girlBaseDecoder =
    Decode.succeed Girl
        |> required "name" string
        |> required "id" Id.idDecoder


girlDecoder : Decoder Girl
girlDecoder =
    Decode.succeed identity
        |> required "modelInfo" girlBaseDecoder


girlsDecoder : Decoder (List Girl)
girlsDecoder =
    Decode.succeed identity
        |> required "models" (list girlBaseDecoder)
