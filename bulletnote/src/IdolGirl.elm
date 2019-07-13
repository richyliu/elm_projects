module IdolGirl exposing (IdolGirl, createGirl, idolGirlParser, idolGirlToString, idolGirlToUrlString, idolGirlsDecoder)

{-| An idol girl model with the idol id
-}

import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required)
import Url exposing (percentDecode, percentEncode)
import Url.Parser exposing (Parser)


{-| Girl has just the idol ID
-}
type IdolGirl
    = IdolGirl String


createGirl : String -> IdolGirl
createGirl =
    IdolGirl



-- SERIALIZATION


idolGirlsDecoder : Decoder (List IdolGirl)
idolGirlsDecoder =
    Decode.succeed identity
        |> required "idolModels" (list (Decode.map IdolGirl string))



-- TRANSFORM


idolGirlParser : Parser (IdolGirl -> a) a
idolGirlParser =
    Url.Parser.custom "idolgirl" (percentDecode >> Maybe.map IdolGirl)


idolGirlToString : IdolGirl -> String
idolGirlToString (IdolGirl idolGirl) =
    idolGirl


idolGirlToUrlString : IdolGirl -> String
idolGirlToUrlString (IdolGirl id) =
    percentEncode id
