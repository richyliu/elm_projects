module Id exposing (Id, idDecoder, idParser, idToString, makeId)

import Json.Decode as Decode exposing (Decoder)
import Url.Parser exposing (Parser)


type Id
    = Id String


makeId : String -> Id
makeId =
    Id



-- SERIALIZATION


idDecoder : Decoder Id
idDecoder =
    Decode.map Id Decode.string



-- TRANSFORM


idParser : Parser (Id -> a) a
idParser =
    Url.Parser.custom "id" (\str -> Just (Id str))


idToString : Id -> String
idToString (Id id) =
    id
