module Album exposing (Album, albumParser, albumToString, albumsDecoder, first)

import Json.Decode as Decode exposing (Decoder, int, list)
import Json.Decode.Pipeline exposing (required)
import Url.Parser exposing (Parser)


type Album
    = Album String


first : Album
first =
    Album "1"



-- SERIALIZATION


albumDecoder : Decoder Album
albumDecoder =
    Decode.succeed Album
        |> required "album" (Decode.map String.fromInt int)


albumsDecoder : Decoder (List Album)
albumsDecoder =
    let
        -- sort the albums in number order
        sortList =
            List.sortBy
                ((\(Album album) -> album)
                    >> String.toInt
                    >> Maybe.withDefault 0
                )
    in
    Decode.succeed sortList
        |> required "model" (list albumDecoder)



-- TRANSFORM


albumParser : Parser (Album -> a) a
albumParser =
    Url.Parser.custom "album" (\str -> Just (Album str))


albumToString : Album -> String
albumToString (Album album) =
    album
