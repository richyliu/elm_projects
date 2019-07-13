module IdolAlbum exposing (IdolAlbum, first, idolAlbumCover, idolAlbumId, idolAlbumParser, idolAlbumToUrlString, idolAlbumsDecoder, removeOrigin)

{-| An idol album with cover image and album id
-}

import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required)
import Url exposing (percentDecode, percentEncode)
import Url.Parser exposing (Parser)


{-| Album has the idol ID and the cover image
-}
type IdolAlbum
    = IdolAlbum String String


{-| First idol album
-}
first : Int
first =
    1



-- SERIALIZATION


idolAlbumDecoder : Decoder IdolAlbum
idolAlbumDecoder =
    Decode.succeed IdolAlbum
        |> required "id" string
        |> required "cover" string


idolAlbumsDecoder : Decoder (List IdolAlbum)
idolAlbumsDecoder =
    Decode.succeed identity
        |> required "idolModel" (list idolAlbumDecoder)



-- TRANSFORM


removeOrigin : IdolAlbum -> String
removeOrigin =
    idolAlbumId >> String.slice 19 -1


idolAlbumParser : Parser (IdolAlbum -> a) a
idolAlbumParser =
    Url.Parser.custom
        "idolalbum"
        (percentDecode >> Maybe.map (\id -> IdolAlbum id ""))


idolAlbumId : IdolAlbum -> String
idolAlbumId (IdolAlbum id _) =
    id


idolAlbumCover : IdolAlbum -> String
idolAlbumCover (IdolAlbum _ cover) =
    cover


idolAlbumToUrlString : IdolAlbum -> String
idolAlbumToUrlString (IdolAlbum id _) =
    percentEncode id
