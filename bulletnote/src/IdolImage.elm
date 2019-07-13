module IdolImage exposing (IdolImage, highRes, idolImagesDecoder, lowRes)

{-| One idol image, with the high res and low res urls
-}

import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required)


{-| First the low res, then the high res
-}
type IdolImage
    = IdolImage String String



-- SERIALIZATION


idolImageDecoder : Decoder IdolImage
idolImageDecoder =
    Decode.succeed IdolImage
        |> required "high" string
        |> required "low" string


idolImagesDecoder : Decoder (List IdolImage)
idolImagesDecoder =
    Decode.succeed identity
        |> required "idolAlbum" (list idolImageDecoder)



-- TRANSFORM


highRes : IdolImage -> String
highRes (IdolImage high _) =
    high


lowRes : IdolImage -> String
lowRes (IdolImage _ low) =
    low
