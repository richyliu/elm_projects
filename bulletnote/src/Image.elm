module Image exposing (Image, diyImagesDecoder, first, imageParser, imageQueryParser, imageToString, imagesDecoder, lazyLoadImage)

import Html exposing (Html, img)
import Html.Attributes exposing (attribute, class, style)
import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required)
import Url.Parser exposing (Parser)
import Url.Parser.Query as Query


type Image
    = Image String


first : Image
first =
    Image "000.jpg"


lazyLoadImage : String -> Html msg
lazyLoadImage image =
    img
        [ class "lazy-load w-full"
        , style "min-height" "500px"
        , attribute "data-src" image
        ]
        []



-- SERIALIZATION


imageDecoder : Decoder Image
imageDecoder =
    Decode.succeed Image
        |> required "image" string


imagesDecoder : Decoder (List Image)
imagesDecoder =
    Decode.succeed identity
        |> required "album" (list imageDecoder)


diyImagesDecoder : Decoder (List Image)
diyImagesDecoder =
    Decode.succeed identity
        |> required "diy" (list <| Decode.map Image string)



-- TRANSFORM


imageParser : Parser (Image -> a) a
imageParser =
    Url.Parser.custom "image" (\str -> Just (Image str))


imageQueryParser : Query.Parser (Maybe Image)
imageQueryParser =
    Query.custom "image" (List.head >> Maybe.andThen (Image >> Just))


imageToString : Image -> String
imageToString (Image image) =
    image
