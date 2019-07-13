module Video exposing (Video, videoToString, videosDecoder)

import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required)


type Video
    = Video String



-- SERIALIZATION


videoDecoder : Decoder Video
videoDecoder =
    Decode.map Video string


videosDecoder : Decoder (List Video)
videosDecoder =
    Decode.succeed identity
        |> required "videos" (list videoDecoder)



-- TRANSFORM


videoToString : Video -> String
videoToString (Video video) =
    video
