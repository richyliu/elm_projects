module Api exposing (baseGet, buildUrl, wrapperDecoder)

import Api.Endpoints exposing (graphqlRoot)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Url



-- HTTP


baseGet : String -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
baseGet partialUrl partialDecoder toMsg =
    Http.get
        { url = buildUrl partialUrl
        , expect = Http.expectJson toMsg (wrapperDecoder partialDecoder)
        }



-- DECODERS


wrapperDecoder : Decoder a -> Decoder a
wrapperDecoder decoder =
    Decode.succeed identity
        |> required "data" decoder



-- PRIVATE


buildUrl : String -> String
buildUrl query =
    graphqlRoot ++ Url.percentEncode query
