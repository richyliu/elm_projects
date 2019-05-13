module Endpoint exposing (Endpoint, items, loginGoogle, loginPassword, request, unwrap, url)

import Http
import Url.Builder exposing (QueryParameter)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , tracker : Maybe String
    , url : Endpoint
    }
    -> Cmd a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , tracker = config.tracker
        , url = unwrap config.url
        }



-- TYPES


{-| Get a URL to the Conduit API.

This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.

-}
type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


root : String
root =
    "http://localhost:8111"


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Endpoint <| Url.Builder.crossOrigin root paths queryParams



-- AUTH ENDPOINTS


authRoot : String
authRoot =
    "https://t485.auth0.com"


clientId : String
clientId =
    "0KDLPs5urmZmeE60gwwB93jrjCG6gjCM"


connectionType : String
connectionType =
    "google-oauth2"


audience : String
audience =
    "https://db-api"


origin : String
origin =
    "http://localhost:8000"


loginGoogle : String
loginGoogle =
    String.join ""
        [ authRoot
        , "/authorize"
        , "?response_type=token"
        , "&client_id=" ++ clientId
        , "&connection=" ++ connectionType
        , "&audience=" ++ audience
        , "&redirect_uri=" ++ origin ++ "/login"
        , "&scope=openid"
        ]


loginPassword : String
loginPassword =
    String.join ""
        [ authRoot
        , "/authorize"
        , "?response_type=token"
        , "&client_id=" ++ clientId
        , "&connection=Username-Password-Authentication"
        , "&audience=" ++ audience
        , "&redirect_uri=" ++ origin ++ "/login"
        ]



-- REST ENDPOINTS


databaseRoot : String
databaseRoot =
    -- "http://localhost:8111"
    "https://gearshare.t485.org"


items : Endpoint
items =
    Endpoint <| databaseRoot ++ "/items"
