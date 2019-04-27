module Routes exposing (Route(..), itemPath, itemsPath, parseUrl)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = ItemsRoute
    | ItemRoute String
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map ItemsRoute top
        , map ItemRoute (s "items" </> string)
        , map ItemsRoute (s "items")
        ]


parseUrl : Url -> Route
parseUrl url =
    case parse matchers url of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


pathFor : Route -> String
pathFor route =
    case route of
        ItemsRoute ->
            "/items"

        ItemRoute id ->
            "/items/" ++ id

        NotFoundRoute ->
            "/"


itemsPath =
    pathFor ItemsRoute


itemPath id =
    pathFor (ItemRoute id)
