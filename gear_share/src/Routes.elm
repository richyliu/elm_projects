module Routes exposing (Route(..), addPath, itemPath, itemsPath, parseUrl)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = ItemsRoute
    | ItemRoute String
    | AddRoute
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map ItemsRoute top
        , map ItemRoute (s "items" </> string)
        , map ItemsRoute (s "items")
        , map AddRoute (s "add")
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

        AddRoute ->
            "/add"

        NotFoundRoute ->
            "/"


itemsPath : String
itemsPath =
    pathFor ItemsRoute


itemPath : String -> String
itemPath id =
    pathFor (ItemRoute id)


addPath : String
addPath =
    pathFor AddRoute
