module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Api exposing (Cred)
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- ROUTING


type Route
    = Home
    | Root
    | Login (Maybe Cred)
    | Logout


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map (Login Nothing) (s "login")
        , Parser.map Logout (s "logout")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    let
        accessToken =
            case url.fragment of
                Just hash ->
                    parseToken hash

                Nothing ->
                    ""

        parsed =
            Parser.parse parser url
    in
    Maybe.andThen
        (\p ->
            case p of
                Login _ ->
                    if String.length accessToken > 0 then
                        -- TODO: get username from the token
                        Just <| Login <| Just <| Api.Cred "defualt_username" accessToken

                    else
                        Just <| Login Nothing

                a ->
                    Just a
        )
        parsed


{-| Parse the access token from a hash
-}
parseToken : String -> String
parseToken hash =
    let
        accToken =
            "access_token="

        accessToken =
            hash
                |> String.indices "&"
                |> List.head
                |> Maybe.andThen (\i -> Just <| String.slice (String.length accToken) i hash)
    in
    case accessToken of
        Just token ->
            token

        Nothing ->
            ""



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Root ->
                    []

                Login _ ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]
    in
    "/" ++ String.join "/" pieces
