module Navbar exposing (Model, Msg, init, subscriptions, update, view)

import Auth
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr exposing (class, style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Icon
import Route exposing (Route)
import Task
import Time exposing (Posix)


type alias Model =
    { navKey : Nav.Key
    , menuOpen : Bool
    , leftSide : Bool
    }


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( { navKey = navKey
      , menuOpen = False
      , leftSide = True
      }
    , Cmd.none
    )


type Msg
    = Back
    | ToggleMenu
    | GoToRoute Route
    | RequestRandomFor (Int -> Route)
    | GoToWithRandom (Int -> Route) Posix
    | Logout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Back ->
            ( model, Nav.back model.navKey 1 )

        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }, Cmd.none )

        GoToRoute route ->
            ( { model | menuOpen = False }, Route.pushUrl model.navKey route )

        -- Used for the seed for shuffle
        RequestRandomFor route ->
            ( model, Task.perform (GoToWithRandom route) Time.now )

        GoToWithRandom route posix ->
            ( { model | menuOpen = False }
            , Route.pushUrl model.navKey <| route <| Time.posixToMillis posix
            )

        Logout ->
            ( model, Cmd.batch [ Auth.logout, Route.pushUrl model.navKey Route.Login ] )


view : Model -> Html Msg
view model =
    lazy viewNavbar model


viewNavbar : Model -> Html Msg
viewNavbar model =
    let
        viewNavButton : String -> String -> Msg -> Html Msg
        viewNavButton classes icon action =
            button
                [ class <| "m-1 w-16 h-16 flex-auto bg-blue-400 active:bg-blue-600 text-white rounded-full text-2xl cursor-pointer select-none hover:shadow-2xl trans " ++ classes
                , onClick action
                ]
                [ span [ style "line-height" "5rem" ] [ Icon.icon icon ] ]

        showMenu : String
        showMenu =
            if model.menuOpen then
                "opacity-100"

            else
                "opacity-0 h-0 w-0 overflow-hidden"

        menuRow : List (Html Msg) -> Html Msg
        menuRow =
            div [ class <| "flex-auto flex flex-row trans trans-slow " ++ showMenu ]
    in
    div
        -- made to dissappear when scrolling down in JS (initBigScroller)
        [ Attr.id "nav-menu"
        , class <|
            "fixed bottom-0 p-1 flex flex-col-reverse trans-slow "
                ++ (if model.leftSide then
                        "left-0"

                    else
                        "right-0"
                   )
        ]
    <|
        [ div [ class "flex-auto flex flex-row" ]
            [ viewNavButton "" "icon-chevron-left" Back
            , viewNavButton "" "icon-bars" ToggleMenu
            , viewNavButton "" "icon-random" <| RequestRandomFor Route.Shuffle
            ]
        , menuRow
            [ viewNavButton "opacity-75" "icon-home" <| GoToRoute Route.Home
            , viewNavButton "opacity-75" "icon-image" <| RequestRandomFor Route.Diy
            , viewNavButton "opacity-75" "icon-female" <| GoToRoute Route.IdolHome
            ]
        , menuRow
            [ viewNavButton "opacity-75" "icon-film" <| RequestRandomFor Route.Videos
            , viewNavButton "opacity-75" "icon-cog" <| GoToRoute Route.Settings
            , viewNavButton "opacity-75" "icon-sign-out" Logout
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
