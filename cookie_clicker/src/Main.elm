module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Task
import Time



-- Main method


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias CookieGenerator =
    -- num cookies generated per tick
    { cookies : Int

    -- every interval ms generate cookies
    , interval : Int

    -- starting time
    , start : Int
    }


type alias Model =
    { cookies : Int
    , generators : List CookieGenerator
    , time : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cookies = 6
      , generators = []
      , time = 0
      }
    , Cmd.none
    )



-- Update


type Msg
    = AddCookie
    | Tick Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddCookie ->
            ( { model | cookies = model.cookies + 1 }, Cmd.none )

        Tick time ->
            ( { model | time = time }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddCookie ] [ text "Big cookies arst" ]
        , p []
            [ text <|
                "You have: "
                    ++ String.fromInt model.cookies
                    ++ " cookies"
            ]
        , p [] [ text <| "time: " ++ String.fromInt model.time ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Time.every 100 (Time.posixToMillis >> Tick)
    Sub.none
