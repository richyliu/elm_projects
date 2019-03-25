module Main exposing (main)

import Browser
import Html exposing (..)



-- main


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- model


type alias Player =
    String


type alias Model =
    { text : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "hello", Cmd.none )



-- update


type Msg
    = SetText String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetText text ->
            ( { model | text = text }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- view


view : Model -> Html Msg
view model =
    p [] [ text model.text ]
