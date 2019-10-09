module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class)


type alias Model =
    { name : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "", Cmd.none )


type Msg
    = One


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        One ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "none"
    , body = [ div [] [ text "hello" ] ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
