module Pages.Login exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred)
import Browser exposing (Document)
import Endpoint exposing (loginGoogle, loginPassword)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , cred : Maybe Cred
    }


init : Session -> Maybe Cred -> ( Model, Cmd Msg )
init session cred =
    ( { session = session
      , cred = cred
      }
    , case cred of
        Just c ->
            Viewer.store <| Viewer.Viewer c

        Nothing ->
            Cmd.none
    )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        div [ class "cred-page" ]
            [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
            , p [] [ a [ href loginGoogle ] [ text "Sign in with google" ] ]
            , p [] [ a [ href loginPassword ] [ text "Sign in with email and password" ] ]
            ]
    }



-- UPDATE


type Msg
    = GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
