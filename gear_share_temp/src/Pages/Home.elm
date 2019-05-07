module Pages.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Http
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , data : Status String
    }


type Status a
    = Loading
    | Loaded a
    | Failed


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , data = Loading
      }
    , fetchData ""
    )


view : Model -> Document Msg
view model =
    { title = "Home"
    , body = [ div [] [ text "hello world" ] ]
    }



-- UPDATE


type Msg
    = GotData (Result Http.Error String)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Ok data) ->
            ( { model | data = Loaded data }, Cmd.none )

        GotData (Err error) ->
            ( { model | data = Failed }, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )



-- HTTP


fetchData : String -> Cmd Msg
fetchData _ =
    Http.get
        { url = "https://postman-echo.com/get?foo1=bar1&foo2=bar2"
        , expect = Http.expectString GotData
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
