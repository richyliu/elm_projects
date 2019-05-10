module Pages.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api
import Browser exposing (Document)
import Endpoint
import Html exposing (Html, div, text)
import Html.Attributes as Attr
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
    , fetchData session
    )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        Html.pre [ Attr.style "margin" "10px" ]
            [ text
                (case model.data of
                    Loading ->
                        "loading..."

                    Loaded data ->
                        "data: " ++ data

                    Failed ->
                        "(home) failed to load data"
                )
            ]
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


fetchData : Session -> Cmd Msg
fetchData session =
    Api.get
        Endpoint.items
        (Session.cred session)
        (Http.expectString GotData)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
