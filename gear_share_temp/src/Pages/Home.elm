module Pages.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api
import Browser exposing (Document)
import Endpoint
import Html exposing (..)
import Html.Attributes as Attr
import Http
import Item exposing (Item, itemsDecoder)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , items : Status (List Item)
    }


type Status a
    = Loading
    | Loaded a
    | Failed


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , items = Loading
      }
    , fetchData session
    )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        case model.items of
            Loading ->
                text "loading..."

            Loaded items ->
                viewItems items

            Failed ->
                text "(home) failed to load items"
    }


viewItems : List Item -> Html Msg
viewItems items =
    let
        viewItem : Item -> Html Msg
        viewItem item =
            div [ Attr.style "margin" "15px" ]
                [ h2 [] [ text item.name ]
                , div [] [ img [ Attr.src item.img ] [] ]
                , span [] [ text <| "Owner: " ++ item.owner ]
                , p [] [ text item.description ]
                ]
    in
    div [] <| List.map viewItem items



-- UPDATE


type Msg
    = GotItems (Result Http.Error (List Item))
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotItems (Ok items) ->
            ( { model | items = Loaded items }, Cmd.none )

        GotItems (Err error) ->
            ( { model | items = Failed }, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )



-- HTTP


fetchData : Session -> Cmd Msg
fetchData session =
    Api.get
        Endpoint.items
        (Session.cred session)
        (Http.expectJson GotItems itemsDecoder)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
