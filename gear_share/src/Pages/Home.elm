module Pages.Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import String exposing (fromInt)



-- model


type alias Item =
    { image : String
    , title : String
    }


type alias Model =
    { items : List Item }


init : String -> ( Model, Cmd Msg )
init _ =
    ( { items =
            [ Item "http://via.placeholder.com/200x200" "hi"
            , Item "http://via.placeholder.com/200x200" "another item"
            ]
      }
    , Cmd.none
    )



-- update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "Page"
    , body = [ viewMain model ]
    }


viewMain : Model -> Html Msg
viewMain model =
    div [] <|
        [ h1 [] [ text "Items list" ]
        ]
            ++ List.map
                (\item ->
                    div []
                        [ h2 [] [ text item.title ]
                        , img [ Attr.src item.image ] []
                        ]
                )
                model.items
