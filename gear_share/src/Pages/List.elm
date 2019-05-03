module Pages.List exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, href, src)
import Http
import Item exposing (Item)
import Json.Decode as Decode
import Routes
import Shared exposing (..)


type alias Model =
    { items : RemoteData (List Item)
    }


type Msg
    = OnFetchItems (Result Http.Error (List Item))


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { items = Loading }, fetchItems flags )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnFetchItems (Ok items) ->
            ( { model | items = Loaded items }, Cmd.none )

        OnFetchItems (Err err) ->
            ( { model | items = Failure }, Cmd.none )



-- DATA


fetchItems : Flags -> Cmd Msg
fetchItems flags =
    Http.get
        { url = flags.api ++ "/items"
        , expect = Http.expectJson OnFetchItems (Decode.list Item.decoder)
        }



-- VIEWS


view : Model -> Html Msg
view model =
    let
        content =
            case model.items of
                NotAsked ->
                    text ""

                Loading ->
                    text "Loading ..."

                Loaded items ->
                    viewWithData items

                Failure ->
                    text "Error"
    in
    section [ class "p-4" ]
        [ content ]


viewWithData : List Item -> Html Msg
viewWithData items =
    table []
        [ thead []
            [ tr []
                [ th [ class "p-2" ] [ text "Img" ]
                , th [ class "p-2" ] [ text "Name" ]
                , th [ class "p-2" ] [ text "Owner" ]
                , th [ class "p-2" ] [ text "Description" ]
                , th [ class "p-2" ] [ text "Actions" ]
                ]
            ]
        , tbody [] (List.map itemRow items)
        ]


itemRow : Item -> Html Msg
itemRow item =
    tr []
        [ td [ class "p-2" ]
            [ img
                [ src
                    (if item.img == "" then
                        "https://via.placeholder.com/100x100.png?text=No%20Image"

                     else
                        item.img
                    )
                , Attr.width 100
                ]
                []
            ]
        , td [ class "p-2" ] [ text item.name ]
        , td [ class "p-2" ] [ text item.owner ]
        , td [ class "p-2" ] [ text item.description ]
        , td [ class "p-2" ]
            [ editBtn item ]
        ]


editBtn : Item -> Html.Html Msg
editBtn item =
    let
        path =
            Routes.itemPath item.id
    in
    a
        [ class "btn regular"
        , href path
        ]
        [ i [ class "fa fa-edit mr-1" ] [], text "Edit" ]
