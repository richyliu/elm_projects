module Pages.Edit exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (class, href, value)
import Html.Events exposing (onClick)
import Http
import Item exposing (Item)
import Routes exposing (itemsPath)
import Shared exposing (..)


type alias Model =
    { item : RemoteData Item
    }


type Msg
    = OnFetchItem (Result Http.Error Item)
    | ChangeDescription Item String
    | OnItemSave (Result Http.Error Item)


init : Flags -> String -> ( Model, Cmd Msg )
init flags itemId =
    ( { item = Loading }, fetchItem flags itemId )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Flags -> Msg -> Model -> ( Model, Cmd Msg )
update flags msg model =
    case msg of
        OnFetchItem (Ok item) ->
            ( { model | item = Loaded item }, Cmd.none )

        OnFetchItem (Err err) ->
            ( { model | item = Failure }, Cmd.none )

        ChangeDescription item newDescription ->
            let
                updatedItem =
                    { item | description = item.description ++ newDescription }
            in
            ( model, saveItem flags updatedItem )

        OnItemSave (Ok item) ->
            ( { model | item = Loaded item }, Cmd.none )

        OnItemSave (Err error) ->
            ( model, Cmd.none )



-- DATA


fetchItem : Flags -> String -> Cmd Msg
fetchItem flags itemId =
    Http.get
        { url = saveItemUrl flags itemId
        , expect = Http.expectJson OnFetchItem Item.decoder
        }


saveItem : Flags -> Item -> Cmd Msg
saveItem flags item =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = saveItemUrl flags item.id
        , body = Item.encode item |> Http.jsonBody
        , expect = Http.expectJson OnItemSave Item.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


saveItemUrl : Flags -> String -> String
saveItemUrl flags itemId =
    flags.api ++ "/items/" ++ itemId



-- VIEWS


view : Model -> Html Msg
view model =
    let
        content =
            case model.item of
                NotAsked ->
                    text ""

                Loading ->
                    text "Loading ..."

                Loaded item ->
                    viewWithData item

                Failure ->
                    text "Error"
    in
    section [ class "p-4" ]
        [ content ]


viewWithData : Item -> Html.Html Msg
viewWithData item =
    div []
        [ h1 [] [ text item.name ]
        , inputDescription item
        ]


inputDescription : Item -> Html.Html Msg
inputDescription item =
    div
        [ class "flex items-end py-2" ]
        [ label [ class "mr-3" ] [ text "Level" ]
        , div [ class "" ]
            [ h3 [ class "bold text-2xl" ] [ text "Description" ]
            , textarea [] [ text item.description ]
            ]
        ]


listBtn : Html Msg
listBtn =
    a
        [ class "btn regular"
        , href itemsPath
        ]
        [ i [ class "fa fa-chevron-left mr-1" ] [], text "List" ]
