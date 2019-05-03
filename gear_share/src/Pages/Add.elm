module Pages.Add exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onClick, onInput)
import Http
import Item exposing (Item, ItemNoId, encodeNoId)
import Routes exposing (itemsPath)
import Shared exposing (..)


type AddedState
    = Success Item
    | Error String
    | Waiting
    | Initial


type alias Model =
    { item : ItemNoId
    , addedItem : AddedState
    }


type Msg
    = ChangeItem ItemNoId
    | AddItem
    | OnItemAdded (Result Http.Error Item)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { item = ItemNoId "" "" ""
      , addedItem = Initial
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Flags -> Msg -> Model -> ( Model, Cmd Msg )
update flags msg model =
    case msg of
        ChangeItem item ->
            ( { model | item = item }, Cmd.none )

        AddItem ->
            ( model, addItem flags model.item )

        OnItemAdded itemResult ->
            ( { model
                | addedItem =
                    case itemResult of
                        Ok item ->
                            Success item

                        Err _ ->
                            Error "failed"
              }
            , Cmd.none
            )



-- DATA


addItem : Flags -> ItemNoId -> Cmd Msg
addItem flags item =
    Http.request
        { method = "POST"
        , headers = []
        , url = saveItemUrl flags
        , body = encodeNoId item |> Http.jsonBody
        , expect = Http.expectJson OnItemAdded Item.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


saveItemUrl : Flags -> String
saveItemUrl flags =
    flags.api ++ "/items/"



-- VIEWS


view : Model -> Html Msg
view model =
    section [ class "p-4" ]
        [ case model.addedItem of
            Success item ->
                text "success"

            Error str ->
                text <| "Error: " ++ str

            Waiting ->
                text "Loading..."

            Initial ->
                viewAddItem model.item
        ]


viewAddItem : ItemNoId -> Html Msg
viewAddItem item =
    div [ class "w-full max-w-xs" ]
        [ form
            [ class "bg-white shadow-md px-8 pt-6 pb-6 mb-4"
            ]
            [ div [ class "mb-4" ]
                [ label
                    [ class "block text-grey-darker text-sm font-bold mb-2"
                    , Attr.for "name"
                    ]
                    [ text "Name" ]
                , input
                    [ class "shadow appearance-none border rounded w-full py-2 px-3 text-grey-darker leading-tight focus:outline-none focus:shadow-outline"
                    , Attr.id "name"
                    , Attr.type_ "text"
                    , Attr.placeholder "My gear item"
                    , onInput (\name -> ChangeItem { item | name = name })
                    ]
                    [ text item.name ]
                ]
            , div [ class "mb-6" ]
                [ label
                    [ class "block text-grey-darker text-sm font-bold mb-2"
                    , Attr.for "description"
                    ]
                    [ text "Description" ]
                , textarea
                    [ class "shadow appearance-none border rounded w-full py-2 px-3 text-grey-darker leading-tight focus:outline-none focus:shadow-outline"
                    , Attr.id "description"
                    , Attr.placeholder "Item description..."
                    , onInput (\desc -> ChangeItem { item | description = desc })
                    ]
                    [ text item.description ]
                ]
            , div [ class "flex items-center justify-between" ]
                [ button
                    [ class "bg-blue hover:bg-blue-dark text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline"
                    , onClick AddItem
                    , Attr.type_ "button"
                    ]
                    [ text "Add" ]
                ]
            ]
        ]


inputDescription : ItemNoId -> Html Msg
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
        , Attr.href itemsPath
        ]
        [ i [ class "fa fa-chevron-left mr-1" ] [], text "List" ]
