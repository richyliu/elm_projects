module Pages.Add exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onClick, onInput)
import Http
import Item exposing (Item, ItemNoId, blankItemNoId, encodeNoId)
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
    , errorFields : List String
    }


type Msg
    = ChangeItem ItemNoId
    | AddItem
    | OnItemAdded (Result Http.Error Item)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { item = blankItemNoId
      , addedItem = Initial
      , errorFields = []
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
            ( { model
                | item = item
                , errorFields =
                    if List.length model.errorFields > 0 then
                        getErrorFields model.item

                    else
                        []
              }
            , Cmd.none
            )

        AddItem ->
            if List.length (getErrorFields model.item) == 0 then
                ( model, addItem flags model.item )

            else
                ( { model | errorFields = getErrorFields model.item }, Cmd.none )

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


{-| Get the erroneous fields (fields that are blank)
-}
getErrorFields : ItemNoId -> List String
getErrorFields item =
    List.filter (String.length >> (/=) 0)
        [ if String.length item.name == 0 then
            "name"

          else
            ""
        , if String.length item.owner == 0 then
            "owner"

          else
            ""
        , if String.length item.description == 0 then
            "description"

          else
            ""
        ]



-- VIEWS


view : Model -> Html Msg
view model =
    section [ class "p-4" ]
        [ case model.addedItem of
            Success item ->
                viewSuccess model

            Error str ->
                text <| "Error: " ++ str

            Waiting ->
                text "Loading..."

            Initial ->
                viewAddItem model.item model.errorFields
        ]


viewAddItem : ItemNoId -> List String -> Html Msg
viewAddItem item errorFields =
    let
        inputClasses : String
        inputClasses =
            "shadow appearance-none border rounded w-full py-2 px-3 text-grey-darker leading-tight focus:outline-none focus:shadow-outline"

        addBorderIfError : String -> String
        addBorderIfError field =
            inputClasses
                ++ (if List.member field errorFields then
                        "border border-red"

                    else
                        ""
                   )
    in
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
                    [ class <| addBorderIfError "name"
                    , Attr.id "name"
                    , Attr.type_ "text"
                    , Attr.placeholder "My gear item"
                    , onInput (\name -> ChangeItem { item | name = name })
                    ]
                    [ text item.name ]
                ]
            , div [ class "mb-4" ]
                [ label
                    [ class "block text-grey-darker text-sm font-bold mb-2"
                    , Attr.for "owner"
                    ]
                    [ text "Owner" ]
                , input
                    [ class <| addBorderIfError "owner"
                    , Attr.id "owner"
                    , Attr.type_ "text"
                    , Attr.placeholder "Rick Astley"
                    , onInput (\owner -> ChangeItem { item | owner = owner })
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
                    [ class <| addBorderIfError "description"
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


viewSuccess : Model -> Html Msg
viewSuccess model =
    div [] []
