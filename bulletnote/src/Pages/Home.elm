module Pages.Home exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toNavKey
    , update
    , view
    )

{-| Home TODO description
-}

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onClick, onInput)
import Route
import Task
import Time exposing (Posix)
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , rootItem : Item
    , showControls : Bool
    }


type alias Item =
    { id : Id
    , content :
        { name : String
        , time : Maybe String
        }
    , editing :
        { name : Bool
        , time : Bool
        }
    , children : ItemChildren
    }


type Id
    = Id String


type ItemChildren
    = ItemChildren (List Item)


createItem : String -> List Item -> Item
createItem name children =
    { id = Id name
    , content = { name = name, time = Just "2018-03-02T17:30" }
    , editing = { name = False, time = False }
    , children = ItemChildren children
    }


createItemWithId : Id -> String -> List Item -> Item
createItemWithId id name children =
    { id = id
    , content = { name = name, time = Nothing }
    , editing = { name = False, time = False }
    , children = ItemChildren children
    }


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( { navKey = navKey
      , rootItem =
            createItem "todo"
                [ createItem "eat pie 1" []
                , createItem "wash face"
                    [ createItem "wash ears" []
                    , createItem "wash nose" []
                    ]
                , createItem "eat pie" []
                ]
      , showControls = False
      }
    , Cmd.none
    )


view : Model -> PageContent Msg
view model =
    { title = "Home"
    , content =
        div []
            [ viewControls model
            , viewItem model model.rootItem
            ]
    }


viewControls : Model -> Html Msg
viewControls model =
    div []
        [ button
            [ class "border border-black rounded px-1 m-2"
            , onClick ToggleControls
            ]
            [ text
                (if model.showControls then
                    "controls on"

                 else
                    "controls off"
                )
            ]
        , button
            [ class "border border-black rounded px-1 m-2"
            , onClick NoOp
            ]
            [ text "serialize" ]
        ]


viewItem : Model -> Item -> Html Msg
viewItem model rootItem =
    let
        renderItem : Item -> List (Html Msg) -> Html Msg
        renderItem item children =
            div [ class "pl-4" ] <|
                [ div [] <|
                    [ if item.editing.name then
                        input
                            [ class "p-2"
                            , Attr.value item.content.name
                            , onInput (EditItemName item)
                            ]
                            []

                      else
                        span [ class "p-2" ] [ text item.content.name ]
                    , case item.content.time of
                        Just time ->
                            input
                                [ class "p-2"
                                , Attr.type_ "datetime-local"
                                , Attr.value time
                                , onInput (EditItemTime item)
                                ]
                                []

                        Nothing ->
                            button
                                [ class "border border-black rounded px-1 mx-1"

                                -- TODO: change default time and add ability to "remove" time
                                , onClick (EditItemTime item "2019-04-02T02:30")
                                ]
                                [ text "add time" ]
                    ]
                        ++ (if model.showControls then
                                itemControls item

                            else
                                []
                           )
                        ++ children
                ]

        itemControls : Item -> List (Html Msg)
        itemControls item =
            [ button
                [ class "border border-black rounded px-1 mx-1"
                , onClick <| ToggleEditName item
                ]
                [ text
                    (if item.editing.name then
                        "done"

                     else
                        "edit"
                    )
                ]
            , button
                [ class "border border-black rounded px-1"
                , onClick <| AddItem item
                ]
                [ text "+" ]
            , button
                [ class "border border-black rounded px-1"
                , onClick <| RemoveItem item
                ]
                [ text "del" ]
            ]
    in
    mapItems renderItem rootItem



-- UPDATE


type Msg
    = EditItemName Item String
    | EditItemTime Item String
    | ToggleEditName Item
    | AddItem Item
    | AddItemWithTime Item Posix
    | RemoveItem Item
    | ToggleControls
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditItemName editItem str ->
            ( { model
                | rootItem =
                    mapItemsToItems
                        (\item ->
                            { item
                                | content =
                                    let
                                        itemContent =
                                            item.content
                                    in
                                    { itemContent
                                        | name =
                                            if item.id == editItem.id then
                                                str

                                            else
                                                item.content.name
                                    }
                            }
                        )
                        model.rootItem
              }
            , Cmd.none
            )

        EditItemTime editItem time ->
            ( { model
                | rootItem =
                    mapItemsToItems
                        (\item ->
                            { item
                                | content =
                                    let
                                        itemContent =
                                            item.content
                                    in
                                    { itemContent
                                        | time =
                                            if item.id == editItem.id then
                                                Just time

                                            else
                                                item.content.time
                                    }
                            }
                        )
                        model.rootItem
              }
            , Cmd.none
            )

        ToggleEditName editItem ->
            ( { model
                | rootItem =
                    mapItemsToItems
                        (\item ->
                            { item
                                | editing =
                                    let
                                        itemEditing =
                                            item.editing
                                    in
                                    { itemEditing
                                        | name =
                                            if item.id == editItem.id then
                                                not item.editing.name

                                            else
                                                item.editing.name
                                    }
                            }
                        )
                        model.rootItem
              }
            , Cmd.none
            )

        AddItem root ->
            ( model, Task.perform (AddItemWithTime root) Time.now )

        AddItemWithTime root now ->
            let
                newId =
                    now
                        |> Time.posixToMillis
                        |> String.fromInt
                        |> Id

                newItem =
                    createItemWithId newId "Untitled" []
            in
            ( { model
                | rootItem =
                    mapItemsToItems
                        (\item ->
                            let
                                (ItemChildren newChildren) =
                                    item.children
                            in
                            { item
                                | children =
                                    ItemChildren <|
                                        newChildren
                                            ++ (if item == root then
                                                    [ newItem ]

                                                else
                                                    []
                                               )
                            }
                        )
                        model.rootItem
              }
            , Cmd.none
            )

        RemoveItem removeItem ->
            ( { model
                | rootItem =
                    mapItemsToItems
                        (\item ->
                            let
                                (ItemChildren newChildren) =
                                    item.children
                            in
                            { item
                                | children =
                                    ItemChildren <|
                                        List.filter (.id >> (/=) removeItem.id)
                                            newChildren
                            }
                        )
                        model.rootItem
              }
            , Cmd.none
            )

        ToggleControls ->
            ( { model | showControls = not model.showControls }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


mapItems : (Item -> List a -> a) -> Item -> a
mapItems mapper item =
    let
        (ItemChildren children) =
            item.children
    in
    mapper item <| List.map (mapItems mapper) children


mapItemsToItems : (Item -> Item) -> Item -> Item
mapItemsToItems mapper item =
    let
        (ItemChildren children) =
            item.children
    in
    mapper
        { item
            | children =
                ItemChildren <|
                    List.map
                        (mapItemsToItems mapper)
                        children
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


serializeItems : Item -> String
serializeItems rootItem =
    let
        a =
            Debug.log "rootItem" rootItem
    in
    mapItems
        (\item children ->
            let
                (Id id) =
                    item.id
            in
            String.join ""
                [ "{ id: '"
                , id
                , "', name: '"
                , item.content.name
                , "', children : ["
                , String.join ", " children
                , "] }"
                ]
        )
        rootItem


toNavKey : Model -> Nav.Key
toNavKey model =
    model.navKey
