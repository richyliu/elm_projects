module Item exposing
    ( Id
    , Item
    , ItemChildren
    , createItem
    , createItemWithId
    , getId
    , itemDecoder
    , itemEncoder
    , mapItems
    , mapItemsNewChildren
    , mapItemsToItems
    )

{-| Utilities and functions for dealing with items
-}

import Json.Decode exposing (Decoder, lazy, list, map, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode


type alias Item =
    { id : Id
    , content : ItemContent
    , editing : ItemEditing
    , children : ItemChildren
    }


getId : Id -> String
getId (Id id) =
    id


type alias ItemContent =
    { name : String
    , time : Maybe String
    }


type alias ItemEditing =
    { name : Bool
    , time : Bool
    }


defaultEditing : ItemEditing
defaultEditing =
    ItemEditing False False


type Id
    = Id String


type ItemChildren
    = ItemChildren (List Item)


getItemChildren : ItemChildren -> List Item
getItemChildren (ItemChildren children) =
    children



-- INITIALIZATION


createItem : String -> List Item -> Item
createItem name children =
    { id = Id name
    , content = { name = name, time = Just "2018-03-02T17:30" }
    , editing = { name = False, time = False }
    , children = ItemChildren children
    }


createItemWithId : String -> String -> List Item -> Item
createItemWithId id name children =
    { id = Id id
    , content = { name = name, time = Nothing }
    , editing = { name = False, time = False }
    , children = ItemChildren children
    }



-- ITERATION


{-| Given a mapper function that takes the current element, mapped results of
the children, it should return a new mapped value.
-}
mapItems : (Item -> List a -> a) -> Item -> a
mapItems mapper item =
    mapper item <|
        List.map
            (mapItems mapper)
            (getItemChildren item.children)


{-| Same as mapItems, except it automatically merges the mapped ItemChildren
into the children of the item
-}
mapItemsToItems : (Item -> Item) -> Item -> Item
mapItemsToItems mapper item =
    mapper
        { item
            | children =
                ItemChildren <|
                    List.map
                        (mapItemsToItems mapper)
                        (getItemChildren item.children)
        }


{-| Same as mapItems, except it allows for the item children to be modified as well
-}
mapItemsNewChildren : (Item -> List Item -> ( Item, List Item )) -> Item -> Item
mapItemsNewChildren mapper item =
    let
        ( newItem, newChildren ) =
            mapper item (getItemChildren item.children)
    in
    { newItem
        | children =
            ItemChildren <|
                List.map
                    (mapItemsNewChildren mapper)
                    newChildren
    }



-- DECODING


itemDecoder : Decoder Item
itemDecoder =
    succeed Item
        |> required "id" idDecoder
        |> required "content" contentDecoder
        |> hardcoded defaultEditing
        |> required "children" childrenDecoder


idDecoder : Decoder Id
idDecoder =
    map Id string


contentDecoder : Decoder ItemContent
contentDecoder =
    succeed ItemContent
        |> required "name" string
        |> optional "time" (map Just string) Nothing


childrenDecoder : Decoder ItemChildren
childrenDecoder =
    map ItemChildren (list (lazy (\_ -> itemDecoder)))



-- SERIALIZATION


itemEncoder : Item -> Encode.Value
itemEncoder item =
    Encode.object
        [ ( "id", Encode.string <| getId item.id )
        , ( "content"
          , Encode.object
                [ ( "name", Encode.string item.content.name )
                , ( "time"
                  , Maybe.withDefault Encode.null <|
                        Maybe.map Encode.string item.content.time
                  )
                ]
          )
        , ( "children", Encode.list itemEncoder (getItemChildren item.children) )
        ]
