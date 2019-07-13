module Card exposing (viewCards, viewCardsCompact)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Image
import List.Extra as List
import Route exposing (Route)


type alias CardOptions =
    { img : String
    , title : String
    , subtitle : Maybe String
    , link : Route
    }


viewCard : CardOptions -> Html msg
viewCard options =
    div [ class "rounded shadow hover:shadow-lg mx-1 lg:mx-4 my-3 lg:my-6 trans" ]
        [ a [ class "block", Route.href options.link ]
            [ Image.lazyLoadImage options.img
            , div [ class "p-1 flex flex-row" ]
                [ h2 [ class "font-bold text-lg capitalize flex-auto break-all" ] [ text options.title ]
                , p
                    [ class "text-sm font-mono flex-initial mt-1" ]
                    [ text <| Maybe.withDefault "" options.subtitle ]
                ]
            ]
        ]


viewCards : List CardOptions -> Html msg
viewCards cards =
    let
        renderedCards : List (Html msg)
        renderedCards =
            List.map viewCard cards

        ( left, right ) =
            List.indexedFoldr
                (\i card ( l, r ) ->
                    if remainderBy 2 i == 0 then
                        ( card :: l, r )

                    else
                        ( l, card :: r )
                )
                ( [], [] )
                renderedCards
    in
    div
        [ class "container flex flex-row overflow-x-hidden mx-auto" ]
        [ div [ class "flex-1" ] left
        , div [ class "flex-1" ] right
        ]


{-| View that only have an image without much space and a callback for click
-}
viewCardsCompact : List { image : String, callback : msg } -> Html msg
viewCardsCompact options =
    let
        renderedCards : List (Html msg)
        renderedCards =
            List.map
                (\{ image, callback } ->
                    div
                        [ class "mx-1 my-2 cursor-pointer"
                        , onClick callback
                        ]
                        [ Image.lazyLoadImage image ]
                )
                options

        ( left, right ) =
            List.indexedFoldr
                (\i card ( l, r ) ->
                    if remainderBy 2 i == 0 then
                        ( card :: l, r )

                    else
                        ( l, card :: r )
                )
                ( [], [] )
                renderedCards
    in
    div
        [ class "container flex flex-row overflow-x-hidden mx-auto" ]
        [ div [ class "flex-1" ] left
        , div [ class "flex-1" ] right
        ]
