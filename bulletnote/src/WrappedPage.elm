module WrappedPage exposing (PageContent, view, viewContent, viewHeader)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Lazy exposing (lazy, lazy2)


type alias PageContent msg =
    { title : String
    , content : Html msg
    , iconButtons : List (Html msg)
    }


{-| Take a page's Html and frames it with a header
-}
view : PageContent msg -> Document msg
view { title, content, iconButtons } =
    { title = "MT"
    , body =
        [ div [ class "w-full font-sans" ] <|
            [ lazy2 viewHeader title iconButtons
            , lazy viewContent content
            ]
        ]
    }


viewHeader : String -> List (Html msg) -> Html msg
viewHeader header iconButtons =
    div [ class "fixed bg-gray-400 top-0 w-full z-20 shadow-md pt-3 pb-1 text-center" ] <|
        (case iconButtons of
            [ left, right ] ->
                [ div [ class "float-left p-2 w-0" ] [ left ]
                , div [ class "float-right p-2 w-0 mr-6" ] [ right ]
                ]

            _ ->
                []
        )
            ++ [ h1 [ class "text-xl font-bold" ] [ text header ] ]


viewContent : Html msg -> Html msg
viewContent content =
    div [ class "w-full mt-12" ] [ content ]
