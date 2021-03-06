module WrappedPage exposing (PageContent, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Lazy exposing (lazy, lazy2)


type alias PageContent msg =
    { title : String
    , content : Html msg
    }


{-| Take a page's Html and frames it with a header
-}
view : Html msg -> Html msg
view content =
    -- The "content" class is required (in order to hide elm debugger)
    div [ class "content px-4" ]
        [ viewNavbar
        , content
        ]


{-| Static navbar
-}
viewNavbar : Html msg
viewNavbar =
    div [] [ text "navbar" ]
