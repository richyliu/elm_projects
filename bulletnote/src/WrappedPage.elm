module WrappedPage exposing (PageContent, view, viewContent)

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
view : PageContent msg -> Document msg
view { title, content } =
    { title = title
    , body =
        -- The "content" class is required (in order to hide elm debugger)
        [ div [ class "content" ] <|
            [ lazy viewContent content
            ]
        ]
    }


viewContent : Html msg -> Html msg
viewContent content =
    div [ class "" ] [ content ]
