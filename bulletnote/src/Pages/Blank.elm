module Pages.Blank exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import WrappedPage exposing (PageContent)


view : PageContent msg
view =
    { title = "Not Found"
    , content = p [ class "mx-4 mt-10" ] [ text "[blank page]" ]
    }
