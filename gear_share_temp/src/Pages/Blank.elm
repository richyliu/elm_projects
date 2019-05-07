module Pages.Blank exposing (view)

import Browser exposing (Document)
import Html exposing (Html, text)


view : Document msg
view =
    { title = ""
    , body = [ text "" ]
    }
