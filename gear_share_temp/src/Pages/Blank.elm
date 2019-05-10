module Pages.Blank exposing (view)

import Browser exposing (Document)
import Html exposing (..)


view : { title : String, content : Html () }
view =
    { title = ""
    , content = p [] [ text "[blank page]" ]
    }
