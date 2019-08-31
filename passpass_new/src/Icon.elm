module Icon exposing (icon)

import Html exposing (Html, i)
import Html.Attributes exposing (class)


icon : String -> Html msg
icon name =
    i [ class <| "icon " ++ name ] []
