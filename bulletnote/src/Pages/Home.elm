module Pages.Home exposing (Model, Msg, init, subscriptions, toNavKey, update, view)

{-| Home TODO description
-}

import Browser.Navigation as Nav
import Html exposing (..)
import Route
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , data : String
    }


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( { navKey = navKey
      , data = "the data"
      }
    , Cmd.none
    )


view : Model -> PageContent Msg
view model =
    { title = "Home"
    , content =
        p [] [ text model.data ]
    }



-- UPDATE


type Msg
    = GotString String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotString str ->
            ( { model | data = str }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toNavKey : Model -> Nav.Key
toNavKey model =
    model.navKey
