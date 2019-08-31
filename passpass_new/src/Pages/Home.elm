module Pages.Home exposing (Model, Msg, init, subscriptions, toNavKey, update, view)

{-| Home of all different girls with the cover of the first album of each.
-}

import Browser.Navigation as Nav
import Html exposing (..)
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    }


type Status a
    = Loading
    | Loaded a
    | Failed


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( { navKey = navKey
      }
    , Cmd.none
    )


view : Model -> PageContent Msg
view _ =
    { title = "Models"
    , iconButtons = []
    , content =
        div [] [ text "hello, world" ]
    }



-- UPDATE


type Msg
    = Temp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Temp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toNavKey : Model -> Nav.Key
toNavKey model =
    model.navKey
