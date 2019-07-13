module Pages.IdolGallery exposing (Model, Msg, init, subscriptions, toNavKey, update, view)

{-| Shows all the albums on a page for an idol girl
-}

import Api
import Browser.Navigation as Nav
import Card exposing (viewCards)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Icon
import IdolAlbum exposing (IdolAlbum)
import IdolGirl exposing (IdolGirl)
import Route
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , idolGirl : IdolGirl
    , albums : Status (List IdolAlbum)
    , page : Int
    }


type Status a
    = Loading
    | Loaded a
    | Failed


init : Nav.Key -> IdolGirl -> Int -> ( Model, Cmd Msg )
init navKey idolGirl page =
    ( { navKey = navKey
      , idolGirl = idolGirl
      , albums = Loading
      , page = page
      }
    , Api.getIdolAlbums idolGirl page GotAlbums
    )


view : Model -> PageContent Msg
view model =
    let
        iconButtons =
            [ button [ onClick <| ChangePage -1 ] [ Icon.icon "icon-arrow-alt-left" ]
            , button [ onClick <| ChangePage 1 ] [ Icon.icon "icon-arrow-alt-right" ]
            ]
    in
    case model.albums of
        Loading ->
            { title = "Gallery"
            , iconButtons = []
            , content = div [] [ text "loading" ]
            }

        Failed ->
            { title = "Gallery"
            , iconButtons = iconButtons
            , content = div [] [ text "failet" ]
            }

        Loaded albums ->
            { title =
                IdolGirl.idolGirlToString model.idolGirl
                    ++ " "
                    ++ String.fromInt model.page
            , iconButtons = iconButtons
            , content = viewAlbums model albums
            }


viewAlbums : Model -> List IdolAlbum -> Html Msg
viewAlbums model albums =
    viewCards <|
        List.map
            (\album ->
                { img = IdolAlbum.idolAlbumCover album
                , title = IdolAlbum.removeOrigin album
                , subtitle = Nothing
                , link = Route.IdolAlbum model.idolGirl model.page album
                }
            )
            albums



-- UPDATE


type Msg
    = GotAlbums (Result Http.Error (List IdolAlbum))
    | ChangePage Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAlbums result ->
            case result of
                Ok albums ->
                    ( { model | albums = Loaded albums }, Cmd.none )

                Err _ ->
                    ( { model | albums = Failed }, Cmd.none )

        ChangePage change ->
            let
                newPage =
                    max IdolAlbum.first <| model.page + change
            in
            ( model
            , Route.pushUrl model.navKey <| Route.IdolGallery model.idolGirl newPage
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toNavKey : Model -> Nav.Key
toNavKey model =
    model.navKey
