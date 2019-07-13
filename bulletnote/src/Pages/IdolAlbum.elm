module Pages.IdolAlbum exposing (Model, Msg, init, subscriptions, toNavKey, update, view)

{-| Shows all the images of an album for an idol girl
-}

import Api
import Browser.Navigation as Nav
import Card exposing (viewCardsCompact)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, src, style)
import Html.Events exposing (onClick)
import Http
import Icon
import IdolAlbum exposing (IdolAlbum)
import IdolGirl exposing (IdolGirl)
import IdolImage exposing (IdolImage)
import List.Extra as List
import Scroller
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , images : Status (List IdolImage)
    , idolAlbum : IdolAlbum
    , zoomImage : Maybe String
    }


type Status a
    = Loading
    | Loaded a
    | Failed


init : Nav.Key -> IdolGirl -> Int -> IdolAlbum -> ( Model, Cmd Msg )
init navKey _ _ idolAlbum =
    ( { navKey = navKey
      , images = Loading
      , idolAlbum = idolAlbum
      , zoomImage = Nothing
      }
    , Api.getIdolImages idolAlbum GotImages
    )


view : Model -> PageContent Msg
view model =
    { title = IdolAlbum.removeOrigin model.idolAlbum
    , iconButtons = []
    , content =
        div []
            [ case model.zoomImage of
                Just zoomImage ->
                    viewZoomImage zoomImage

                Nothing ->
                    div [] []
            , case model.images of
                Loading ->
                    div [] [ text "loading" ]

                Failed ->
                    div [] [ text "failed" ]

                Loaded images ->
                    div []
                        [ viewImages images
                        , div
                            -- controlled by JS (initBigScroller)
                            [ Attr.id "big-scroller"
                            , style "transition" "left .5s"
                            , class "fixed rounded-r-full top-0 left-0 mt-12 w-16 h-16 bg-blue-700 z-40"
                            ]
                            [ p
                                [ class "relative top-0 left-0 pl-5 pt-5 w-16 h-16 text-2xl text-white" ]
                                [ Icon.icon "icon-arrows-v" ]
                            ]
                        ]
            ]
    }


viewImages : List IdolImage -> Html Msg
viewImages images =
    viewCardsCompact <|
        List.map
            (\image ->
                { image = IdolImage.lowRes image
                , callback = OpenZoom <| IdolImage.highRes image
                }
            )
            images


viewZoomImage : String -> Html Msg
viewZoomImage zoomImage =
    div
        [ class "fixed h-full w-full z-50"
        , style "background-color" "rgba(0, 0, 0, 0.7)"
        , onClick CloseZoom
        ]
        [ img [ class "w-full", src zoomImage ] [] ]



-- UPDATE


type Msg
    = GotImages (Result Http.Error (List IdolImage))
    | OpenZoom String
    | CloseZoom


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotImages result ->
            case result of
                Ok images ->
                    ( { model | images = Loaded images }, Scroller.initScroller )

                Err _ ->
                    ( { model | images = Failed }, Cmd.none )

        OpenZoom zoomImage ->
            ( { model | zoomImage = Just zoomImage }, Cmd.none )

        CloseZoom ->
            ( { model | zoomImage = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toNavKey : Model -> Nav.Key
toNavKey model =
    model.navKey
