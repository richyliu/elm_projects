module Pages.Album exposing (Model, Msg, init, subscriptions, toNavKey, update, view)

{-| Shows all the images of an album for a girl
-}

import Album exposing (Album)
import Api
import Api.Endpoints as Endpoints
import Browser.Navigation as Nav
import Card exposing (viewCardsCompact)
import Girl exposing (Girl)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, src, style)
import Html.Events exposing (onClick)
import Http
import Icon
import Id exposing (Id)
import Image exposing (Image)
import List.Extra as List
import Random
import Route
import Scroller
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , images : Status (List Image)
    , album : Album
    , id : Id
    , zoomImage : Maybe Image
    , name : String
    , albums : List Album
    }


type Status a
    = Loading
    | Loaded a
    | Failed


init : Nav.Key -> Id -> Album -> Maybe Image -> ( Model, Cmd Msg )
init navKey id album scrollTo =
    ( { navKey = navKey
      , images = Loading
      , album = album
      , id = id
      , zoomImage = Nothing
      , name = ""
      , albums = []
      }
    , Cmd.batch <|
        [ Api.getImages id album GotImages
        , Api.getGirl id GotGirl
        ]
            ++ (case scrollTo of
                    Just image ->
                        [ Scroller.scrollTo <| Image.imageToString image ]

                    Nothing ->
                        [ Scroller.scrollTop ]
               )
    )


view : Model -> PageContent Msg
view model =
    { title =
        capitalCase <|
            String.join " "
                [ model.name
                , String.toUpper <| Id.idToString model.id
                , Album.albumToString model.album
                ]
    , iconButtons =
        [ button [ onClick RandomAlbum ] [ Icon.icon "icon-random" ]
        , button [ onClick BackToGirl ] [ Icon.icon "icon-reply" ]
        ]
    , content =
        div []
            [ case model.zoomImage of
                Just zoomImage ->
                    viewZoomImage model.id model.album zoomImage

                Nothing ->
                    div [] []
            , case model.images of
                Loading ->
                    div [] [ text "loading" ]

                Failed ->
                    div [] [ text "failed" ]

                Loaded images ->
                    div []
                        [ viewImages model.id model.album images
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


viewImages : Id -> Album -> List Image -> Html Msg
viewImages id album images =
    viewCardsCompact <|
        List.map
            (\image ->
                { image = Endpoints.imageUrl id album image
                , callback = OpenZoom image
                }
            )
            images


viewZoomImage : Id -> Album -> Image -> Html Msg
viewZoomImage id album zoomImage =
    div
        [ class "fixed h-full w-full z-50"
        , style "background-color" "rgba(0, 0, 0, 0.7)"
        , onClick CloseZoom
        ]
        [ img
            [ class "w-full"
            , src <| Endpoints.imageUrl id album zoomImage
            ]
            []
        , p
            [ class "text-white p-1" ]
            [ text <| Image.imageToString zoomImage ]
        ]


capitalCase : String -> String
capitalCase str =
    str
        |> String.split " "
        |> List.map
            (\s ->
                String.toUpper (String.slice 0 1 s)
                    ++ String.slice 1 (String.length s) s
            )
        |> String.join " "



-- UPDATE


type Msg
    = GotImages (Result Http.Error (List Image))
    | OpenZoom Image
    | CloseZoom
    | GotGirl (Result Http.Error Girl)
    | RandomAlbum
    | GotAlbums (Result Http.Error (List Album))
    | ChooseRandomAlbum Int
    | BackToGirl


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

        GotGirl result ->
            case result of
                Ok girl ->
                    ( { model | name = girl.name }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        RandomAlbum ->
            ( model, Api.getAlbums model.id GotAlbums )

        GotAlbums result ->
            case result of
                Ok albums ->
                    ( { model | albums = albums }
                    , Random.generate ChooseRandomAlbum <| Random.int 0 <| List.length albums
                    )

                Err _ ->
                    ( model, Cmd.none )

        ChooseRandomAlbum albumNum ->
            let
                album : Album
                album =
                    model.albums
                        |> List.getAt albumNum
                        |> Maybe.withDefault Album.first
            in
            ( model, Route.pushUrl model.navKey <| Route.Album model.id album Nothing )

        BackToGirl ->
            ( model, Route.pushUrl model.navKey <| Route.Gallery model.id )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toNavKey : Model -> Nav.Key
toNavKey model =
    model.navKey
