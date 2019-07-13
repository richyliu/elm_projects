module Pages.Gallery exposing (Model, Msg, init, subscriptions, toNavKey, update, view)

{-| Shows all the albums for one girl
-}

import Album exposing (Album)
import Api
import Api.Endpoints exposing (imageUrl)
import Browser.Navigation as Nav
import Card exposing (viewCards)
import Girl exposing (Girl)
import Html exposing (..)
import Http
import Id exposing (Id)
import Image
import Route
import Scroller
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , albums : Status (List Album)
    , id : Id
    , name : String
    }


type Status a
    = Loading
    | Loaded a
    | Failed


init : Nav.Key -> Id -> Maybe Album -> ( Model, Cmd Msg )
init navKey id scrollTo =
    ( { navKey = navKey
      , albums = Loading
      , id = id
      , name = ""
      }
    , Cmd.batch <|
        [ Api.getAlbums id GotAlbums
        , Api.getGirls GotGirls
        ]
            ++ (case scrollTo of
                    Just album ->
                        -- tell JS to scroll to the image that has the album
                        -- album is in a separate part of the image url
                        -- Ex; album 6 -> "/a/6/020.jpg"
                        [ Scroller.scrollTo <| "/" ++ Album.albumToString album ++ "/" ]

                    Nothing ->
                        []
               )
    )


view : Model -> PageContent Msg
view model =
    case model.albums of
        Loading ->
            { title = "Gallery"
            , iconButtons = []
            , content = div [] [ text "loading" ]
            }

        Failed ->
            { title = "Gallery"
            , iconButtons = []
            , content = div [] [ text "failet" ]
            }

        Loaded albums ->
            { title = capitalCase <| model.name ++ " " ++ String.toUpper (Id.idToString model.id)
            , iconButtons = []
            , content = viewAlbums model.id albums
            }


viewAlbums : Id -> List Album -> Html Msg
viewAlbums id albums =
    viewCards <|
        List.map
            (\album ->
                { img = imageUrl id album Image.first
                , title = Album.albumToString album
                , subtitle = Nothing
                , link = Route.Album id album Nothing
                }
            )
            albums


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
    = GotAlbums (Result Http.Error (List Album))
    | GotGirls (Result Http.Error (List Girl))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAlbums result ->
            case result of
                Ok albums ->
                    ( { model | albums = Loaded albums }, Cmd.none )

                Err _ ->
                    ( { model | albums = Failed }, Cmd.none )

        GotGirls result ->
            case result of
                Ok girls ->
                    ( { model
                        | name =
                            girls
                                |> List.filter (.id >> (==) model.id)
                                |> List.map .name
                                |> List.head
                                |> Maybe.withDefault ""
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toNavKey : Model -> Nav.Key
toNavKey model =
    model.navKey
