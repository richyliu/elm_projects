module Pages.Diy exposing (Model, Msg, init, subscriptions, toNavKey, update, view)

{-| Show list diy images
-}

import Api
import Api.Endpoints as Endpoints
import Browser.Navigation as Nav
import Card
import Html exposing (..)
import Html.Attributes exposing (attribute, class, placeholder, src, style)
import Html.Events exposing (onClick, onInput)
import Http
import Image exposing (Image)
import Random exposing (Generator, Seed)
import Random.List exposing (shuffle)
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , zoomImage : Maybe String
    , diy :
        Status
            { all : List Image
            , visible : List Image
            }
    }


type Status a
    = Loading
    | Loaded a
    | Failed


init : Nav.Key -> Int -> ( Model, Cmd Msg )
init navKey seedNum =
    let
        seed =
            Random.initialSeed seedNum
    in
    ( { navKey = navKey
      , zoomImage = Nothing
      , diy = Loading
      }
    , Api.getDiy (GotDiy seed)
    )


view : Model -> PageContent Msg
view model =
    { title = "Diy"
    , iconButtons = []
    , content =
        case model.zoomImage of
            Just image ->
                viewZoomImage image

            Nothing ->
                case model.diy of
                    Loading ->
                        div
                            [ class "pt-6 text-center text-xl" ]
                            [ h2 [] [ text "Loading..." ] ]

                    Failed ->
                        div [] [ text "Failed" ]

                    Loaded diy ->
                        div [ class "" ]
                            [ viewSearchbar
                            , viewDiy diy.visible
                            ]
    }


viewDiy : List Image -> Html Msg
viewDiy diyImages =
    div [ class "container mx-auto overflow-x-hidden pt-10" ]
        [ Card.viewCardsCompact <|
            List.map
                (\diy ->
                    { image = Endpoints.diyUrl diy
                    , callback = OpenZoom <| Endpoints.diyUrl diy
                    }
                )
                diyImages
        ]


viewSearchbar : Html Msg
viewSearchbar =
    div []
        [ input
            [ class "fixed p-2 w-full block appearance-none"
            , placeholder "Search..."
            , attribute "autocomplete" "off"
            , onInput Search
            ]
            []
        ]


viewZoomImage : String -> Html Msg
viewZoomImage zoomImage =
    div
        [ class "fixed h-full w-full"
        , style "background-color" "rgba(0, 0, 0, 0.7)"
        , onClick CloseZoom
        ]
        [ img [ class "w-full", src zoomImage ] []
        , p [ class "text-white p-1" ] [ text zoomImage ]
        ]



-- UPDATE


type Msg
    = GotDiy Seed (Result Http.Error (List Image))
    | Search String
    | OpenZoom String
    | CloseZoom


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDiy seed result ->
            case result of
                Ok diy ->
                    let
                        shuffler : Generator (List Image)
                        shuffler =
                            shuffle diy

                        ( shuffled, _ ) =
                            Random.step shuffler seed
                    in
                    ( { model
                        | diy =
                            Loaded
                                { all = shuffled
                                , visible = shuffled
                                }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | diy = Failed }, Cmd.none )

        Search str ->
            let
                filterDiy : List Image -> List Image
                filterDiy diy =
                    if String.length str == 0 then
                        diy

                    else
                        diy
                            |> List.sortBy Image.imageToString
                            |> List.filter (Image.imageToString >> String.contains (String.toLower str))
            in
            case model.diy of
                Loaded diy ->
                    ( { model
                        | diy =
                            Loaded
                                { diy | visible = filterDiy diy.all }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        OpenZoom image ->
            ( { model | zoomImage = Just image }, Cmd.none )

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
