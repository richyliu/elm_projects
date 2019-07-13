module Pages.Videos exposing (Model, Msg, init, subscriptions, toNavKey, update, view)

{-| Show list of videos from /var/www/html/videos
-}

import Api
import Api.Endpoints as Endpoints
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, placeholder)
import Html.Events exposing (onInput)
import Http
import Image
import Random exposing (Generator, Seed)
import Random.List exposing (shuffle)
import Video exposing (Video)
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , videos :
        Status
            { all : List Video
            , visible : List Video
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
      , videos = Loading
      }
    , Api.getVideos (GotVideos seed)
    )


view : Model -> PageContent Msg
view model =
    { title = "Videos"
    , iconButtons = []
    , content =
        case model.videos of
            Loading ->
                div
                    [ class "pt-6 text-center text-xl" ]
                    [ h2 [] [ text "Loading..." ] ]

            Failed ->
                div [] [ text "Failed" ]

            Loaded videos ->
                div [ class "" ]
                    [ viewSearchbar
                    , viewVideos videos.visible
                    ]
    }


viewVideos : List Video -> Html Msg
viewVideos videos =
    let
        viewVideo : Video -> Html Msg
        viewVideo video =
            div
                [ class "my-2 md:p-2 md:my-4 md:bg-white md:shadow hover:shadow-lg md:rounded trans"
                ]
                [ a [ href <| Endpoints.videoUrl video ]
                    [ Image.lazyLoadImage <| Endpoints.videoImageUrl video ]
                , h2 [ class "p-1 pb-2 text-base font-normal" ] [ text <| Video.videoToString video ]
                ]
    in
    div [ class "container mx-auto overflow-x-hidden pt-10" ] <|
        List.map viewVideo videos


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



-- UPDATE


type Msg
    = GotVideos Seed (Result Http.Error (List Video))
    | Search String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotVideos seed result ->
            case result of
                Ok videos ->
                    let
                        shuffler : Generator (List Video)
                        shuffler =
                            shuffle videos

                        ( shuffled, _ ) =
                            Random.step shuffler seed
                    in
                    ( { model
                        | videos =
                            Loaded
                                { all = shuffled
                                , visible = shuffled
                                }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | videos = Failed }, Cmd.none )

        Search str ->
            let
                filterVideos : List Video -> List Video
                filterVideos videos =
                    if String.length str == 0 then
                        videos

                    else
                        videos
                            |> List.sortBy Video.videoToString
                            |> List.filter (Video.videoToString >> String.contains (String.toLower str))
            in
            case model.videos of
                Loaded videos ->
                    ( { model
                        | videos =
                            Loaded
                                { videos | visible = filterVideos videos.all }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toNavKey : Model -> Nav.Key
toNavKey model =
    model.navKey
