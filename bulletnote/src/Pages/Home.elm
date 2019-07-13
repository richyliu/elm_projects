module Pages.Home exposing (Model, Msg, init, subscriptions, toNavKey, update, view)

{-| Home of all different girls with the cover of the first album of each.
-}

import Album
import Api
import Api.Endpoints exposing (imageUrl)
import Browser.Navigation as Nav
import Card exposing (viewCards)
import Girl exposing (Girl)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, placeholder)
import Html.Events exposing (onInput)
import Http
import Id
import Image
import Route
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , girls :
        Status
            { all : List Girl
            , visible : List Girl
            }
    }


type Status a
    = Loading
    | Loaded a
    | Failed


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( { navKey = navKey
      , girls = Loading
      }
    , Api.getGirls GotGirls
    )


view : Model -> PageContent Msg
view model =
    { title = "Models"
    , iconButtons = []
    , content =
        case model.girls of
            Loading ->
                div
                    [ class "pt-6 text-center text-xl" ]
                    [ h2 [] [ text "Loading..." ] ]

            Failed ->
                div [] [ text "failed" ]

            Loaded girls ->
                div [ class "" ]
                    [ viewSearchbar
                    , viewGirls girls.visible
                    ]
    }


viewGirls : List Girl -> Html Msg
viewGirls girls =
    div [ class "pt-10" ]
        [ viewCards <|
            List.map
                (\girl ->
                    { img = imageUrl girl.id Album.first Image.first
                    , title = girl.name
                    , subtitle = Just <| "Id: " ++ Id.idToString girl.id
                    , link = Route.Gallery girl.id
                    }
                )
                girls
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



-- UPDATE


type Msg
    = GotGirls (Result Http.Error (List Girl))
    | Search String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGirls result ->
            case result of
                Ok girls ->
                    ( { model
                        | girls =
                            Loaded { all = girls, visible = girls }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | girls = Failed }, Cmd.none )

        Search str ->
            let
                filterGirls : List Girl -> List Girl
                filterGirls girls =
                    List.filter
                        (\girl ->
                            String.contains
                                (String.toLower str)
                                (girl.name ++ Id.idToString girl.id)
                        )
                        girls
            in
            case model.girls of
                Loaded girls ->
                    ( { model
                        | girls =
                            Loaded
                                { girls | visible = filterGirls girls.all }
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
