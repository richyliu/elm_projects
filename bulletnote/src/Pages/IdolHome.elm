module Pages.IdolHome exposing (Model, Msg, init, subscriptions, toNavKey, update, view)

{-| Home of all different idol girls with the id of each to choose from
-}

import Api
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Icon
import IdolAlbum
import IdolGirl exposing (IdolGirl)
import Route
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , idolGirls : Status (List IdolGirl)
    , search : String
    }


type Status a
    = Loading
    | Loaded a
    | Failed


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( { navKey = navKey
      , idolGirls = Loading
      , search = ""
      }
    , Api.getIdolGirls GotGirls
    )


view : Model -> PageContent Msg
view model =
    { title = "Idol Models"
    , iconButtons = []
    , content =
        case model.idolGirls of
            Loading ->
                div
                    [ class "pt-6 text-center text-xl" ]
                    [ h2 [] [ text "Loading..." ] ]

            Failed ->
                div [] [ text "failed" ]

            Loaded idolGirls ->
                div [ class "container mx-auto p-2" ]
                    [ viewSearch
                    , viewIdolGirls idolGirls
                    ]
    }


viewIdolGirls : List IdolGirl -> Html Msg
viewIdolGirls idolGirls =
    let
        viewIdolGirl : IdolGirl -> Html Msg
        viewIdolGirl idolGirl =
            a
                [ class "block mx-2 my-4 p-2 rounded bg-blue-400 text-white font-bold"
                , Route.href <| Route.IdolGallery idolGirl IdolAlbum.first
                ]
                [ text <| IdolGirl.idolGirlToString idolGirl ]
    in
    div [ class "" ] <|
        List.map viewIdolGirl idolGirls


viewSearch : Html Msg
viewSearch =
    div [ class "m-2 pt-2 flex flex-row" ]
        [ input
            [ class "flex-1 px-2 py-1 rounded-lg bg-gray-300 focus:bg-white focus:border trans"
            , onInput UpdateSearch
            , placeholder "Custom tag..."
            ]
            []
        , button
            [ class "flex-initial ml-2 p-3 pb-0 text-xl rounded-lg bg-blue-400 text-white"
            , onClick GoToSearch
            ]
            [ Icon.icon "icon-search" ]
        ]



-- UPDATE


type Msg
    = GotGirls (Result Http.Error (List IdolGirl))
    | UpdateSearch String
    | GoToSearch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGirls result ->
            case result of
                Ok idolGirls ->
                    ( { model | idolGirls = Loaded idolGirls }, Cmd.none )

                Err _ ->
                    ( { model | idolGirls = Failed }, Cmd.none )

        UpdateSearch str ->
            ( { model | search = str }, Cmd.none )

        GoToSearch ->
            if String.length model.search == 0 then
                ( model, Cmd.none )

            else
                ( model
                , Route.pushUrl model.navKey <|
                    Route.IdolGallery (IdolGirl.createGirl model.search) IdolAlbum.first
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toNavKey : Model -> Nav.Key
toNavKey model =
    model.navKey
