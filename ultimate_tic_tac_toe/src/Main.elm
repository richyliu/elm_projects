module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
import Pages.UltimateTTT as UltimateTTT
import Url


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type Page
    = UltimateTTT UltimateTTT.Model


type alias Model =
    { page : Page
    , key : Nav.Key
    , url : Url.Url
    }


init : String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( pageModel, pageCmd ) =
            UltimateTTT.init flags
    in
    ( { page = UltimateTTT pageModel
      , key = key
      , url = url
      }
    , Cmd.map GotUltimateTTTMsg pageCmd
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotUltimateTTTMsg UltimateTTT.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    -- ( model, Nav.pushUrl model.key (Url.toString url) )
                    ( model, Nav.load <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            ( { model | url = url }
            , Cmd.none
            )

        ( GotUltimateTTTMsg pageMsg, UltimateTTT pageModel ) ->
            let
                ( resultModel, resultCmd ) =
                    UltimateTTT.update pageMsg pageModel
            in
            ( { model | page = UltimateTTT resultModel }
            , Cmd.map GotUltimateTTTMsg resultCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        UltimateTTT pageModel ->
            Sub.map GotUltimateTTTMsg <| UltimateTTT.subscriptions pageModel



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        UltimateTTT m ->
            let
                rendered =
                    UltimateTTT.view m
            in
            { title = rendered.title
            , body = List.map (Html.map GotUltimateTTTMsg) rendered.body
            }
