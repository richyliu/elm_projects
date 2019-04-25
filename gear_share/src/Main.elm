module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
import Pages.Home as Home
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
    = Home Home.Model


type alias Model =
    { page : Page
    , key : Nav.Key
    , url : Url.Url
    }


init : String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( pageModel, pageCmd ) =
            Home.init flags
    in
    ( { page = Home pageModel
      , key = key
      , url = url
      }
    , Cmd.map GotHomeMsg pageCmd
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg Home.Msg
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

        ( GotHomeMsg pageMsg, Home pageModel ) ->
            let
                ( resultModel, resultCmd ) =
                    Home.update pageMsg pageModel
            in
            ( { model | page = Home resultModel }
            , Cmd.map GotHomeMsg resultCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Home pageModel ->
            Sub.map GotHomeMsg <| Home.subscriptions pageModel



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Home m ->
            let
                rendered =
                    Home.view m
            in
            { title = rendered.title
            , body = List.map (Html.map GotHomeMsg) rendered.body
            }
