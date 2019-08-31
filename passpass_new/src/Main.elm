module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attr
import Pages.Blank as Blank
import Pages.Home as Home
import Route exposing (Route)
import Url exposing (Url)
import WrappedPage


type alias BaseModel =
    Model


type Model
    = Redirect Nav.Key
    | NotFound Nav.Key
    | Home Home.Model



-- MODEL


init : Bool -> Url -> Nav.Key -> ( BaseModel, Cmd Msg )
init _ url navKey =
    changeRouteTo
        (Route.fromUrl url)
        (Redirect navKey)



-- VIEW


view : BaseModel -> Document Msg
view model =
    let
        --  Renders the page based on the content, viewer, and the msg
        viewPage : (msg -> Msg) -> WrappedPage.PageContent msg -> Document Msg
        viewPage toMsg config =
            let
                { title, body } =
                    WrappedPage.view config
            in
            { title = title
            , body =
                [ Html.div [ Attr.class "content" ] <|
                    List.map (Html.map toMsg) body
                ]
            }
    in
    case model of
        Home home ->
            viewPage GotHomeMsg (Home.view home)

        _ ->
            viewPage (\_ -> NoOp) Blank.view



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | NoOp


{-| Converts a model to a navKey
-}
toNavKey : Model -> Nav.Key
toNavKey page =
    case page of
        Redirect navKey ->
            navKey

        NotFound navKey ->
            navKey

        Home home ->
            Home.toNavKey home


changeRouteTo : Maybe Route -> BaseModel -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        navKey =
            toNavKey model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound navKey, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl navKey Route.Home )

        Just Route.Home ->
            Home.init navKey
                |> updateWith Home GotHomeMsg


update : Msg -> BaseModel -> ( BaseModel, Cmd Msg )
update msg model =
    let
        ( newModel, newCmd ) =
            case ( msg, model ) of
                ( ClickedLink urlRequest, _ ) ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( model
                            , Nav.pushUrl (toNavKey model) (Url.toString url)
                            )

                        Browser.External href ->
                            ( model
                            , Nav.load href
                            )

                ( ChangedUrl url, _ ) ->
                    changeRouteTo (Route.fromUrl url) model

                ( GotHomeMsg subMsg, Home home ) ->
                    Home.update subMsg home
                        |> updateWith Home GotHomeMsg

                ( _, _ ) ->
                    -- Disregard messages that arrived for the wrong page.
                    ( model, Cmd.none )
    in
    ( newModel, newCmd )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : BaseModel -> Sub Msg
subscriptions model =
    case model of
        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        _ ->
            Sub.none



-- MAIN


{-| Bool flag is whether user is logged in or not
-}
main : Program Bool BaseModel Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
