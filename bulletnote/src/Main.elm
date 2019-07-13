module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attr
import Navbar
import Pages.Album as Album
import Pages.Blank as Blank
import Pages.Diy as Diy
import Pages.Gallery as Gallery
import Pages.Home as Home
import Pages.IdolAlbum as IdolAlbum
import Pages.IdolGallery as IdolGallery
import Pages.IdolHome as IdolHome
import Pages.Login as Login
import Pages.Settings as Settings
import Pages.Shuffle as Shuffle
import Pages.Videos as Videos
import Route exposing (Route)
import Scroller
import Url exposing (Url)
import WrappedPage


type alias BaseModel =
    { model : Model
    , viewModel : Navbar.Model
    , prev : Model
    }


type Model
    = Redirect Nav.Key
    | NotFound Nav.Key
    | Home Home.Model
    | Gallery Gallery.Model
    | Album Album.Model
    | Shuffle Shuffle.Model
    | Login Login.Model
    | Videos Videos.Model
    | Diy Diy.Model
    | Settings Settings.Model
    | IdolHome IdolHome.Model
    | IdolGallery IdolGallery.Model
    | IdolAlbum IdolAlbum.Model



-- MODEL


init : Bool -> Url -> Nav.Key -> ( BaseModel, Cmd Msg )
init loggedIn url navKey =
    let
        ( viewModel, cmd1 ) =
            Navbar.init navKey

        ( model, cmd2 ) =
            changeRouteTo
                (if loggedIn then
                    Route.fromUrl url

                 else
                    Just Route.Login
                )
                { model = Redirect navKey
                , viewModel = viewModel
                , prev = NotFound navKey
                }
    in
    ( { model = model
      , viewModel = viewModel
      , prev = NotFound navKey
      }
    , Cmd.batch [ Cmd.map GotNavbarMsg cmd1, cmd2 ]
    )



-- VIEW


view : BaseModel -> Document Msg
view { model, viewModel } =
    let
        --  Renders the page based on the content, viewer, and the msg
        viewPage : (msg -> Msg) -> WrappedPage.PageContent msg -> Document Msg
        viewPage toMsg config =
            let
                { title, body } =
                    WrappedPage.view config
            in
            { title = title

            -- puts navbar and wrapped body together into a div
            , body =
                [ Html.div [ Attr.class "content" ] <|
                    List.map (Html.map toMsg) body
                        ++ [ Html.map GotNavbarMsg (Navbar.view viewModel) ]
                ]
            }
    in
    case model of
        Home home ->
            viewPage GotHomeMsg (Home.view home)

        Gallery gallery ->
            viewPage GotGalleryMsg (Gallery.view gallery)

        Album album ->
            viewPage GotAlbumMsg (Album.view album)

        Shuffle shuffle ->
            viewPage GotShuffleMsg (Shuffle.view shuffle)

        Login login ->
            viewPage GotLoginMsg (Login.view login)

        Videos videos ->
            viewPage GotVideosMsg (Videos.view videos)

        Diy diy ->
            viewPage GotDiyMsg (Diy.view diy)

        Settings settings ->
            viewPage GotSettingsMsg (Settings.view settings)

        IdolHome idolHome ->
            viewPage GotIdolHomeMsg (IdolHome.view idolHome)

        IdolGallery idolGallery ->
            viewPage GotIdolGalleryMsg (IdolGallery.view idolGallery)

        IdolAlbum idolAlbum ->
            viewPage GotIdolAlbumMsg (IdolAlbum.view idolAlbum)

        _ ->
            viewPage (\_ -> NoOp) Blank.view



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotGalleryMsg Gallery.Msg
    | GotAlbumMsg Album.Msg
    | GotShuffleMsg Shuffle.Msg
    | GotLoginMsg Login.Msg
    | GotVideosMsg Videos.Msg
    | GotDiyMsg Diy.Msg
    | GotSettingsMsg Settings.Msg
    | GotIdolHomeMsg IdolHome.Msg
    | GotIdolGalleryMsg IdolGallery.Msg
    | GotIdolAlbumMsg IdolAlbum.Msg
    | GotNavbarMsg Navbar.Msg
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

        Gallery gallery ->
            Gallery.toNavKey gallery

        Album album ->
            Album.toNavKey album

        Shuffle shuffle ->
            Shuffle.toNavKey shuffle

        Login login ->
            Login.toNavKey login

        Videos videos ->
            Videos.toNavKey videos

        Diy diy ->
            Diy.toNavKey diy

        Settings settings ->
            Settings.toNavKey settings

        IdolHome idolHome ->
            IdolHome.toNavKey idolHome

        IdolGallery idolGallery ->
            IdolGallery.toNavKey idolGallery

        IdolAlbum idolAlbum ->
            IdolAlbum.toNavKey idolAlbum


changeRouteTo : Maybe Route -> BaseModel -> ( Model, Cmd Msg )
changeRouteTo maybeRoute baseModel =
    let
        navKey =
            toNavKey model

        model =
            baseModel.model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound navKey, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl navKey Route.Home )

        Just Route.Home ->
            Home.init navKey
                |> updateWith Home GotHomeMsg

        Just (Route.Gallery id) ->
            let
                -- try to get the previous album to scroll gallery page to
                prevAlbum =
                    case baseModel.prev of
                        Album albumModel ->
                            Just albumModel.album

                        _ ->
                            -- did not come from album (via back button, just scroll to top)
                            Nothing
            in
            Gallery.init navKey id prevAlbum
                |> updateWith Gallery GotGalleryMsg

        Just (Route.Album id album maybeImage) ->
            Album.init navKey id album maybeImage
                |> updateWith Album GotAlbumMsg

        Just (Route.Shuffle seed) ->
            Shuffle.init navKey seed
                |> updateWith Shuffle GotShuffleMsg

        Just Route.Login ->
            Login.init navKey
                |> updateWith Login GotLoginMsg

        Just (Route.Videos seed) ->
            Videos.init navKey seed
                |> updateWith Videos GotVideosMsg

        Just (Route.Diy seed) ->
            Diy.init navKey seed
                |> updateWith Diy GotDiyMsg

        Just Route.Settings ->
            Settings.init navKey
                |> updateWith Settings GotSettingsMsg

        Just Route.IdolHome ->
            IdolHome.init navKey
                |> updateWith IdolHome GotIdolHomeMsg

        Just (Route.IdolGallery idolGirl page) ->
            IdolGallery.init navKey idolGirl page
                |> updateWith IdolGallery GotIdolGalleryMsg

        Just (Route.IdolAlbum idolGirl page idolAlbum) ->
            IdolAlbum.init navKey idolGirl page idolAlbum
                |> updateWith IdolAlbum GotIdolAlbumMsg


update : Msg -> BaseModel -> ( BaseModel, Cmd Msg )
update msg baseModel =
    let
        model =
            baseModel.model

        viewModel =
            baseModel.viewModel

        updateWithView toModel toMsg subModelCmd =
            let
                ( updatedModel, updatedCmd ) =
                    updateWith toModel toMsg subModelCmd
            in
            ( { baseModel | model = updatedModel }, updatedCmd )

        ( newModel, newCmd ) =
            case ( msg, model ) of
                ( ClickedLink urlRequest, _ ) ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( baseModel
                            , Nav.pushUrl (toNavKey model) (Url.toString url)
                            )

                        Browser.External href ->
                            ( baseModel
                            , Nav.load href
                            )

                ( ChangedUrl url, _ ) ->
                    let
                        ( updatedModel, updatedCmd ) =
                            changeRouteTo (Route.fromUrl url) baseModel
                    in
                    ( { baseModel
                        | model = updatedModel
                        , prev = model
                      }
                    , updatedCmd
                    )

                ( GotHomeMsg subMsg, Home home ) ->
                    Home.update subMsg home
                        |> updateWithView Home GotHomeMsg

                ( GotGalleryMsg subMsg, Gallery gallery ) ->
                    Gallery.update subMsg gallery
                        |> updateWithView Gallery GotGalleryMsg

                ( GotAlbumMsg subMsg, Album album ) ->
                    Album.update subMsg album
                        |> updateWithView Album GotAlbumMsg

                ( GotShuffleMsg subMsg, Shuffle shuffle ) ->
                    Shuffle.update subMsg shuffle
                        |> updateWithView Shuffle GotShuffleMsg

                ( GotLoginMsg subMsg, Login login ) ->
                    Login.update subMsg login
                        |> updateWithView Login GotLoginMsg

                ( GotVideosMsg subMsg, Videos videos ) ->
                    Videos.update subMsg videos
                        |> updateWithView Videos GotVideosMsg

                ( GotDiyMsg subMsg, Diy diy ) ->
                    Diy.update subMsg diy
                        |> updateWithView Diy GotDiyMsg

                ( GotSettingsMsg subMsg, Settings settings ) ->
                    Settings.update subMsg settings
                        |> updateWithView Settings GotSettingsMsg

                ( GotIdolHomeMsg subMsg, IdolHome idolHome ) ->
                    IdolHome.update subMsg idolHome
                        |> updateWithView IdolHome GotIdolHomeMsg

                ( GotIdolGalleryMsg subMsg, IdolGallery idolGallery ) ->
                    IdolGallery.update subMsg idolGallery
                        |> updateWithView IdolGallery GotIdolGalleryMsg

                ( GotIdolAlbumMsg subMsg, IdolAlbum idolAlbum ) ->
                    IdolAlbum.update subMsg idolAlbum
                        |> updateWithView IdolAlbum GotIdolAlbumMsg

                ( GotNavbarMsg subMsg, _ ) ->
                    let
                        ( newViewModel, viewCmd ) =
                            Navbar.update subMsg viewModel
                    in
                    ( { baseModel | viewModel = newViewModel }
                    , Cmd.map GotNavbarMsg viewCmd
                    )

                ( _, _ ) ->
                    -- Disregard messages that arrived for the wrong page.
                    ( baseModel, Cmd.none )
    in
    ( newModel, Cmd.batch [ newCmd, Scroller.lazyLoad ] )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : BaseModel -> Sub Msg
subscriptions { model, viewModel } =
    Sub.batch
        [ case model of
            Home home ->
                Sub.map GotHomeMsg (Home.subscriptions home)

            Gallery gallery ->
                Sub.map GotGalleryMsg (Gallery.subscriptions gallery)

            Album album ->
                Sub.map GotAlbumMsg (Album.subscriptions album)

            Shuffle shuffle ->
                Sub.map GotShuffleMsg (Shuffle.subscriptions shuffle)

            Login login ->
                Sub.map GotLoginMsg (Login.subscriptions login)

            Videos videos ->
                Sub.map GotVideosMsg (Videos.subscriptions videos)

            Diy diy ->
                Sub.map GotDiyMsg (Diy.subscriptions diy)

            Settings settings ->
                Sub.map GotSettingsMsg (Settings.subscriptions settings)

            IdolHome idolHome ->
                Sub.map GotIdolHomeMsg (IdolHome.subscriptions idolHome)

            IdolGallery idolGallery ->
                Sub.map GotIdolGalleryMsg (IdolGallery.subscriptions idolGallery)

            IdolAlbum idolAlbum ->
                Sub.map GotIdolAlbumMsg (IdolAlbum.subscriptions idolAlbum)

            _ ->
                Sub.none
        , Sub.map GotNavbarMsg (Navbar.subscriptions viewModel)
        ]



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
