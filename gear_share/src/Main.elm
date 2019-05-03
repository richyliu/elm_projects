module Main exposing (init, main, subscriptions)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, a, div, section, span, text)
import Html.Attributes exposing (class, href)
import Pages.Add as Add
import Pages.Edit as Edit
import Pages.List as List
import Routes exposing (Route)
import Shared exposing (..)
import Url exposing (Url)


type alias Model =
    { flags : Flags
    , navKey : Key
    , route : Route
    , page : Page
    }


type Page
    = PageNone
    | PageList List.Model
    | PageEdit Edit.Model
    | PageAdd Add.Model


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | ListMsg List.Msg
    | EditMsg Edit.Msg
    | AddMsg Add.Msg


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url navKey =
    loadCurrentPage
        ( { flags = flags
          , navKey = navKey
          , route = Routes.parseUrl url
          , page = PageNone
          }
        , Cmd.none
        )


loadCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadCurrentPage ( model, cmd ) =
    let
        ( page, newCmd ) =
            case model.route of
                Routes.ItemsRoute ->
                    let
                        ( pageModel, pageCmd ) =
                            List.init model.flags
                    in
                    ( PageList pageModel, Cmd.map ListMsg pageCmd )

                Routes.ItemRoute itemId ->
                    let
                        ( pageModel, pageCmd ) =
                            Edit.init model.flags itemId
                    in
                    ( PageEdit pageModel, Cmd.map EditMsg pageCmd )

                Routes.AddRoute ->
                    let
                        ( pageModel, pageCmd ) =
                            Add.init model.flags
                    in
                    ( PageAdd pageModel, Cmd.map AddMsg pageCmd )

                Routes.NotFoundRoute ->
                    ( PageNone, Cmd.none )
    in
    ( { model | page = page }, Cmd.batch [ cmd, newCmd ] )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        PageList pageModel ->
            Sub.map ListMsg (List.subscriptions pageModel)

        PageEdit pageModel ->
            Sub.map EditMsg (Edit.subscriptions pageModel)

        PageAdd pageModel ->
            Sub.map AddMsg (Add.subscriptions pageModel)

        PageNone ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( OnUrlRequest urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( OnUrlChange url, _ ) ->
            let
                newRoute =
                    Routes.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> loadCurrentPage

        ( ListMsg subMsg, PageList pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    List.update subMsg pageModel
            in
            ( { model | page = PageList newPageModel }
            , Cmd.map ListMsg newCmd
            )

        ( ListMsg subMsg, _ ) ->
            ( model, Cmd.none )

        ( EditMsg subMsg, PageEdit pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Edit.update model.flags subMsg pageModel
            in
            ( { model | page = PageEdit newPageModel }
            , Cmd.map EditMsg newCmd
            )

        ( EditMsg subMsg, _ ) ->
            ( model, Cmd.none )

        ( AddMsg subMsg, PageAdd pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Add.update model.flags subMsg pageModel
            in
            ( { model | page = PageAdd newPageModel }
            , Cmd.map AddMsg newCmd
            )

        ( AddMsg subMsg, _ ) ->
            ( model, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }



-- VIEWS


view : Model -> Browser.Document Msg
view model =
    { title = "Gear share"
    , body = [ currentPage model ]
    }


currentPage : Model -> Html Msg
currentPage model =
    let
        page =
            case model.page of
                PageList pageModel ->
                    List.view pageModel
                        |> Html.map ListMsg

                PageEdit pageModel ->
                    Edit.view pageModel
                        |> Html.map EditMsg

                PageAdd pageModel ->
                    Add.view pageModel
                        |> Html.map AddMsg

                PageNone ->
                    viewNotFound
    in
    section []
        [ viewNav model
        , page
        ]


viewNav : Model -> Html Msg
viewNav model =
    let
        links : List (Html Msg)
        links =
            case model.route of
                Routes.ItemsRoute ->
                    [ span [ class "font-bold text-white m-2" ] [ text "Items" ], addLink ]

                Routes.ItemRoute _ ->
                    [ linkToList, addLink ]

                Routes.AddRoute ->
                    [ linkToList ]

                Routes.NotFoundRoute ->
                    [ linkToList, addLink ]

        linkToList : Html Msg
        linkToList =
            a [ href Routes.itemsPath, class "text-white m-2" ] [ text "List" ]

        addLink : Html Msg
        addLink =
            a [ href Routes.addPath, class "text-white m-2" ] [ text "Add" ]
    in
    div
        [ class "mb-2 text-white bg-black p-4" ]
        links


viewNotFound : Html Msg
viewNotFound =
    div [] [ text "Not found" ]
