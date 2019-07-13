module Pages.Login exposing (Model, Msg, init, subscriptions, toNavKey, update, view)

{-| Force the user to login
-}

import Auth
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Route
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , nums : List Int
    }


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( { navKey = navKey
      , nums = []
      }
    , Cmd.none
    )


view : Model -> PageContent Msg
view model =
    { title = "Login"
    , iconButtons = []
    , content = viewLogin model
    }


viewLogin : Model -> Html Msg
viewLogin model =
    let
        row =
            div [ class "flex flex-row flex-auto" ]

        numButton num =
            button
                [ class "flex-auto m-2 p-4 rounded-lg bg-gray-400 text-4xl"
                , onClick <| TypedNum num
                , attribute "data-number" <| String.fromInt num
                ]
                [ text <| String.fromInt num ]
    in
    div
        [ class "fixed top-0 z-50 w-screen h-screen bg-white overflow-hidden" ]
        [ p [ class "mt-20 text-4xl text-center" ]
            [ model.nums
                |> List.map String.fromInt
                |> String.join " "
                |> text
            ]
        , div [ class "flex flex-col-reverse fixed bottom-0 w-full" ]
            [ row
                [ numButton 7
                , numButton 8
                , numButton 9
                ]
            , row
                [ numButton 4
                , numButton 5
                , numButton 6
                ]
            , row
                [ numButton 1
                , numButton 2
                , numButton 3
                ]
            ]
        ]



-- UPDATE


type Msg
    = TypedNum Int


passwordLength : Int
passwordLength =
    4


checkPassword : List Int -> Bool
checkPassword nums =
    case nums of
        [ a, b, c, d ] ->
            (a + b + c + d == 22)
                && (a - d == 5 * (c - b))
                && (remainderBy a c == remainderBy a d)
                && (d - b == c - a)

        _ ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TypedNum num ->
            let
                newNums =
                    model.nums ++ [ num ]
            in
            if List.length newNums == passwordLength then
                if checkPassword newNums then
                    ( model, Cmd.batch [ Auth.login, Route.pushUrl model.navKey Route.Home ] )

                else
                    ( { model | nums = [] }, Cmd.none )

            else
                ( { model | nums = newNums }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toNavKey : Model -> Nav.Key
toNavKey model =
    model.navKey
