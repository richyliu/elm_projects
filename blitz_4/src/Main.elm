module Main exposing (main)

import Browser
import Html as Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import String exposing (fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- main


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- model


type alias Column =
    Int


type Player
    = Red
    | Blue
    | Empty


{-| The first layer list is the column, the second layer is the game plays,
starting from the top
-}
type alias Board =
    List (List Player)


type alias Model =
    { board : Board }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board =
            [ [ Empty, Empty, Empty, Empty, Empty, Red ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty, Blue, Empty ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty ]
            ]
      }
    , Cmd.none
    )



-- update


type Msg
    = MakeMove Column Player
    | Batch (List Msg)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MakeMove col player ->
            ( { model | board = model.board }, Cmd.none )

        Batch msgs ->
            List.foldl
                (\curMsg ( curModel, prevCommand ) ->
                    let
                        ( updatedModel, newCommand ) =
                            update curMsg curModel
                    in
                    ( updatedModel, Cmd.batch [ prevCommand, newCommand ] )
                )
                ( model, Cmd.none )
                msgs

        NoOp ->
            ( model, Cmd.none )


isColumnEmpty : Board -> Column -> Bool
isColumnEmpty board col =
    List.all
        (\( colNum, curCol ) ->
            if colNum == col then
                List.any ((==) Empty) curCol

            else
                True
        )
    <|
        List.indexedMap Tuple.pair board



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- view


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "hello" ]
        , svg
            [ width "400"
            , height "400"
            , stroke "none"
            , fill "black"
            ]
            [ viewBoard model
            ]
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    g
        []
        [ viewBoardBackground model
        , viewBoardPieces model
        ]


viewBoardBackground : Model -> Html Msg
viewBoardBackground model =
    rect
        [ fill "hsl(0, 0%, 30%)"
        , x "0"
        , y "0"
        , width "160"
        , height "140"
        ]
        []


viewBoardPieces : Model -> Html Msg
viewBoardPieces model =
    model.board
        |> List.indexedMap
            (\col colPieces ->
                List.indexedMap
                    (\row piece ->
                        circle
                            [ fill
                                (case piece of
                                    Red ->
                                        "red"

                                    Blue ->
                                        "red"

                                    Empty ->
                                        "white"
                                )
                            , cx <| fromInt <| 20 * col
                            , cy <| fromInt <| 20 * row
                            , r "9"
                            ]
                            []
                    )
                    colPieces
            )
        |> List.concat
        |> g [ transform "translate(20 20)" ]
