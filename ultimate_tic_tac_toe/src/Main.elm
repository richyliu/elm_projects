module Main exposing (main)

import Browser
import Html
import Html.Attributes as Attr
import String exposing (fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Player
    = Red
    | Blue
    | Empty


type alias Grid =
    List (List Player)


type alias Pos =
    { x : Int
    , y : Int
    }


type NextMove
    = Any
    | Place Pos


type alias Model =
    { grid : Grid
    , currentPlayer : Player
    , nextMove : NextMove
    }


initialGrid : Grid
initialGrid =
    List.map
        (always <| List.map (always Empty) <| List.repeat 9 0)
    <|
        List.repeat 9 0


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = initialGrid
      , currentPlayer = Red
      , nextMove = Any
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = MakeMove Pos
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MakeMove pos ->
            ( if isValidMove model.nextMove pos then
                { model
                    | grid = setPos model.grid pos model.currentPlayer
                    , nextMove = getMoveDirection pos
                    , currentPlayer =
                        case model.currentPlayer of
                            Red ->
                                Blue

                            Blue ->
                                Red

                            Empty ->
                                Red
                }

              else
                model
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


isValidMove : NextMove -> Pos -> Bool
isValidMove nextMove posIn =
    let
        pos =
            Pos (posIn.x // 3) (posIn.y // 3)
    in
    case nextMove of
        Any ->
            True

        Place validPos ->
            validPos == pos


{-| Get the direction the next player has to move in after this move
-}
getMoveDirection : Pos -> NextMove
getMoveDirection pos =
    let
        mod3 =
            remainderBy 3
    in
    Place <| Pos (mod3 pos.x) (mod3 pos.y)


setPos : Grid -> Pos -> Player -> Grid
setPos grid pos player =
    List.indexedMap
        (\y row ->
            List.indexedMap
                (\x p ->
                    if x == pos.x && y == pos.y then
                        player

                    else
                        p
                )
                row
        )
        grid



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Attr.style "padding" "10px"
        , Attr.style "border" "1px dotted black"
        , Attr.style "display" "inline-block"
        ]
        [ svg
            [ width "400"
            , height "400"
            , stroke "none"
            , fill "black"
            , strokeWidth "2"
            ]
            [ g [ transform "translate(10, 10)" ] <|
                List.concat
                    [ viewGrid model
                    , viewGridLines model
                    , viewWhereToMoveNext model
                    ]
            ]
        ]


squareSize : Int
squareSize =
    20


squarePadding : Int
squarePadding =
    6


viewWhereToMoveNext : Model -> List (Svg Msg)
viewWhereToMoveNext model =
    case model.nextMove of
        Any ->
            []

        Place pos ->
            [ rect
                [ x <| fromInt <| pos.x * 3 * (squareSize + squarePadding) - squarePadding // 2
                , y <| fromInt <| pos.y * 3 * (squareSize + squarePadding) - squarePadding // 2
                , width <| fromInt <| 3 * (squareSize + squarePadding)
                , height <| fromInt <| 3 * (squareSize + squarePadding)
                , fill
                    (case model.currentPlayer of
                        Blue ->
                            "blue"

                        Red ->
                            "red"

                        Empty ->
                            "white"
                    )
                , stroke "none"
                , opacity "0.2"
                , pointerEvents "none"
                ]
                []
            ]


viewGrid : Model -> List (Svg Msg)
viewGrid model =
    List.concat <|
        mapGrid
            (\pos player ->
                case player of
                    Blue ->
                        [ line
                            [ x1 <| fromInt <| pos.x * (squareSize + squarePadding)
                            , y1 <| fromInt <| pos.y * (squareSize + squarePadding)
                            , x2 <| fromInt <| pos.x * (squareSize + squarePadding) + squareSize
                            , y2 <| fromInt <| pos.y * (squareSize + squarePadding) + squareSize
                            , stroke "blue"
                            ]
                            []
                        , line
                            [ x1 <| fromInt <| pos.x * (squareSize + squarePadding) + squareSize
                            , y1 <| fromInt <| pos.y * (squareSize + squarePadding)
                            , x2 <| fromInt <| pos.x * (squareSize + squarePadding)
                            , y2 <| fromInt <| pos.y * (squareSize + squarePadding) + squareSize
                            , stroke "blue"
                            ]
                            []
                        ]

                    Red ->
                        [ circle
                            [ cx <| fromInt <| pos.x * (squareSize + squarePadding) + squareSize // 2
                            , cy <| fromInt <| pos.y * (squareSize + squarePadding) + squareSize // 2
                            , r <| fromInt <| squareSize // 2
                            , stroke "red"
                            , fill "transparent"
                            ]
                            []
                        ]

                    Empty ->
                        [ rect
                            [ x <| fromInt <| pos.x * (squareSize + squarePadding)
                            , y <| fromInt <| pos.y * (squareSize + squarePadding)
                            , width <| fromInt squareSize
                            , height <| fromInt squareSize
                            , stroke "none"
                            , fill "transparent"
                            , onClick <| MakeMove pos
                            ]
                            []
                        ]
            )
            model.grid


{-| Specifically designed for a 9x9 square. This will draw a thicker line every
3 lines
-}
viewGridLines : Model -> List (Svg Msg)
viewGridLines model =
    let
        rows =
            List.length model.grid

        cols =
            Maybe.withDefault 0 <| List.maximum <| List.map List.length model.grid
    in
    List.map
        (\row ->
            line
                [ x1 "0"
                , y1 <| fromInt <| (squareSize + squarePadding) * row - squarePadding // 2
                , x2 <| fromInt <| (squareSize + squarePadding) * cols
                , y2 <| fromInt <| (squareSize + squarePadding) * row - squarePadding // 2
                , stroke
                    (if remainderBy 3 row == 0 then
                        "black"

                     else
                        "darkgray"
                    )
                ]
                []
        )
        (List.range 1 (rows - 1))
        ++ List.map
            (\col ->
                line
                    [ x1 <| fromInt <| (squareSize + squarePadding) * col - squarePadding // 2
                    , y1 "0"
                    , x2 <| fromInt <| (squareSize + squarePadding) * col - squarePadding // 2
                    , y2 <| fromInt <| (squareSize + squarePadding) * rows
                    , stroke
                        (if remainderBy 3 col == 0 then
                            "black"

                         else
                            "darkgray"
                        )
                    ]
                    []
            )
            (List.range 1 (cols - 1))


mapGrid : (Pos -> Player -> a) -> Grid -> List a
mapGrid mapper grid =
    List.concat <|
        List.indexedMap
            (\y row ->
                List.indexedMap
                    (\x player -> mapper (Pos x y) player)
                    row
            )
            grid
