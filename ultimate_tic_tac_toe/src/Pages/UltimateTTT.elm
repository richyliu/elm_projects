module Pages.UltimateTTT exposing (Model, Msg(..), init, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Ports exposing (..)
import Random
import String exposing (fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)



-- MODEL


type Player
    = Red
    | Blue
    | Empty


type alias Grid =
    List (List Player)


type alias GridArray =
    Array (Array Player)


type alias Pos =
    { x : Int
    , y : Int
    }


type NextMove
    = Any
    | Place Pos


type alias Settings =
    { firebaseRoot : String }


type alias Model =
    { grid : Grid
    , bigGrid : Grid
    , currentPlayer : Player
    , player : Player
    , nextMove : NextMove
    , winner : Player
    , gameId : Maybe String
    , settings : Settings
    , yourLink : String
    , friendLink : String
    }


initialGrid : Int -> Grid
initialGrid size =
    let
        manyZeros =
            List.repeat size 0
    in
    List.map
        (always <| List.map (always Empty) manyZeros)
        manyZeros


init : String -> ( Model, Cmd Msg )
init gameIdWithPlayer =
    let
        spacePos =
            gameIdWithPlayer
                |> String.indexes " "
                |> List.head

        ( gameId, player ) =
            case spacePos of
                Just pos ->
                    if String.length gameIdWithPlayer > 3 then
                        ( if pos == 0 then
                            Nothing

                          else
                            Just <| String.slice 0 pos gameIdWithPlayer
                        , case String.slice (pos + 1) (String.length gameIdWithPlayer) gameIdWithPlayer of
                            "red" ->
                                Red

                            "blue" ->
                                Blue

                            _ ->
                                -- TODO tell user when erroneous data occurs
                                Red
                        )

                    else
                        ( Nothing, Empty )

                Nothing ->
                    ( Nothing, Empty )
    in
    ( { nextMove = Any
      , grid = initialGrid 9
      , bigGrid = initialGrid 3
      , currentPlayer = Red
      , player = player
      , winner = Empty
      , gameId = gameId
      , yourLink = ""
      , friendLink = ""
      , settings =
            { firebaseRoot = "https://main-fe047.firebaseio.com/ultimatettt/"
            }
      }
    , Cmd.none
    )


getFirebaseUrl : Model -> String -> String
getFirebaseUrl model gameId =
    model.settings.firebaseRoot ++ gameId ++ ".json"



-- UPDATE


type Msg
    = MakeMove Pos
    | MakeMoveRequest Pos
    | ReceiveRandom Int
    | NewGame
    | SetFirstPlayer Player
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MakeMove pos ->
            if model.winner == Empty && isValidMove model.nextMove pos then
                ( let
                    nextGrid =
                        setPos model.grid pos model.currentPlayer

                    nextBigGrid =
                        checkForWins nextGrid model.bigGrid model.currentPlayer
                  in
                  { model
                    | grid = nextGrid
                    , nextMove =
                        let
                            possibleNextMove =
                                getMoveDirection pos
                        in
                        case possibleNextMove of
                            Any ->
                                Any

                            Place move ->
                                if isInBigGrid nextBigGrid move then
                                    Place move

                                else
                                    Any
                    , currentPlayer =
                        case model.currentPlayer of
                            Red ->
                                Blue

                            Blue ->
                                Red

                            Empty ->
                                Red
                    , bigGrid = nextBigGrid
                    , winner = checkWinSimple (toGridArray nextBigGrid)
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        MakeMoveRequest pos ->
            ( model
            , if
                (model.winner == Empty)
                    && isValidMove model.nextMove pos
                    && (model.currentPlayer == model.player)
              then
                case model.gameId of
                    Just gameId ->
                        addMoveRequest (getFirebaseUrl model gameId) pos model.currentPlayer

                    Nothing ->
                        Cmd.none

              else
                Cmd.none
            )

        ReceiveRandom num ->
            let
                id =
                    String.fromInt num

                firstPlayer =
                    "red"

                otherPlayer =
                    "blue"
            in
            ( { model
                | yourLink = "/?id=" ++ id ++ "&player=" ++ firstPlayer
                , friendLink = "/?id=" ++ id ++ "&player=" ++ otherPlayer
              }
            , Http.request
                { method = "PUT"
                , headers = []
                , url = getFirebaseUrl model id
                , body =
                    Http.jsonBody
                        (Encode.object
                            [ ( "firstPlayer", Encode.string firstPlayer )
                            ]
                        )

                -- NoOp for now... TODO
                , expect = Http.expectWhatever <| always NoOp
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        SetFirstPlayer player ->
            ( { model | currentPlayer = player }, Cmd.none )

        NewGame ->
            ( model, Random.generate ReceiveRandom <| Random.int 1 (floor 1.0e9) )

        NoOp ->
            ( model, Cmd.none )


addMoveRequest : String -> Pos -> Player -> Cmd Msg
addMoveRequest url pos player =
    Http.post
        { url = url
        , body =
            ( pos, player )
                |> moveEncoder
                |> Http.jsonBody
        , expect = Http.expectWhatever <| always NoOp
        }


moveEncoder : ( Pos, Player ) -> Encode.Value
moveEncoder ( pos, player ) =
    Encode.object
        [ ( "pos", Encode.string <| fromInt pos.x ++ " " ++ fromInt pos.y )
        , ( "player"
          , Encode.string
                (case player of
                    Red ->
                        "Red"

                    Blue ->
                        "Blue"

                    Empty ->
                        "Empty"
                )
          )
        ]


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


{-| Checks if the next move can is possible in the big grid
-}
isInBigGrid : Grid -> Pos -> Bool
isInBigGrid grid move =
    grid
        |> List.indexedMap
            (\y ->
                List.indexedMap
                    (\x player ->
                        case player of
                            Empty ->
                                True

                            _ ->
                                (x /= move.x) || (y /= move.y)
                    )
            )
        |> List.concat
        |> List.all identity


{-| Checks for wins within each 3x3 square in the larger 9x9 square to be
applied to the big square and reture the updated big square
-}
checkForWins : Grid -> Grid -> Player -> Grid
checkForWins grid bigGrid player =
    List.indexedMap
        (\y row ->
            List.indexedMap
                (\x alreadyWon ->
                    case alreadyWon of
                        Empty ->
                            check3x3Win grid <| Pos x y

                        p ->
                            p
                )
                row
        )
        bigGrid


toGridArray : Grid -> GridArray
toGridArray grid =
    grid
        |> List.map Array.fromList
        |> Array.fromList


{-| Check for a win within the pos (of the bigger grid)
-}
check3x3Win : Grid -> Pos -> Player
check3x3Win grid pos =
    grid
        |> toGridArray
        |> Array.map (Array.slice (pos.x * 3) (pos.x * 3 + 3))
        |> Array.slice (pos.y * 3) (pos.y * 3 + 3)
        |> checkWinSimple


{-| Check for wins in a 3x3 grid. Returns Empty for no winner
-}
checkWinSimple : GridArray -> Player
checkWinSimple grid =
    let
        get : Int -> Int -> Maybe Player
        get x y =
            Array.get y grid |> Maybe.andThen (Array.get x)
    in
    if
        ((get 0 0 == get 0 1 && get 0 1 == get 0 2)
            || (get 0 0 == get 1 0 && get 1 0 == get 2 0)
        )
            && (get 0 0 /= Just Empty)
    then
        Maybe.withDefault Empty <| get 0 0

    else if
        ((get 0 0 == get 1 1 && get 1 1 == get 2 2)
            || (get 1 0 == get 1 1 && get 1 1 == get 1 2)
            || (get 0 2 == get 1 1 && get 1 1 == get 2 0)
            || (get 0 1 == get 1 1 && get 1 1 == get 2 1)
        )
            && (get 1 1 /= Just Empty)
    then
        Maybe.withDefault Empty <| get 1 1

    else if
        ((get 2 0 == get 2 1 && get 2 1 == get 2 2)
            || (get 0 2 == get 1 2 && get 1 2 == get 2 2)
        )
            && (get 2 2 /= Just Empty)
    then
        Maybe.withDefault Empty <| get 2 2

    else
        Empty


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
{- JS side code to listen to firbase updates:
   let source = new EventSource('https://main-fe047.firebaseio.com/ultimatettt.json');
   source.addEventListener('put', console.log);
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ addedMove
            (\( pos, player ) ->
                case player of
                    Empty ->
                        NoOp

                    _ ->
                        MakeMove pos
            )
        , receiveFirstPlayer (decodeFirstPlayer >> SetFirstPlayer)
        ]


decodeFirstPlayer : String -> Player
decodeFirstPlayer playerStr =
    case playerStr of
        "red" ->
            Red

        "blue" ->
            Blue

        _ ->
            -- TODO tell user when erroneous data occurs
            Red


addedMove : (( Pos, Player ) -> msg) -> Sub msg
addedMove callback =
    receiveAddMove (decodeMove >> callback)


decodeMove : Decode.Value -> ( Pos, Player )
decodeMove val =
    let
        result =
            Decode.decodeValue
                (Decode.map2 (\a b -> ( a, b ))
                    (Decode.field "pos" posDecoder)
                    (Decode.field "player" playerDecoder)
                )
                val
    in
    case result of
        Ok value ->
            value

        Err err ->
            ( Pos 0 0, Empty )


playerDecoder : Decode.Decoder Player
playerDecoder =
    Decode.andThen
        (\str ->
            case str of
                "Red" ->
                    Decode.succeed Red

                "Blue" ->
                    Decode.succeed Blue

                unknown ->
                    Decode.fail <| "Incorrect option: " ++ unknown
        )
        Decode.string


posDecoder : Decode.Decoder Pos
posDecoder =
    Decode.andThen
        (\str ->
            let
                parts =
                    str
                        |> String.split " "
                        |> Array.fromList

                x =
                    Array.get 0 parts
                        |> Maybe.andThen String.toInt

                y =
                    Array.get 1 parts
                        |> Maybe.andThen String.toInt
            in
            case ( x, y ) of
                ( Just theX, Just theY ) ->
                    Decode.succeed <| Pos theX theY

                _ ->
                    Decode.fail "error"
        )
        Decode.string



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Page"
    , body = [ viewBase model ]
    }


viewBase : Model -> Html Msg
viewBase model =
    Html.div
        [ Attr.style "margin" "10px"
        , Attr.style "display" "inline-block"
        , Attr.style "font-family" "Verdana"
        ]
    <|
        viewWelcome
            :: (if String.length model.yourLink > 0 && String.length model.yourLink > 0 then
                    [ viewLinks model ]

                else
                    case model.gameId of
                        Just _ ->
                            [ viewStatus model
                            , viewWinner model
                            , svg
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
                                        , viewCompletedBigSquares model
                                        ]
                                ]
                            ]

                        Nothing ->
                            []
               )


viewWelcome : Html Msg
viewWelcome =
    Html.div [ Attr.style "margin-bottom" "40px" ]
        [ Html.h1
            [ Attr.style "font-family" "Georgia" ]
            [ Html.text "Ultimate tic tac toe" ]
        , Html.p []
            [ Html.text <|
                "This game is similar to normal tic tac toe, except "
                    ++ "that each smaller 3x3 region is it's own game of tic tac "
                    ++ "toe. The winner of each 3x3 region corresponds to a move "
                    ++ "on the larger board, and the winner of the larger board "
                    ++ "wins the game. After each move your opponent may only "
                    ++ "move in the direction of your move (the highlighted "
                    ++ "region explains this)."
            ]
        , Html.div []
            [ Html.button [ Events.onClick NewGame ] [ Html.text <| "new game" ]
            ]
        ]


viewLinks : Model -> Html Msg
viewLinks model =
    Html.div
        []
        [ Html.p []
            [ Html.text "Go to this link to start your game: "
            , Html.a [ Attr.href model.yourLink ] [ Html.text model.yourLink ]
            ]
        , Html.p []
            [ Html.text "And send this link to your friend: "
            , Html.a [ Attr.href model.friendLink ] [ Html.text model.friendLink ]
            ]
        ]


playerString : Player -> String
playerString player =
    case player of
        Red ->
            "red"

        Blue ->
            "blue"

        Empty ->
            "no one"


viewStatus : Model -> Html Msg
viewStatus model =
    Html.div []
        [ Html.p []
            [ Html.text <|
                "You are "
                    ++ playerString model.player
            ]
        , Html.p []
            [ Html.text "It is "
            , if model.player == model.currentPlayer then
                Html.mark [] [ Html.text " YOUR TURN." ]

              else
                Html.text " not your turn."
            , Html.text " You have to make a move in the colored region."
            ]
        ]


viewWinner : Model -> Html Msg
viewWinner model =
    case model.winner of
        Empty ->
            Html.div [] []

        player ->
            Html.p
                [ Attr.style "position" "relative"
                , Attr.style "background" "white"
                , Attr.style "margin" "0px"
                , Attr.style "padding-top" "10px"
                , Attr.style "height" "30px"
                , Attr.style "top" "140px"
                ]
                [ Html.text <|
                    "Congratulations for winning: "
                        ++ (if player == Blue then
                                "blue"

                            else
                                "red"
                           )
                ]


squareSize : Int
squareSize =
    30


squarePadding : Int
squarePadding =
    8


makeMoveOpacity : String
makeMoveOpacity =
    "0.15"


viewWhereToMoveNext : Model -> List (Svg Msg)
viewWhereToMoveNext model =
    case model.nextMove of
        Any ->
            [ rect
                [ x "0"
                , y "0"
                , width <| fromInt <| 9 * (squareSize + squarePadding)
                , height <| fromInt <| 9 * (squareSize + squarePadding)
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
                , opacity makeMoveOpacity
                , pointerEvents "none"
                ]
                []
            ]

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
                , opacity makeMoveOpacity
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
                            , onClick <| MakeMoveRequest pos
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


viewCompletedBigSquares : Model -> List (Svg Msg)
viewCompletedBigSquares model =
    List.concat <|
        mapGrid
            (\pos player ->
                case player of
                    Blue ->
                        [ line
                            [ x1 <| fromInt <| pos.x * 3 * (squareSize + squarePadding)
                            , y1 <| fromInt <| pos.y * 3 * (squareSize + squarePadding)
                            , x2 <| fromInt <| (pos.x + 1) * 3 * (squareSize + squarePadding) - squarePadding
                            , y2 <| fromInt <| (pos.y + 1) * 3 * (squareSize + squarePadding) - squarePadding
                            , stroke "blue"
                            , strokeWidth "3"
                            ]
                            []
                        , line
                            [ x1 <| fromInt <| (pos.x + 1) * 3 * (squareSize + squarePadding) - squarePadding
                            , y1 <| fromInt <| pos.y * 3 * (squareSize + squarePadding)
                            , x2 <| fromInt <| pos.x * 3 * (squareSize + squarePadding)
                            , y2 <| fromInt <| (pos.y + 1) * 3 * (squareSize + squarePadding) - squarePadding
                            , stroke "blue"
                            , strokeWidth "3"
                            ]
                            []
                        ]

                    Red ->
                        [ circle
                            [ cx <| fromInt <| (pos.x * 3) * (squareSize + squarePadding) + squareSize * 2 - squarePadding // 2
                            , cy <| fromInt <| (pos.y * 3) * (squareSize + squarePadding) + squareSize * 2 - squarePadding // 2
                            , r <| fromInt <| (squareSize + squarePadding) * 3 // 2 - squarePadding
                            , stroke "red"
                            , fill "transparent"
                            , strokeWidth "3"
                            ]
                            []
                        ]

                    Empty ->
                        []
            )
            model.bigGrid
