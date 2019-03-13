module Model exposing (Grid, GridArray, Model, NextMove(..), Player(..), Pos, initialGrid)

import Array exposing (Array)



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
    { firebaseUrl : String }


type alias Model =
    { grid : Grid
    , bigGrid : Grid
    , currentPlayer : Player
    , nextMove : NextMove
    , winner : Player
    , settings : Settings
    }


initialGrid : Int -> Grid
initialGrid size =
    List.map
        (always <| List.map (always Empty) <| List.repeat size 0)
    <|
        List.repeat size 0
