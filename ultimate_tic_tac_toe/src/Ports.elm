port module Ports exposing (receiveAddMove, receiveFirstPlayer, setGameId)

import Json.Decode exposing (Value)


port receiveAddMove : (Value -> msg) -> Sub msg


port receiveFirstPlayer : (String -> msg) -> Sub msg


port setGameId : Int -> Cmd msg
