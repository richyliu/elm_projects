port module Ports exposing (addedMove)

import Array
import Json.Decode as Decode
import Model exposing (..)


port receiveAddMove : (Decode.Value -> msg) -> Sub msg


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
