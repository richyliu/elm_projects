module Car exposing (main)

import Browser
import Browser.Events as Events
import Html
import Html.Attributes as HtmlAtt
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Random
import String exposing (fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Course =
    List Rect


course : Course
course =
    [ Rect (Vector 0 0) (Vector 100 50)
    , Rect (Vector 60 50) (Vector 160 100)
    , Rect (Vector 110 100) (Vector 410 170)
    ]


type alias Rect =
    { topLeft : Vector
    , bottomRight : Vector
    }


type alias Vector =
    { x : Float
    , y : Float
    }


type alias Car =
    { pos : Vector
    , velocity : Vector
    , driver : Driver
    }


type alias Driver =
    List Float


type alias SensorReading =
    { direction : Vector
    , isWall : Bool
    }


type alias Model =
    { cars : List Car
    , active : Bool
    , messages : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cars =
            [ { pos = initialPos
              , velocity = Vector 0.31 0.2
              , driver = [ 0, 0, 0, 0, 0 ]
              }
            , { pos = initialPos
              , velocity = Vector 0 -0.2
              , driver = [ 0, 0, 0, 0, 0 ]
              }
            ]
      , active = True
      , messages = []
      }
    , Cmd.none
    )


carSize : Vector
carSize =
    Vector 10 10


initialPos : Vector
initialPos =
    Vector 5 5


drive : Driver -> List SensorReading -> Vector -> Vector
drive driver reading velocity =
    let
        steer =
            reading
                |> List.map2
                    -- TODO: fix algorithm
                    -- d is the driving number, r is the sensor reading
                    (\d r ->
                        if r.isWall then
                            0

                        else
                            d
                    )
                    driver
                |> List.foldr (+) 0

        ( magnitude, theta ) =
            toPolar velocity
    in
    toCartesian ( magnitude, theta + steer )



-- UPDATE


type Msg
    = Move
    | ChangeVelocity Vector
    | Pause
    | Reset
    | Clicked Int Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move ->
            ( { model
                | cars =
                    model.cars
                        |> List.map
                            (\car ->
                                { car | velocity = drive car.driver (readSensors car.pos car.velocity) car.velocity }
                            )
                        |> List.filterMap
                            (\car ->
                                let
                                    moved =
                                        add car.pos car.velocity
                                in
                                if isCarWithinCourse moved then
                                    Just { car | pos = moved }

                                else
                                    Nothing
                            )
              }
            , Cmd.none
            )

        ChangeVelocity vel ->
            ( model, Cmd.none )

        Pause ->
            ( { model | active = not model.active }, Cmd.none )

        Reset ->
            ( { model | cars = List.map (\car -> { car | pos = initialPos }) model.cars }, Cmd.none )

        Clicked x y ->
            ( { model | messages = [ "x: " ++ fromInt x ++ " y: " ++ fromInt y ] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


isCarWithinCourse : Vector -> Bool
isCarWithinCourse pos =
    List.any
        (\rect ->
            (pos.x + carSize.x / 2 >= rect.topLeft.x)
                && (pos.y + carSize.y / 2 >= rect.topLeft.y)
                && (pos.x + carSize.x / 2 <= rect.bottomRight.x)
                && (pos.y + carSize.y / 2 <= rect.bottomRight.y)
        )
        course


sightDistance =
    80


readSensors : Vector -> Vector -> List SensorReading
readSensors pos velocity =
    let
        ( _, theta ) =
            toPolar velocity

        readingAngles =
            [ -pi / 2, -pi / 4, 0, pi / 4, pi / 2 ]

        readingPositions =
            List.map (\angle -> add pos <| toCartesian ( sightDistance, angle + theta )) readingAngles
    in
    List.map
        (\p ->
            { direction = subtract p pos
            , isWall = not <| isCarWithinCourse p
            }
        )
        readingPositions


add : Vector -> Vector -> Vector
add a b =
    Vector (a.x + b.x) (a.y + b.y)


subtract : Vector -> Vector -> Vector
subtract a b =
    Vector (a.x - b.x) (a.y - b.y)


toPolar : Vector -> ( Float, Float )
toPolar v =
    ( getLength v, atan2 v.y v.x )


toCartesian : ( Float, Float ) -> Vector
toCartesian ( r, theta ) =
    Vector (r * cos theta) (r * sin theta)


getLength : Vector -> Float
getLength v =
    sqrt <| v.x ^ 2 + v.y ^ 2



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.active then
        Sub.batch
            [ Time.every 10 (\_ -> Move)
            , Events.onClick decodeMouse
            ]

    else
        Events.onClick decodeMouse


decodeMouse : Decode.Decoder Msg
decodeMouse =
    Decode.succeed Clicked
        |> required "offsetX" Decode.int
        |> required "offsetY" Decode.int



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div [] <|
            List.map
                (\car ->
                    Html.p [] [ Html.text <| toString car.pos ]
                )
                model.cars
        , Html.div [] <|
            List.map
                (\message ->
                    Html.p [] [ Html.text message ]
                )
                model.messages
        , Html.button [ onClick Move ] [ Html.text "Move" ]
        , Html.button [ onClick Pause ] [ Html.text "pause/resume" ]
        , Html.button [ onClick Reset ] [ Html.text "Reset" ]
        , Html.div []
            [ Html.div
                [ HtmlAtt.style "margin" "10px"
                , HtmlAtt.style "border" "1px solid black"
                , HtmlAtt.style "display" "inline-block"
                ]
                [ svg
                    [ width "500"
                    , height "500"
                    , fill "white"
                    , strokeWidth "0"
                    ]
                    (List.concat
                        [ [ rect [ x "0", y "0", width "500", height "500", fill "black" ] [] ]
                        , viewCourse course
                        , List.concat <| List.map viewCar model.cars
                        ]
                    )
                ]
            ]
        ]


viewCar : Car -> List (Svg Msg)
viewCar { pos, velocity } =
    [ rect
        [ x <| toStr pos.x
        , y <| toStr pos.y
        , width <| toStr carSize.x
        , height <| toStr carSize.y
        , fill "red"
        ]
        []
    , line
        (let
            -- car's current center x and y position
            car =
                { x = pos.x + carSize.x / 2
                , y = pos.y + carSize.y / 2
                }
         in
         [ x1 <| toStr car.x
         , y1 <| toStr car.y
         , x2 <| toStr (car.x + sightDistance * velocity.x)
         , y2 <| toStr (car.y + sightDistance * velocity.y)
         , strokeWidth "2"
         , stroke "blue"
         ]
        )
        []
    ]


viewCourse : List Rect -> List (Svg Msg)
viewCourse =
    List.map
        (\r ->
            rect
                [ x <| toStr r.topLeft.x
                , y <| toStr r.topLeft.y
                , width <| toStr (r.bottomRight.x - r.topLeft.x)
                , height <| toStr (r.bottomRight.y - r.topLeft.y)
                ]
                []
        )


toStr : Float -> String
toStr =
    String.fromFloat


toString : Vector -> String
toString v =
    let
        x =
            toFloat (round (v.x * 100)) / 100

        y =
            toFloat (round (v.y * 100)) / 100
    in
    "(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"
