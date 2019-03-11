module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (fromFloat, fromInt)
import String.Interpolate exposing (interpolate)
import Task
import Time



-- Main method


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


{-| number of ms for each tick
-}
tickLen : Int
tickLen =
    100


{-| Source generator. Tracks prices of generator
-}
type alias SourceGenerator =
    -- how many cookies generated per tick
    { cookies : Int

    -- interval to generate cookie
    , interval : Int

    -- current price of generator
    , price : Int

    -- number of generators already bought
    , num : Int
    }


sourceGenerators : List SourceGenerator
sourceGenerators =
    [ { cookies = 1
      , interval = 10
      , price = 10
      , num = 0
      }
    ]


{-| Actual cookie generator instance
-}
type alias CookieGenerator =
    -- num cookies generated per tick
    { cookies : Int

    -- how many ticks to generate cookies
    , interval : Int

    -- starting time
    , start : Int
    }


type alias Model =
    -- Current cookies you have
    { cookies : Int

    -- active generators
    , generators : List CookieGenerator

    -- available generators to buy
    , sources : List SourceGenerator

    -- current time
    , time : Int

    -- cookies generated per tick
    , cpt : Float

    -- whether the clock is running or not
    , active : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cookies = 20
      , generators = []
      , sources = sourceGenerators
      , cpt = 0
      , time = 0
      , active = True
      }
    , Cmd.none
    )



-- Update


type Msg
    = AddCookie
    | Tick Int
    | AddGenerator SourceGenerator
    | ToggleActive


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddCookie ->
            ( { model | cookies = model.cookies + 1 }, Cmd.none )

        Tick time ->
            ( { model
                | time = time
                , cookies =
                    model.cookies
                        + List.foldr
                            (\g total ->
                                if
                                    (tickLen
                                        |> divide (time - g.start)
                                        |> round
                                        |> remainderBy g.interval
                                    )
                                        == 0
                                then
                                    total + g.cookies

                                else
                                    total
                            )
                            0
                            model.generators
              }
            , Cmd.none
            )

        AddGenerator source ->
            ( { model
                | cookies = model.cookies - source.price
                , generators =
                    { cookies = source.cookies
                    , interval = source.interval
                    , start = model.time
                    }
                        :: model.generators
                , sources =
                    List.map
                        (\s ->
                            if s == source then
                                { s | num = s.num + 1 }

                            else
                                s
                        )
                        model.sources
                , cpt = model.cpt + divide source.cookies source.interval
              }
            , Cmd.none
            )

        ToggleActive ->
            ( { model | active = not model.active }, Cmd.none )


{-| Divides 2 integers into a float

    divide 3 10 == 0.3

-}
divide : Int -> Int -> Float
divide a b =
    toFloat a / toFloat b



-- View


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddCookie ] [ text "Big cookies" ]
        , button [ onClick ToggleActive ]
            [ text
                (if model.active then
                    "Turn off"

                 else
                    "Turn on"
                )
            ]
        , div [] <|
            List.map
                (\s ->
                    button
                        [ onClick <| AddGenerator s
                        , disabled <| s.price > model.cookies
                        ]
                        [ text <|
                            interpolate "Generates {0} cookies every {1} tick at a price of {2} (you have {3})"
                                [ fromInt s.cookies
                                , fromInt s.interval
                                , fromInt s.price
                                , fromInt s.num
                                ]
                        ]
                )
                model.sources
        , div []
            [ p []
                [ text <|
                    "generating at "
                        ++ (model.cpt
                                -- Ends up multiplying by 10 b/c tick is 100ms
                                |> (*) 10000
                                |> round
                                |> toFloat
                                |> (\a -> a / 1000)
                                |> fromFloat
                           )
                        ++ " cookies/sec"
                ]
            ]
        , p []
            [ text <|
                "You have: "
                    ++ fromInt model.cookies
                    ++ " cookies"
            ]
        , p [] [ text <| "time: " ++ fromInt model.time ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.active then
        Time.every (toFloat tickLen) (Time.posixToMillis >> Tick)

    else
        Sub.none
