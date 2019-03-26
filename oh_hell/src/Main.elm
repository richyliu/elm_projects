module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import String exposing (fromInt)



-- main


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- model


type alias Player =
    String


type State
    = Home
    | EnterPlayerName
    | MainGame


type alias Score =
    { tricks : Int
    , made : Int
    , score : Int
    }


type alias ScoreBoard =
    List
        { player : Player
        , score : Score
        }


type alias Model =
    { state : State
    , players : List Player
    , text : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = EnterPlayerName
      , players = [ "", "", "", "" ]
      , text = ""
      }
    , Cmd.none
    )



-- update


type Msg
    = ChangeState State
    | UpdatePlayer Int Player
    | RemoveEmptyPlayers
    | Batch (List Msg)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeState state ->
            ( { model | state = state }, Cmd.none )

        UpdatePlayer index newPlayer ->
            ( { model
                | players =
                    List.indexedMap
                        (\i p ->
                            if i == index then
                                newPlayer

                            else
                                p
                        )
                        model.players
              }
            , Cmd.none
            )

        RemoveEmptyPlayers ->
            ( { model | players = List.filter (String.length >> (<) 0) model.players }, Cmd.none )

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



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- view


view : Model -> Html Msg
view model =
    case model.state of
        Home ->
            viewHome model

        EnterPlayerName ->
            viewEnterPlayerName model

        MainGame ->
            viewMainGame model


viewHome : Model -> Html Msg
viewHome model =
    div []
        [ button [ onClick (ChangeState EnterPlayerName) ] [ text "Begin" ]
        ]


viewEnterPlayerName : Model -> Html Msg
viewEnterPlayerName model =
    div [] <|
        List.indexedMap
            (\i p ->
                div []
                    [ label [] [ text <| "Player " ++ fromInt i ]
                    , input [ onInput <| UpdatePlayer i ] []
                    , span [] [ text p ]
                    ]
            )
            model.players
            ++ [ div []
                    [ button [ onClick <| ChangeState Home ] [ text "Back" ]
                    , button [ onClick <| Batch [ RemoveEmptyPlayers, ChangeState MainGame ] ] [ text "Next" ]
                    ]
               ]


viewMainGame : Model -> Html Msg
viewMainGame model =
    div [] [ text "main game" ]
