module Pages.Settings exposing (Model, Msg, init, subscriptions, toNavKey, update, view)

{-| Home of all different girls with the cover of the first album of each.
-}

import Api
import Api.Build
import Browser.Navigation as Nav
import Girl exposing (Girl)
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Icon
import Id exposing (Id)
import List.Extra as List
import Process
import Task
import WrappedPage exposing (PageContent)



-- MODEL


type alias Model =
    { navKey : Nav.Key
    , build : String
    , girls : Status (List Girl)
    , id : String
    , name : String
    , response : String
    }


type Status a
    = Loading
    | Loaded a
    | Failed


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( { navKey = navKey
      , build = ""
      , girls = Loading
      , id = ""
      , name = ""
      , response = ""
      }
      -- because of weird bug, we have to request things that expect a port
      -- response after a delay
    , Cmd.batch
        [ Task.perform (always GetBuild) <| Process.sleep 50
        , Api.getGirls GotGirls
        ]
    )


view : Model -> PageContent Msg
view model =
    { title = "Settings"
    , iconButtons = []
    , content = viewContainer model
    }


viewContainer : Model -> Html Msg
viewContainer model =
    div [ class "container mx-auto p-2" ]
        [ viewBuild model.build
        , viewGirlEdit model
        , viewResponse model.response
        ]


viewBuild : String -> Html Msg
viewBuild build =
    div [ class "my-3 overflow-x-hidden" ]
        [ h2 [ class "text-lg font-bold" ] [ text "Build" ]
        , p [ class "font-mono break-words" ] [ text build ]
        ]


viewGirlEdit : Model -> Html Msg
viewGirlEdit model =
    case model.girls of
        Loading ->
            p [] [ text "Loading..." ]

        Failed ->
            p [] [ text "Failed to load list of models" ]

        Loaded girls ->
            div [ class "my-3 overflow-x-hidden" ]
                [ h2 [ class "mb-2 text-lg font-bold" ] [ text "Models" ]
                , viewAddGirl model.id model.name
                , viewGirlList girls
                ]


viewAddGirl : String -> String -> Html Msg
viewAddGirl id name =
    let
        inputRow : String -> (String -> Msg) -> String -> Html Msg
        inputRow title inputMsg initialValue =
            input
                [ class "w-full mb-3 px-2 py-1 rounded-lg bg-gray-300 focus:bg-white focus:border trans"
                , placeholder title
                , onInput inputMsg
                , value initialValue
                ]
                []
    in
    div []
        [ inputRow "Model id" InputId id
        , inputRow "Model name" InputName name
        , button
            [ class "block mb-3 px-2 py-1 rounded-lg bg-blue-400 text-white hover:shadow active:bg-blue-800 trans"
            , onClick AddGirl
            ]
            [ text "Add Model" ]
        ]


viewGirlList : List Girl -> Html Msg
viewGirlList girls =
    let
        viewGirl : Girl -> Html Msg
        viewGirl girl =
            li
                [ class "flex flex-row my-1 px-2 py-1 hover:bg-blue-200 rounded-lg trans" ]
                [ div [ class "flex-initial mr-2 font-mono" ]
                    [ text <| String.toUpper <| Id.idToString girl.id
                    ]
                , div [ class "flex-auto" ] [ text <| capitalCase girl.name ]
                , button
                    [ class "flex-initial hover:text-red-500 trans"
                    , style "margin-bottom" "-.5rem"
                    , onClick <| RemoveGirl girl.id
                    ]
                    [ Icon.icon "icon-trash-alt" ]
                ]
    in
    ul [ class "mb-16" ] <| List.map viewGirl girls


{-| Shows a "notification" to the user
-}
viewResponse : String -> Html Msg
viewResponse response =
    let
        visible : Bool
        visible =
            String.length response > 0
    in
    div
        [ class "fixed left-0 w-full my-1 p-4 z-40 opacity-75 bg-black text-white rounded-lg"
        , style "transition" "top .5s"
        , style "top"
            (if visible then
                "0px"

             else
                "-100px"
            )
        ]
        [ text response ]


capitalCase : String -> String
capitalCase str =
    str
        |> String.split " "
        |> List.map
            (\s ->
                String.toUpper (String.slice 0 1 s)
                    ++ String.slice 1 (String.length s) s
            )
        |> String.join " "



-- UPDATE


type Msg
    = GotBuild String
    | GetBuild
    | GotGirls (Result Http.Error (List Girl))
    | InputId String
    | InputName String
    | AddGirl
    | AddedGirl (Result Http.Error String)
    | RemoveGirl Id
    | RemovedGirl (Result Http.Error String)
    | ResetResponse


{-| Wait 5 seconds, then "hide" the notification by resetting response
-}
hideResponse : Cmd Msg
hideResponse =
    Task.perform (always ResetResponse) (Process.sleep 5000)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBuild build ->
            ( { model | build = build }, Cmd.none )

        GetBuild ->
            ( model, Api.Build.getBuild )

        GotGirls result ->
            case result of
                Ok girls ->
                    ( { model
                        | girls =
                            girls
                                |> List.sortBy (.id >> Id.idToString)
                                |> List.reverse
                                |> Loaded
                        , id = nextModelId girls
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | girls = Failed }, Cmd.none )

        InputId id ->
            ( { model | id = id }, Cmd.none )

        InputName name ->
            ( { model | name = name }, Cmd.none )

        AddGirl ->
            let
                girlsContainsId : List Girl -> String -> Bool
                girlsContainsId girls id =
                    List.length
                        (List.filter
                            (.id >> Id.idToString >> (==) id)
                            girls
                        )
                        == 0
            in
            case model.girls of
                Loaded girls ->
                    if model.id /= "" then
                        if model.name /= "" then
                            if girlsContainsId girls model.id then
                                let
                                    newGirl =
                                        Girl.makeGirl model.id model.name
                                in
                                ( { model | girls = Loaded <| newGirl :: girls }
                                , Api.addGirl newGirl AddedGirl
                                )

                            else
                                ( { model | response = "Id is already in use" }, hideResponse )

                        else
                            ( { model | response = "Name is required" }, hideResponse )

                    else
                        ( { model | response = "Id is required" }, hideResponse )

                _ ->
                    ( model, hideResponse )

        AddedGirl result ->
            case result of
                Ok res ->
                    ( { model | response = "Added model. " ++ res }, hideResponse )

                Err _ ->
                    ( { model | response = "Http error" }, hideResponse )

        RemoveGirl id ->
            case model.girls of
                Loaded girls ->
                    ( { model
                        | girls =
                            Loaded <|
                                List.filter (.id >> (/=) id) girls
                      }
                    , Api.removeGirl id RemovedGirl
                    )

                _ ->
                    ( model, Cmd.none )

        RemovedGirl result ->
            case result of
                Ok res ->
                    ( { model | response = "Removed model. " ++ res }, hideResponse )

                Err _ ->
                    ( { model | response = "Http error" }, hideResponse )

        ResetResponse ->
            ( { model | response = "" }, Cmd.none )


nextModelId : List Girl -> String
nextModelId girls =
    let
        -- Id of the last model (all models have 2 letter ids)
        lastModelId : String
        lastModelId =
            girls
                |> List.map (.id >> Id.idToString)
                |> List.sort
                |> List.last
                |> Maybe.withDefault ""

        -- Adds 1 to the char's codepoint and returns true also if it wrapped
        -- around
        incrementChar : Char -> ( Char, Bool )
        incrementChar char =
            let
                code =
                    Char.toCode char
            in
            -- between a-y
            if code >= 97 && code <= 121 then
                ( Char.fromCode <| code + 1, False )
                -- z wraps around to a

            else if code == 122 then
                ( 'a', True )

            else
                ( char, False )
    in
    -- Gets the next id by "incrementing" the 2nd letter, or incrementing
    -- the 1st & 2nd letter if the 2nd letter is already at z
    case String.toList lastModelId of
        [ first, second ] ->
            let
                ( newSecond, wrap ) =
                    incrementChar second

                ( newFirst, _ ) =
                    if wrap then
                        incrementChar first

                    else
                        ( first, False )
            in
            String.fromList [ newFirst, newSecond ]

        _ ->
            ""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.build of
        "" ->
            Api.Build.receiveBuild GotBuild

        _ ->
            Sub.none



-- EXPORT


toNavKey : Model -> Nav.Key
toNavKey model =
    model.navKey
