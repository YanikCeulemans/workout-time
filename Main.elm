module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Time exposing (..)
import Stopwatch
import CycleTimer


type Msg
    = Play
    | Reset
    | Pause
    | Tick


type alias Model =
    { workingTime : Time.Time
    , restingTime : Time.Time
    , stopwatch : Stopwatch.Model
    , cycleTimer : CycleTimer.Model
    }


initialModel : Model
initialModel =
    { workingTime = 45 * second
    , restingTime = 15 * second
    , stopwatch = Stopwatch.initial
    , cycleTimer =
        CycleTimer.initialize
            (CycleTimer.cycle "Burpees" (45 * second))
            []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            { model | stopwatch = Stopwatch.start model.stopwatch } ! []

        Pause ->
            { model | stopwatch = Stopwatch.pause model.stopwatch } ! []

        Tick ->
            { model | stopwatch = Stopwatch.tick model.stopwatch } ! []

        Reset ->
            { model | stopwatch = Stopwatch.reset model.stopwatch } ! []


playToggleButton : Stopwatch.Model -> Html Msg
playToggleButton timer =
    let
        ( msg, iconClass ) =
            if Stopwatch.isPaused timer then
                ( Play, "fa-play" )
            else
                ( Pause, "fa-pause" )
    in
        Html.button
            [ Html.Events.onClick msg
            , class <| iconClass ++ " fa-3x control"
            ]
            []


view : Model -> Html Msg
view model =
    Html.div [ class "master" ]
        [ Html.div [ class "timer" ]
            [ Stopwatch.toString model.stopwatch
                |> Html.text
            ]
        , Html.div [ class "timer" ]
            [ CycleTimer.toString model.cycleTimer
                |> Html.text
            ]
        , Html.div [ class "controls" ]
            [ playToggleButton model.stopwatch
            , Html.button
                [ Html.Events.onClick Reset
                , class "fa-repeat fa-3x control"
                ]
                []
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    every second (\_ -> Tick)


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
