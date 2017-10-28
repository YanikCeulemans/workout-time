module Stopwatch exposing (Model, initial, start, pause, reset, tick, toString, isPaused)

import Time exposing (..)
import Util


type StopwatchState
    = Counting
    | Paused


type Model
    = Model
        { time : Time.Time
        , state : StopwatchState
        }


initial : Model
initial =
    Model
        { time = 0 * second
        , state = Paused
        }


start : Model -> Model
start (Model model) =
    case model.state of
        Paused ->
            Model { model | state = Counting }

        Counting ->
            Model model


pause : Model -> Model
pause (Model model) =
    case model.state of
        Counting ->
            Model { model | state = Paused }

        Paused ->
            Model model


reset : Model -> Model
reset timer =
    initial


tick : Model -> Model
tick (Model model) =
    case model.state of
        Counting ->
            Model { model | time = (model.time + (1 * second)) }

        Paused ->
            Model model


timeToString : Time.Time -> String
timeToString time =
    let
        minutes =
            inMinutes time
                |> floor
                |> ((flip (%)) 60)
                |> Util.padNumber

        seconds =
            inSeconds time
                |> floor
                |> ((flip (%)) 60)
                |> Util.padNumber
    in
        minutes ++ ":" ++ seconds


toString : Model -> String
toString (Model { time }) =
    timeToString time


isPaused : Model -> Bool
isPaused (Model model) =
    case model.state of
        Counting ->
            False

        Paused ->
            True
