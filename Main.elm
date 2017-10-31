module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Time exposing (..)
import Stopwatch
import CycleTimer
import Json.Encode as JsonE
import Json.Decode as JsonD


type Msg
    = Play
    | Reset
    | Pause
    | Tick


type alias Model =
    { stopwatch : Stopwatch.Model
    , cycleTimer : CycleTimer.Model
    }


initialModel : Model
initialModel =
    { stopwatch = Stopwatch.initial
    , cycleTimer =
        CycleTimer.initialize
            (CycleTimer.cycle "Burpees" (45 * second))
            [ CycleTimer.cycle "Rest" (15 * second)
            , CycleTimer.cycle "Push ups" (45 * second)
            ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            { model
                | stopwatch = Stopwatch.start model.stopwatch
                , cycleTimer = CycleTimer.start model.cycleTimer
            }
                ! []

        Pause ->
            { model
                | stopwatch = Stopwatch.pause model.stopwatch
                , cycleTimer = CycleTimer.pause model.cycleTimer
            }
                ! []

        Tick ->
            { model
                | stopwatch = Stopwatch.tick model.stopwatch
                , cycleTimer = CycleTimer.tick model.cycleTimer
            }
                ! []

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
        , Html.pre [] [ CycleTimer.modelToJson model.cycleTimer |> JsonE.encode 2 |> Html.text ]
        , Html.pre [] [ CycleTimer.modelToJson model.cycleTimer |> JsonE.encode 0 |> JsonD.decodeString CycleTimer.modelJsonDecoder |> toString |> Html.text ]
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
