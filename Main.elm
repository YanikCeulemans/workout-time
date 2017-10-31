module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Time exposing (..)
import List.Selection exposing (Selection)
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
    , workouts : Selection CycleTimer.Model
    }


initialModel : Model
initialModel =
    { stopwatch = Stopwatch.initial
    , workouts =
        List.Selection.fromList
            [ CycleTimer.initialize "Fat burning"
                (CycleTimer.cycle "Burpees" (45 * second))
                [ CycleTimer.cycle "Rest" (15 * second)
                , CycleTimer.cycle "Push ups" (45 * second)
                ]
            ]
    }


mapSelected : (a -> a) -> Selection a -> Selection a
mapSelected mapFn selection =
    List.Selection.selected selection
        |> Maybe.map
            (\selected ->
                List.Selection.map
                    (\el ->
                        if el /= selected then
                            el
                        else
                            mapFn el
                    )
                    selection
            )
        |> Maybe.withDefault selection


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            { model
                | stopwatch = Stopwatch.start model.stopwatch
                , workouts = mapSelected CycleTimer.start model.workouts
            }
                ! []

        Pause ->
            { model
                | stopwatch = Stopwatch.pause model.stopwatch
                , workouts = mapSelected CycleTimer.pause model.workouts
            }
                ! []

        Tick ->
            { model
                | stopwatch = Stopwatch.tick model.stopwatch
                , workouts = mapSelected CycleTimer.tick model.workouts
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
