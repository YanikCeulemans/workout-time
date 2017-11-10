module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Time exposing (..)
import List.Selection exposing (Selection)
import Stopwatch
import CycleTimer
import Json.Encode as JsonE
import Json.Decode as JsonD
import Debug


type Msg
    = Play
    | Reset
    | Pause
    | Tick
    | SelectWorkout CycleTimer.Model
    | Deselect
    | NavigateToAdd
    | NavigateToHome


type Route
    = Select
    | Workout CycleTimer.Model
    | Edit CycleTimer.Model
    | Add


type alias Model =
    { stopwatch : Stopwatch.Model
    , workouts : Selection CycleTimer.Model
    , activeRoute : Route
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
    , activeRoute = Select
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
            { model
                | stopwatch = Stopwatch.reset model.stopwatch
                , workouts = List.Selection.map CycleTimer.reset model.workouts
            }
                ! []

        SelectWorkout workout ->
            { model | workouts = List.Selection.select workout model.workouts } ! []

        Deselect ->
            { model
                | workouts =
                    List.Selection.map CycleTimer.reset model.workouts
                        |> List.Selection.deselect
            }
                ! []

        NavigateToAdd ->
            { model | activeRoute = Add } ! []

        NavigateToHome ->
            { model | activeRoute = Select } ! []


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
        [ Html.header [ class "header" ] [ viewHeader model ]
        , Html.div [ class "content" ] [ viewRouteContent model ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    Html.div [ class "header-content" ]
        [ Html.div [ class "header-title" ]
            [ Html.button
                [ onClick NavigateToHome
                , class "fa-arrow-left control-transparent control-navigate-back"
                , classList [ ( "active", model.activeRoute /= Select ) ]
                ]
                []
            , Html.span [ class "header-title-text" ]
                [ Html.text "Workout time" ]
            ]
        , Html.button
            [ onClick NavigateToAdd
            , class "fa-plus-circle control-transparent hidable"
            , classList [ ( "active", model.activeRoute == Add ) ]
            ]
            []
        ]


viewRouteContent : Model -> Html Msg
viewRouteContent model =
    case model.activeRoute of
        Select ->
            viewWorkoutSelector model

        Workout workout ->
            viewWorkout model

        Edit workout ->
            Html.div [] []

        Add ->
            viewAdd model


workoutItem : CycleTimer.Model -> Html Msg
workoutItem workout =
    Html.li
        [ class "workout"
        , onClick <| SelectWorkout workout
        ]
        [ CycleTimer.title workout |> Html.text ]


viewWorkoutSelector : Model -> Html Msg
viewWorkoutSelector model =
    Html.ul []
        (List.Selection.map
            workoutItem
            model.workouts
            |> List.Selection.toList
        )


viewWorkout : Model -> Html Msg
viewWorkout model =
    Html.div []
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
            , Html.button
                [ class "control"
                , onClick Deselect
                ]
                [ Html.text "Back" ]
            ]
        ]


viewAdd : Model -> Html Msg
viewAdd model =
    Html.div [] [ Html.text "Add" ]


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
