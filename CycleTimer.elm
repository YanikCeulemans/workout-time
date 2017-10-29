module CycleTimer exposing (Cycle, cycle, Model, start, pause, initialize, tick, toString)

import Time
import Util
import SelectList


type Cycle
    = Cycle
        { title : String
        , duration : Time.Time
        }


cycle : String -> Time.Time -> Cycle
cycle title duration =
    Cycle
        { title = title
        , duration = duration
        }


type ModelState
    = Counting
    | Paused


type Model
    = Model
        { state : ModelState
        , timer : Time.Time
        , cycles : SelectList.SelectList Cycle
        }


initialize : Cycle -> List Cycle -> Model
initialize (Cycle cycle) cycles =
    Model
        { state = Paused
        , timer = cycle.duration
        , cycles = SelectList.fromLists [] (Cycle cycle) cycles
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


tick : Model -> Model
tick (Model model) =
    case model.state of
        Paused ->
            Model model

        Counting ->
            if model.timer > 0 then
                Model { model | timer = model.timer - (1 * Time.second) }
            else if isLast model.cycles then
                pause (Model model)
            else
                selectNextCycle (Model model)


selectNextCycle : Model -> Model
selectNextCycle (Model model) =
    let
        nextedCycles =
            selectNext model.cycles

        nextDuration =
            SelectList.selected nextedCycles
                |> (\(Cycle next) -> next.duration)
    in
        Model
            { model
                | cycles = nextedCycles
                , timer = nextDuration
            }


selectNext : SelectList.SelectList a -> SelectList.SelectList a
selectNext slist =
    SelectList.after slist
        |> List.head
        |> Maybe.map
            (\next ->
                SelectList.fromLists
                    ((SelectList.before slist) ++ [ SelectList.selected slist ])
                    next
                    (SelectList.after slist |> List.tail |> Maybe.withDefault [])
            )
        |> Maybe.withDefault slist


isLast : SelectList.SelectList a -> Bool
isLast =
    SelectList.after >> List.isEmpty


toString : Model -> String
toString (Model { timer, cycles }) =
    (SelectList.selected cycles |> (\(Cycle cycle) -> cycle.title)) ++ " " ++ Util.timeToString timer
