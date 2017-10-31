module CycleTimer exposing (Cycle, cycle, Model, start, pause, initialize, tick, toString, modelToJson, modelJsonDecoder)

import Time
import Json.Encode as JsonE
import Json.Decode as JsonD
import Util
import SelectList


type Cycle
    = Cycle
        { title : String
        , duration : Time.Time
        }


cycleToJson : Cycle -> JsonE.Value
cycleToJson (Cycle cycle) =
    JsonE.object
        [ ( "title", JsonE.string cycle.title )
        , ( "duration", JsonE.float <| Time.inMilliseconds cycle.duration )
        ]


cycleJsonDecoder : JsonD.Decoder Cycle
cycleJsonDecoder =
    JsonD.map2
        (\title duration ->
            Cycle
                { title = title
                , duration = duration
                }
        )
        (JsonD.field "title" JsonD.string)
        (JsonD.field "duration" (JsonD.map ((*) Time.millisecond) JsonD.float))


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


modelStateToJson : ModelState -> JsonE.Value
modelStateToJson =
    Basics.toString >> JsonE.string


modelToJson : Model -> JsonE.Value
modelToJson (Model model) =
    JsonE.object
        [ ( "state", modelStateToJson model.state )
        , ( "timer", JsonE.float <| Time.inMilliseconds model.timer )
        , ( "cycles"
          , SelectList.map cycleToJson model.cycles
                |> SelectList.toList
                |> JsonE.list
          )
        ]


modelStateFromString : String -> ModelState
modelStateFromString str =
    case str of
        "Counting" ->
            Counting

        _ ->
            Paused


modelCyclesFromList : List Cycle -> JsonD.Decoder (SelectList.SelectList Cycle)
modelCyclesFromList cycles =
    case cycles of
        [] ->
            JsonD.fail "A minimum on 1 cycle is required"

        [ cycle ] ->
            JsonD.succeed <| SelectList.singleton cycle

        head :: tail ->
            JsonD.succeed <| SelectList.fromLists [] head tail


modelCyclesDecoder : JsonD.Decoder (SelectList.SelectList Cycle)
modelCyclesDecoder =
    JsonD.list cycleJsonDecoder
        |> JsonD.andThen modelCyclesFromList


modelJsonDecoder : JsonD.Decoder Model
modelJsonDecoder =
    JsonD.map3
        (\state timer cycles ->
            Model
                { state = state
                , timer = timer
                , cycles = cycles
                }
        )
        (JsonD.field "state" (JsonD.map modelStateFromString JsonD.string))
        (JsonD.field "timer" (JsonD.map ((*) Time.millisecond) JsonD.float))
        (JsonD.field "cycles" modelCyclesDecoder)
