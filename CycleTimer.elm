module CycleTimer exposing (Cycle, cycle, Model, initialize, title, start, pause, reset, tick, current, cycleTimer, modelToJson, modelJsonDecoder)

import Time
import Json.Encode as JsonE
import Json.Decode as JsonD
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


cycleDuration : Cycle -> Time.Time
cycleDuration (Cycle cycle) =
    cycle.duration


type ModelState
    = Counting
    | Paused


type Model
    = Model
        { title : String
        , state : ModelState
        , timer : Time.Time
        , cycles : SelectList.SelectList Cycle
        }


initialize : String -> Cycle -> List Cycle -> Model
initialize title (Cycle cycle) cycles =
    Model
        { title = title
        , state = Paused
        , timer = cycle.duration
        , cycles = SelectList.fromLists [] (Cycle cycle) cycles
        }


title : Model -> String
title (Model model) =
    model.title


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
reset model =
    pause model
        |> selectFirstCycle


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


selectFirstCycle : Model -> Model
selectFirstCycle (Model model) =
    let
        cycleList =
            SelectList.toList model.cycles
    in
        case cycleList of
            [] ->
                Model model

            [ single ] ->
                Model
                    { model
                        | cycles = SelectList.singleton single
                        , timer = cycleDuration single
                    }

            head :: tail ->
                Model
                    { model
                        | cycles = SelectList.fromLists [] head tail
                        , timer = cycleDuration head
                    }


selectNextCycle : Model -> Model
selectNextCycle (Model model) =
    let
        nextedCycles =
            selectNext model.cycles

        nextDuration =
            SelectList.selected nextedCycles
                |> cycleDuration
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


current : Model -> ( String, Time.Time )
current (Model { timer, cycles }) =
    ( SelectList.selected cycles |> (\(Cycle cycle) -> cycle.title), timer )


cycleTimer : Model -> ( String, List Cycle )
cycleTimer (Model model) =
    ( model.title, SelectList.toList model.cycles )


modelStateToJson : ModelState -> JsonE.Value
modelStateToJson =
    Basics.toString >> JsonE.string


modelToJson : Model -> JsonE.Value
modelToJson (Model model) =
    JsonE.object
        [ ( "title", JsonE.string model.title )
        , ( "state", modelStateToJson model.state )
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
    JsonD.map4
        (\title state timer cycles ->
            Model
                { title = title
                , state = state
                , timer = timer
                , cycles = cycles
                }
        )
        (JsonD.field "title" JsonD.string)
        (JsonD.field "state" (JsonD.map modelStateFromString JsonD.string))
        (JsonD.field "timer" (JsonD.map ((*) Time.millisecond) JsonD.float))
        (JsonD.field "cycles" modelCyclesDecoder)
