module Util exposing (padNumber, timeToString)

import Time


padZeroesLeft : String -> String
padZeroesLeft =
    String.padLeft 2 '0'


padNumber : Int -> String
padNumber num =
    let
        numStr =
            toString num
    in
        if num < 10 then
            numStr |> padZeroesLeft
        else
            numStr


timeToString : Time.Time -> String
timeToString time =
    let
        minutes =
            Time.inMinutes time
                |> floor
                |> ((flip (%)) 60)
                |> padNumber

        seconds =
            Time.inSeconds time
                |> floor
                |> ((flip (%)) 60)
                |> padNumber
    in
        minutes ++ ":" ++ seconds
