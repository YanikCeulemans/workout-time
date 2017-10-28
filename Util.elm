module Util exposing (padNumber)


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
