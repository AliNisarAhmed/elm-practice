module Playground exposing (main)

import Html
import MyList exposing (MyList(..), isEmpty, sum)


multiplyBy5 : Int -> Int
multiplyBy5 number =
    let
        multiplier =
            5
    in
    number * multiplier


list1 : MyList.MyList a
list1 =
    Empty


list2 : MyList.MyList number
list2 =
    Node 9 Empty


main =
    MyList.isEmpty list2
        |> Debug.toString
        |> Html.text
