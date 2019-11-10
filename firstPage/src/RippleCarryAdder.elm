module RippleCarryAdder exposing (Binary, andGate, fullAdder, halfAdder, inverter, orGate, rippleCarryAdder)

import Array
import Bitwise


andGate : Int -> Int -> Int
andGate a b =
    Bitwise.and a b


orGate : Int -> Int -> Int
orGate a b =
    Bitwise.or a b


inverter : Int -> Int
inverter num =
    case num of
        1 ->
            0

        0 ->
            1

        _ ->
            -1


type alias SumCarry =
    { sum : Int
    , carry : Int
    }


halfAdder : Int -> Int -> SumCarry
halfAdder a b =
    let
        d =
            orGate a b

        e =
            andGate a b |> inverter

        sumDigit =
            andGate d e

        carryOut =
            andGate a b
    in
    { carry = carryOut
    , sum = sumDigit
    }


fullAdder : Int -> Int -> Int -> SumCarry
fullAdder a b carryIn =
    let
        firstResult =
            halfAdder b carryIn

        secondResult =
            halfAdder a firstResult.sum

        finalCarry =
            orGate firstResult.carry secondResult.carry
    in
    { carry = finalCarry
    , sum = secondResult.sum
    }


type alias Binary =
    { d0 : Int
    , d1 : Int
    , d2 : Int
    , d3 : Int
    }



-- rippleCarryAdder : Binary -> Binary -> Binary -


rippleCarryAdder a b carryIn =
    let
        -- Extracting Digits
        firstSignal =
            extractDigits a

        secondSignal =
            extractDigits b

        firstResult =
            fullAdder firstSignal.d3 secondSignal.d3 carryIn

        secondResult =
            fullAdder firstSignal.d2 secondSignal.d2 firstResult.carry

        thirdResult =
            fullAdder firstSignal.d1 secondSignal.d2 secondResult.carry

        finalResult =
            fullAdder firstSignal.d0 secondSignal.d0 thirdResult.carry
    in
    [ finalResult, thirdResult, secondResult, firstResult ]
        |> List.map .sum
        |> (::) finalResult.carry
        |> numberFromDigits


numberFromDigits digitsList =
    List.foldl (\x acc -> x + 10 * acc) 0 digitsList


extractDigits number =
    String.fromInt number
        |> String.split ""
        |> List.map stringToInt
        |> Array.fromList
        |> arrayToRecord


stringToInt string =
    String.toInt string
        |> Maybe.withDefault -1


arrayToRecord arr =
    let
        firstElement =
            Array.get 0 arr |> Maybe.withDefault -1

        secondElement =
            Array.get 1 arr |> Maybe.withDefault -1

        thirdElement =
            Array.get 2 arr |> Maybe.withDefault -1

        fourthElement =
            Array.get 3 arr |> Maybe.withDefault -1
    in
    { d0 = firstElement
    , d1 = secondElement
    , d2 = thirdElement
    , d3 = fourthElement
    }
