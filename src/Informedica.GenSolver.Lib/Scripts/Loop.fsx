
type Message = | ErrorMsg of int

exception MyException of Message

let runA n =
    printfn $"runA {n}"
    if n > 5 then
        n
        |> ErrorMsg
        |> MyException
        |> raise
    else ()

let runB n = runA n

let rec loop2 n =
    if n > 11 then ()
    else
        try
            runB n
            loop2 (n + 1)
        with
        | MyException m ->
            printfn $"{n}. loop2 inner error {m}"
            m |> MyException |> raise


let rec loop1 n =
    let _loop2 n =
        try
            loop2 n
        with
        | MyException m ->
            printfn $"{n}. loop1 inner error {m}"
            m |> MyException |> raise

    if n > 11 then ()
    else
        _loop2 n
        loop1 (n + 1)

let runLoop () =
    try
        loop1 0
    with
    | MyException m -> printfn $"outer error {m}"

runLoop ()

#load "../Scripts/load.fsx"

#time

open MathNet.Numerics
open Informedica.GenSolver.Lib

open System
open System.IO

module Name = Variable.Name
module ValueRange = Variable.ValueRange
module Minimum = ValueRange.Minimum
module Maximum = ValueRange.Maximum
module Increment = ValueRange.Increment
module ValueSet = ValueRange.ValueSet


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


module Solve =

    let clearLog () = File.WriteAllText("order.log", "")

    let procss s =
        File.AppendAllLines("order.log", [s])
        $"{s} " |> printfn "%s"

    let setProp n p eqs =
        let n = n |> Name.createExc
        match eqs |> Api.setVariableValues true n p with
        | Some var ->
            eqs
            |> List.map (fun e ->
                e |> Equation.replace var
            )
        | None -> eqs

    let setMinIncl n min = min |> Minimum.create true |> MinProp |> setProp n
    let setMinExcl n min = min |> Minimum.create false |> MinProp |> setProp n
    let setMaxIncl n max = max |> Maximum.create true |> MaxProp |> setProp n
    let setMaxExcl n max = max |> Maximum.create true |> MaxProp |> setProp n
    let setValues n vals = vals |> ValueSet.create |> ValsProp |> setProp n

    let printEqs = Solver.printEqs true procss
    let solve n p eqs =
        let logger =
            fun s ->
                File.AppendAllLines("order.log", [s])
            |> SolverLogging.logger
        try
            eqs
            |> Api.solve true Solver.sortQue logger (n |> Name.createExc) p
        with
        | :? Exceptions.SolverException as e ->
            printfn $"{e.Data0}"
            raise e

    let solveMinIncl n min = solve n (min |> Minimum.create true |> MinProp)
    let solveMinExcl n min = solve n (min |> Minimum.create false |> MinProp)
    let solveMaxIncl n max = solve n (max |> Maximum.create true |> MaxProp)
    let solveMaxExcl n max = solve n (max |> Maximum.create false |> MaxProp)
    let solveIncr n incr = solve n (set [incr] |> Increment.create |> IncrProp)
    let solveValues n vals = solve n (vals |> ValueSet.create |> ValsProp)


Solve.clearLog ()

["a = b"]
|> Api.init
|> Solve.setMinIncl "a" 10N
|> Solve.solveMaxIncl "b" 1N
