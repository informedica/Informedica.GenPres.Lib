// throw this line first to the fsi

#r "nuget: Expecto"
#r "nuget: Expecto.FSCheck"


#load "../Scripts/load.fsx"

#time

open Informedica.GenSolver.Lib

open System
open System.IO

open MathNet.Numerics

module Name = Variable.Name
module ValueRange = Variable.ValueRange
module Minimum = ValueRange.Minimum
module Maximum = ValueRange.Maximum
module Increment = ValueRange.Increment
module ValueSet = ValueRange.ValueSet


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


module Solve =

    let procss s =
        File.AppendAllLines("order.log", [s])
        $"{s} " |> printfn "%s"

    let setProp n p eqs =
        let n = n |> Name.createExc
        match eqs |> Api.setVariableValues true None n p with
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
            |> Api.solve true Solver.sortQue logger None (n |> Name.createExc) p
        with
        | :? Variable.Exceptions.VariableException as e ->
            printfn $"{e.Data0}"
            raise e
        | :? Solver.Exception.SolverException as e ->
            printfn $"{e.Data0}"
            raise e

    let solveMinIncl n min = solve n (min |> Minimum.create true |> MinProp)
    let solveMinExcl n min = solve n (min |> Minimum.create false |> MinProp)
    let solveMaxIncl n max = solve n (max |> Maximum.create true |> MaxProp)
    let solveMaxExcl n max = solve n (max |> Maximum.create false |> MaxProp)
    let solveIncr n incr = solve n (set [incr] |> Increment.create |> IncrProp)
    let solveValues n vals = solve n (vals |> ValueSet.create |> ValsProp)

    let init     = Api.init
    let nonZeroNegative = Api.nonZeroNegative


    let findValidValues n (eqs : Equation list) =
        let var =
            eqs
            |> List.collect Equation.toVars
            |> List.tryFind (fun v ->
                v
                |> Variable.getName
                |> Name.toString
                |> fun x -> x = n
            )
            |> Option.get

        match var.Values |> ValueRange.getValSet with
        | None    -> ()
        | Some (ValueSet vs) ->
            for v in vs do
                try
                    eqs
                    |> solveValues n [v]
                    |> ignore
                    printfn $"can set {v}"
                with
                | _ ->
                    printfn $"cannot set {v}"



// minimal failing case
// a [1N..999N] = b <1/40N..999/5N> + c <9/25N..223/4N>
// d <1N..1998N> = f <1N..2N> * a [1N..5111/20N>
// d <1N..5111/10N> = e [10, 40] * b <1/40N..999/5N>
let failEqs =
    [
        "a = b + c"
        "d = e * b"
        "d = f * a"
    ]
    |> Api.init
//    |> nonZeroNegative


open Solve





failEqs
|> solveMinIncl "b" 1N
|> printEqs
|> solveMinIncl "c" 1N
|> printEqs
|> solveMinIncl "e" 10N
|> printEqs
|> solveMaxIncl "e" 20N
|> printEqs
|> solveMinIncl "f" 1N
|> printEqs
|> solveMaxIncl "f" 2N
|> printEqs
|> solveMaxIncl "a" 30N
|> printEqs
|> solveMaxIncl "d" 15N // a = 5/4*c_{max}
|> printEqs
|> ignore


let calcFail eqs n =
    eqs
    |> nonZeroNegative
    |> solveIncr "d" 1N
    |> solveMinIncl "b" 1N
    // |> printEqs
    // |> solveMinIncl "c" 1N
    // |> printEqs
    |> solveMaxIncl "c" 20N
    //|> printEqs
    |> solveMinIncl "e" 10N
    //|> printEqs
    |> solveMaxIncl "e" 20N
    //|> printEqs
    |> solveMaxIncl "f" 2N
    |> printEqs
    //|> solveMaxIncl "b" (20N/9N) // passes with 25
    |> solveMaxIncl "a" n //passes with 25 fails with 30
    |> printEqs
    |> ignore


calcFail failEqs 30N

for i in [5N..30N] do
    printfn $"trying {i}"
    calcFail failEqs i


failEqs
//|> solveMinIncl "b" 1N
//|> printEqs
|> solveIncr "d" 1N
|> printEqs
|> solveMinIncl "c" 1N
|> printEqs
|> solveMaxIncl "c" 4N
|> printEqs
|> solveMinIncl "e" 10N
|> printEqs
|> solveMaxIncl "e" 20N
|> printEqs
|> solveMinIncl "f" 1N
|> printEqs
|> solveMaxIncl "f" 3N
|> printEqs
//|> solveMaxExcl "a" 10N // this doesn't start a loop
//|> solveMaxExcl "a" 25N // this doesn't start a loop
//|> solveMaxExcl "a" 26N // this starts a loop
|> solveMaxExcl "a" 25N // this starts a loop
|> printEqs
|> ignore


// fails with 20!
let failEqs2 n =
    try
        failEqs
        |> solveValues "c" [20N]
        |> solveValues "e" [20N]
        |> solveValues "f" [10N]
        |> printEqs
        |> solveValues "a" [n]
        |> printEqs
        |> ignore
        printfn $"passed with {n}"
    with
    | _ ->
        printfn $"failed with {n}"
        ()

failEqs2 20N
for i in [1N .. 300N] do failEqs2 i


failEqs
|> solveMinIncl "b" 2N
|> printEqs
|> solveMinIncl "c" 1N
|> printEqs
|> solveMaxIncl "c" 2N
|> printEqs
|> solveMinIncl "e" 10N
|> printEqs
|> solveMaxIncl "e" 20N
|> printEqs
|> solveMaxIncl "f" 1N
|> printEqs
|> solveMaxIncl "a" 20N
|> printEqs
|> ignore

//b_max x  e_max > a_max x f_max


let maxEqs =
    failEqs
    |> List.map Equation.nonZeroOrNegative
    |> setMaxIncl "a" 25N
    |> setMaxIncl "b" 1N
    |> setMaxIncl "c" 20N
    |> setMaxIncl "f" 20N
    |> setMaxIncl "e" 2N


maxEqs
|> printEqs
|> solveMaxIncl "d" 10N
|> solveValues "d" [2N]
|> printEqs
