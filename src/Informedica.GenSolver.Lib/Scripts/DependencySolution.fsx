// throw this line first to the fsi

#r "nuget: Expecto"
#r "nuget: Expecto.FSCheck"


#load "../Scripts/load.fsx"

#time

open Informedica.GenSolver.Lib

open System
open System.IO

open Informedica.GenSolver.Lib
open Informedica.Utils.Lib.BCL
open MathNet.Numerics
open Types

module Api = Informedica.GenSolver.Lib.Api
module Solver = Informedica.GenSolver.Lib.Solver
module Name = Variable.Name
module ValueRange = Variable.ValueRange


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__



module Solve =

    let procss s =
        File.AppendAllLines("order.log", [s])
        $"{s} " |> printfn "%s"

    let set n p eqs = 
        let n = n |> Name.createExc
        match eqs |> Api.setVariableValues None n p with
        | Some var ->
            eqs
            |> List.map (fun e ->
                e |> Equation.replace var
            )
        | None -> eqs

    let setMinIncl n min = set n (MinInclProp min)
    let setMinExcl n min = set n (MinExclProp min)
    let setMaxIncl n max = set n (MaxInclProp max)
    let setMaxExcl n max = set n (MaxExclProp max)
    let setValues n vals = set n (ValsProp (vals |> Set.ofSeq))


    let printEqs = Solver.printEqs true procss
    let solve n p eqs =
        let logger = 
            fun s ->
                File.AppendAllLines("order.log", [s])
            |> SolverLogging.logger
        try
            eqs
            |> Api.solve Solver.sortQue logger None (n |> Name.createExc) p
        with
        | :? Variable.Exceptions.VariableException as e ->
            printfn $"{e.Data0}"
            raise e
        | :? Solver.Exception.SolverException as e ->
            printfn $"{e.Data0}"
            raise e

    let solveMinIncl n min = solve n (MinInclProp min)
    let solveMinExcl n min = solve n (MinExclProp min)
    let solveMaxIncl n max = solve n (MaxInclProp max)
    let solveMaxExcl n max = solve n (MaxExclProp max)
    let solveValues n vals = solve n (ValsProp (vals |> Set.ofSeq))

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

        match var.Values |> ValueRange.getValueSet with
        | None    -> ()
        | Some vs ->
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
//|> solveValues "z" [1N]
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
|> solveMaxIncl "d" 5N // a = 5/4*c_{max}
|> printEqs
|> ignore


failEqs
|> nonZeroNegative
|> solveMinIncl "b" 1N
|> printEqs
// |> solveMinIncl "c" 1N
// |> printEqs
|> solveMaxIncl "c" 20N
|> printEqs
|> solveMinIncl "e" 10N
|> printEqs
|> solveMaxIncl "e" 20N
|> printEqs
|> solveMaxIncl "f" 2N
|> printEqs
//|> solveMaxIncl "b" (20N/9N) // passes with 25
|> solveMaxIncl "a" 30N // passes with 25
|> printEqs
|> ignore





failEqs
//|> solveMinIncl "b" 1N
//|> printEqs
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
|> solveMaxExcl "a" 26N // this starts a loop
|> printEqs
|> ignore



failEqs
//|> solveValues "b" [1N]
//|> printEqs
//|> solveValues "c" [1N]
//|> printEqs
|> solveValues "c" [20N]
|> printEqs
//|> solveValues "e" [10N]
//|> printEqs
|> solveValues "e" [20N]
|> printEqs
//|> solveMinExcl "f" 1N
//|> printEqs
|> solveValues "f" [2N]
|> printEqs
//|> solveMaxExcl "a" 10N // this doesn't start a loop
//|> solveMaxExcl "a" 25N // this doesn't start a loop
//|> solveMaxExcl "a" 26N // this starts a loop
|> solveValues "a" [30N] // this starts a loop
|> printEqs
|> ignore


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