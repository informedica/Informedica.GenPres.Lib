
// #I __SOURCE_DIRECTORY__

#load "load.fsx"

#load "../Utils.fs"
#load "../Variable.fs"

#time

open Informedica.GenSolver.Lib
open Informedica.Utils.Lib.BCL
open Informedica.GenSolver.Utils
open MathNet.Numerics
open Types

module Api = Informedica.GenSolver.Lib.Api
module Solver = Informedica.GenSolver.Lib.Solver
module Name = Variable.Name
module ValueRange = Variable.ValueRange

let procss s = $"{s} " |> printfn "%s"

type Logger = Types.Logging.Logger

let printEqs = Solver.printEqs true procss
let solve n p eqs =
    let logger = { Logger.Log = ignore }
    let n = n |> Name.createExc
    Api.solve id logger None n p eqs
//    |> fun eqs -> eqs |> printEqs |> printfn "%A"; eqs

let init     = Api.init
let nonZeroNegative = Api.nonZeroNegative

printfn "=== Product Equation ==="
let prodEq1 = 
    [
        "a = b * c"  
    ] 
    |> init
// print the equation
prodEq1 |> printEqs |> ignore

printfn "=== Sum Equation ==="
let sumEq1 = 
    [
        "c = d + e + f"  
    ] 
    |> init
// print the equation
sumEq1 |> printEqs |> ignore


prodEq1
|> solve "b" (IncrProp ([2N; 3N; 6N] |> Set.ofList))
|> solve "b" (MaxInclProp 24N)
|> solve "a" (MaxInclProp 720N)
|> solve "c" (ValsProp ([60N; 120N; 240N; 500N; 1000N] |> Set.ofList))
|> printEqs 
|> ignore