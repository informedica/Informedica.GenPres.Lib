
//#I __SOURCE_DIRECTORY__

#load "load.fsx"

#time

open Informedica.GenSolver.Lib
open Informedica.Utils.Lib.BCL
open MathNet.Numerics
open Types

module Api = Informedica.GenSolver.Lib.Api
module Solver = Informedica.GenSolver.Lib.Solver
module Name = Variable.Name
module ValueRange = Variable.ValueRange

let procss s = "> " + s + " </br> "|> String.replace "*" "\*" |> printfn "%s"

type Logger = Types.Logging.Logger

let printEqs = Solver.printEqs true procss
let solve n p eqs =
    let logger = { Logger.Log = ignore }
    let n = n |> Name.createExc
    Api.solve id logger None n p eqs
    |> fun eqs -> eqs |> printEqs |> printfn "%A"; eqs

let init     = Api.init
let nonZeroNegative = Api.nonZeroNegative

let eqs = " = "
let tms = " * "
let add = " + "

// Test set min smaller than incr
["A" + eqs + "B" + tms + "C"
 "C" + eqs + "X" + add + "Y"
]
|> Api.init
|> solve "A" (MinInclProp 0N)
// |> solve "B" (MinInclProp 0N)
// |> solve "C" (MinInclProp 10N)
|> solve "A" (MaxExclProp 10N)
|> printEqs
|> ignore

