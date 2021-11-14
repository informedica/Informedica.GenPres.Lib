
//#I __SOURCE_DIRECTORY__

#load "load.fsx"

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
|> solve "B" (MinInclProp 0N)
|> solve "C" (MinInclProp 10N)
|> solve "A" ([1N] |> set |> IncrProp )
|> solve "A" (MaxExclProp 10N)



//=== Error Setting Setting ValueRange !! ===
//variable: 1.gentamicin.Component.Orderable.Qty [1/10000N..[1/10000]..1/2500N]
//valuerange: [11/100000N..1/9000N]
//error: ValueRangeException (MinLargerThanMax (MinIncl 1/5000N,MaxIncl 1/10000N))

(11N/100000N) 
|> BigRational.toMinMultipleOf (1N/10000N)

(11N/100000N) / (1N/10000N)
(11N/100000N) > (2N/10000N)

(1N/9000N) 
|> Variable.ValueRange.Maximum.createMax true
|> Variable.ValueRange.maxMultipleOf 
    ((1N/10000N) |> Set.singleton |> ValueRange.Increment.createIncr)

(1N/9000N) 
|> BigRational.toMinMultipleOf 
    ((1N/10000N))

((1N/5000N) - (1N/10000N)) < (1N/9000N) 

(11N/100000N) 
|> BigRational.toFloat

//- 3.912436
//=== Error Setting Setting ValueRange !! ===
//variable: 1.gentamicin.Component.Orderable.Qty [1/10000N..[1/10000]..1/2500N]
//valuerange: [11/100000N..1/9000N]
//error: ValueRangeException (MinLargerThanMax (MinIncl 1/5000N,MaxIncl 1/10000N))

//- 3.923305
//=== Cannot Solve Equation ===
//1.gentamicin.Item.Orderable.Qty [11/2500N..1/225N] = 
//1.gentamicin.Item.Component.Conc [10, 40] * 
//1.gentamicin.Component.Orderable.Qty [1/10000N..[1/10000]..1/2500N] 

(1N/225N) * 1000N |> BigRational.toFloat
(11N/2500N) * 1000N |> BigRational.toFloat



for x in [1..10000] do   
    for y in [1..100000] do   
        x * y |> ignore



module Types =
    
    type MyType = MyType of int


module MyType =

    open Types

    type MyTypeExtension = | MyTypeExtension with

        static member inline (?<-) (MyTypeExtension, x1 : ^a, x2 : ^a) = x1 * x2
        
        static member inline (?<-) (MyTypeExtension, x1 , x2) = 
            match x1, x2 with
            | (MyType x1), (MyType x2) -> x1 * x2 |> MyType

        
    let inline (*) x1 x2 = (?<-) MyTypeExtension x1 x2 

//    let inline (/) x1 x2 = (?<-) Div x1 x2

