
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


(1N/2N)
|> BigRational.toMaxMultipleOf (1N/3N)


let m = 1000N
let isExcl = false

//[60N; 120N; 500N]
let calcMinOrMaxToMultiple isMax isIncl incrs m =
    incrs
    |> Set.fold (fun (b, acc) i ->
        let ec = if isMax then (>=) else (<=)
        let nc = if isMax then (>) else (<)
        let ad = if isMax then (-) else (+)

        let m' = 
            if isMax then m |> BigRational.toMaxMultipleOf i
            else m |> BigRational.toMinMultipleOf i
            
        let m' = 
            if (isIncl |> not) && (m' |> ec <| m) then 
                printfn $"recalc because is excl: {(m' |> ad <| i) }"
                (m' |> ad <| i) 
            else m'
        
        match acc with 
        | Some a -> if (m' |> nc <| a) then (true, Some m') else (b, Some a)
        | None   -> (true, Some m')
    ) (isIncl, None)
    |> fun (b, r) -> b, r |> Option.defaultValue m


let maxInclMultipleOf = calcMinOrMaxToMultiple true true 

let maxExclMultipleOf = calcMinOrMaxToMultiple true false 

let minInclMultipleOf = calcMinOrMaxToMultiple false true

let minExclMultipleOf = calcMinOrMaxToMultiple false false


1000N
|> maxInclMultipleOf ([60N; 120N; 500N] |> Set.ofList)

1000N
|> maxExclMultipleOf ([60N; 120N; 500N] |> Set.ofList)

7N
|> minInclMultipleOf ([2N; 3N] |> Set.ofList)

9N
|> minInclMultipleOf ([2N; 3N] |> Set.ofList)

9N
|> minExclMultipleOf ([2N; 3N] |> Set.ofList)

9N
|> minExclMultipleOf ([] |> Set.ofList)

