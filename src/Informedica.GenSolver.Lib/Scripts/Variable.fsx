
// #I __SOURCE_DIRECTORY__

#load "load.fsx"

#load "../Utils.fs"
#load "../Variable.fs"

#time

open MathNet.Numerics
open Informedica.GenSolver.Lib

let v1 = 
    [1N..1000N] 
    |> Set.ofList
    |> Variable.ValueRange.createValueSet 
    |> Variable.createSucc ("vr1" |> Variable.Name.createExc)

let v2 = 
    [1N..1000N] 
    |> Set.ofList
    |> Variable.ValueRange.createValueSet 
    |> Variable.createSucc ("vr2" |> Variable.Name.createExc)

open Variable.Operators

// Example of long running operation
v1 ^* v2

module Set =

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL

    let removeBigRationalMultiples xs =
        xs
        |> Set.fold (fun acc x1 ->
            acc 
            |> Set.filter (fun x2 ->
                x1 = x2 ||
                x2 |> BigRational.isMultiple x1 |> not
            )
        ) xs


[2N; 3N; 6N; 11N; 12N; 19N]
|> Set.ofList
|> Set.removeBigRationalMultiples

