
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



