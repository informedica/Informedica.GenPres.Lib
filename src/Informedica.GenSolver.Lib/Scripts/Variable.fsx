
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"

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


v1 * v2


module ValueRange = Variable.ValueRange

// Test setting min < incr

let vr =
    1N
    |> ValueRange.createIncr id (string >> exn >> raise)
    |> ValueRange.minIncrValueRange (1N |> Variable.ValueRange.createMin true)

vr 
|> ValueRange.setMin (1N/10N |> Variable.ValueRange.createMin true)


