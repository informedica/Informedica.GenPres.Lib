
#load "load.fsx"


open MathNet.Numerics

open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

let id = "1" |> WrappedString.Id.create

let n = [ "pcm" ] |> WrappedString.Name.create

let str_prs =
    Prescription.timed ValueUnit.NoUnit ValueUnit.NoUnit

Order.createNew id n "infusion" str_prs "iv"

VariableUnit.Quantity.quantity [ id |> WrappedString.Id.toString; n |> WrappedString.Name.toString; OrderAdjustQty |> Order.Mapping.map ] ValueUnit.NoUnit


Order.Dto.timed "1" "paracetamol" "infusion" "iv"
|> Order.Dto.fromDto
|> Order.getId

