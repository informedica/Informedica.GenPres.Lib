
#load "load.fsx"

#time

open MathNet.Numerics
    
open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

module Quantity = VariableUnit.Quantity

ValueUnit.Units.Mass.milliGram
|> VariableUnit.Quantity.quantity ["gentamicin"] 
|> Quantity.toString

VariableUnit.RateAdjust.rateAdjust 
    ["dopamin"]
    ValueUnit.Units.Mass.microGram
    ValueUnit.Units.Weight.kiloGram
    ValueUnit.Units.Time.minute


module Units = ValueUnit.Units

let mg1 = ValueUnit.create Units.Mass.milliGram 1N
let piece = ValueUnit.create (Units.General.general "piece") 1N
let kg1 = ValueUnit.create (Units.Weight.kiloGram) 1N
let min1 = ValueUnit.create (Units.Time.minute) 1N

[mg1; mg1 / piece] |> List.reduce (*) |> ValueUnit.get |> snd

mg1 * (mg1 / piece)
(mg1 / piece) * mg1
mg1 * (piece / mg1)

let mgperkgpermin = mg1 / kg1 / min1
mgperkgpermin * min1

(mg1 / kg1) = (mg1/ kg1 / min1) * min1

(mg1 / kg1 / min1) = (mg1 / kg1) / min1

(mg1 / kg1 / min1)
|> ValueUnit.get
|> snd
|> ValueUnit.unitToString
|> ValueUnit.unitFromString



