

#time

#load "load.fsx"


#load "../Types.fs"
#load "../Utils.fs"
#load "../Mapping.fs"
#load "../MinMax.fs"
#load "../Patient.fs"
#load "../Product.fs"
#load "../DoseRule.fs"
#load "../SolutionRule.fs"


open System
open System.IO


open MathNet.Numerics

open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.GenForm.Lib


DoseRule.doseRules ()


SolutionRule.getRules()
|> SolutionRule.printGenerics

