
#time

#load "load.fsx"


#load "../Types.fs"
#load "../Utils.fs"
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

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


let noProdsFilt (sr : SolutionRule) = sr.Products |> Array.isEmpty


SolutionRule.getRules ()
|> Array.filter noProdsFilt
|> SolutionRule.printGenerics
|> fun s -> File.WriteAllText("solutions.md", s |> String.concat "\n")



