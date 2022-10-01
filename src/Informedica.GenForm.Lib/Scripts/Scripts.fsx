
#time

#load "load.fsx"

open System
open System.IO


open MathNet.Numerics
open Informedica.GenForm.Lib

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


File.WriteAllText("formularium.md", "")


DoseRule.doseRules ()
|> fun drs ->
    drs
    |> DoseRule.generics
    |> Array.sort
    |> Array.map(fun g ->
        drs
        |> DoseRule.filter
               { DoseRule.allFilter with
                    Generic = Some g
                    //Age = Some 7N
                    //GestAge = Some (7N * 39N)
                }
        |> DoseRule.toMarkdown
    )
    |> fun s -> File.AppendAllLines("formularium.md", s)


DoseRule.doseRules ()
|> DoseRule.filter
    { DoseRule.allFilter with
        Generic = Some "paracetamol"
    }

