
#load "load.fsx"

#load "../Measures.fs"
#load "../Calculations.fs"
#load "../MinIncrMax.fs"
#load "../Patient.fs"


open Informedica.GenCore.Lib

open System

let dto = AgeValue.Dto.dto ()

dto.Years <- Some 157
dto.Days <- Some 8

dto
|> AgeValue.Dto.fromDto



