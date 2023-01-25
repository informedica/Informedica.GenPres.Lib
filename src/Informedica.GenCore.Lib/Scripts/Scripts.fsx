
#load "load.fsx"

#load "../Measures.fs"
#load "../Calculations.fs"
#load "../MinIncrMax.fs"


open Informedica.GenCore.Lib

open System


Calculations.Age.actAge (DateTime(1965, 12, 7)) DateTime.Now


