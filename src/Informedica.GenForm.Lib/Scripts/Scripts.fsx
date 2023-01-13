

#time

#load "load.fsx"


#load "../Types.fs"
#load "../Utils.fs"
#load "../Mapping.fs"
#load "../MinMax.fs"
#load "../Patient.fs"
#load "../DoseType.fs"
#load "../Product.fs"
#load "../Filter.fs"
#load "../DoseRule.fs"
#load "../SolutionRule.fs"
#load "../PrescriptionRule.fs"


open System
open System.IO


open MathNet.Numerics

open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.GenForm.Lib



Product.get ()
|> Array.filter (fun p -> p.RequiresReconstitution)


Mapping.mappingRouteShape


Mapping.filterRouteShapeUnit "INTRAMUSCULAIR" "POEDER VOOR SUSPENSIE VOOR INJECTIE" "stuk"