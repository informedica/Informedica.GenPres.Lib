
#load "load.fsx"


#load "../MetaVision.fs"


open System

open ClosedXML.Excel
open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.ZIndex.Lib
open Informedica.MetaVision.Lib
open Informedica.ZIndex.Lib.ATCGroup
open Informedica.ZIndex.Lib.GenericProduct

Environment.CurrentDirectory
File.exists "data/output/DrugDatabaseForImport.xlsx"


let rts =
    MetaVision.createRoutes "Routes.csv"
    |> Array.map (fun s -> s |> String.split "\t" |> List.toArray)

let loadDataImport rts forms ingrds meds compl brands prods =
    let wb = new XLWorkbook("data/output/DrugDatabaseForImport.xlsx")
    wb.Worksheet("Routes").Cell(2, 3).Value <- rts
    wb.Worksheet("DoseForms").Cell(2, 3).Value <- forms
    wb.Worksheet("Ingredients").Cell(2, 3).Value <- ingrds
    wb.Worksheet("Medications").Cell(2, 3).Value <- meds
    wb.Worksheet("ComplexMedications").Cell(2, 3).Value <- compl
    wb.Worksheet("Brands").Cell(2, 3).Value <- brands
    wb.Worksheet("Products").Cell(2, 3).Value <- prods

    wb.Save()
    printfn "finished"
