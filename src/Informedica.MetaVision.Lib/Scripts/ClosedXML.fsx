
#load "load.fsx"


#load "../MetaVision.fs"

#r "nuget: ClosedXML"

open System
open ClosedXML

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

let loadDataImport =
    let wb = new XLWorkbook("data/output/DrugDatabaseForImport.xlsx")
    let ws = wb.Worksheet("Routes")
    ws.Cell(2, 3).Value <- rts
    wb.Save()

    printfn "finished"
