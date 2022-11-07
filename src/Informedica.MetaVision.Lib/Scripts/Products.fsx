

#load "load.fsx"


#load "../MetaVision.fs"


open DocumentFormat.OpenXml.Office2021.Excel.RichDataWebImage
open Informedica.Utils.Lib.BCL
open Informedica.ZIndex.Lib
open Informedica.MetaVision.Lib
open Informedica.ZIndex.Lib.ATCGroup
open Informedica.ZIndex.Lib.GenericProduct


let prods =
    GenPresProduct.get true
    |> filter
    |> Array.collect (fun gpp -> gpp.GenericProducts)


prods
|> Array.tryFind (fun gp -> gp.Id = 101656)

