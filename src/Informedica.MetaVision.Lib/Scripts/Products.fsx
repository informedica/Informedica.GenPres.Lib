

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
    |> filterGenericProducts


prods
|> Array.tryFind (fun gp ->
    gp.Shape |> String.equalsCapInsens "gel" &&
    gp.Route |> Array.exists (String.equalsCapInsens "endotracheopulmonair")
)

