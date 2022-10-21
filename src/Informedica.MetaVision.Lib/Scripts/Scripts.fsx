
#load "load.fsx"

#load "../MetaVision.fs"

open Informedica.Utils.Lib.BCL
open Informedica.ZIndex.Lib
open Informedica.MetaVision.Lib
open Informedica.ZIndex.Lib.ATCGroup
open Informedica.ZIndex.Lib.GenericProduct


MetaVision.shapeUnits "ShapeUnits.csv"


MetaVision.createRoutes "Routes.csv"


MetaVision.createDoseForms "DoseForms.csv"


MetaVision.createMedications None "Ingredients.csv" "Medications.csv" "ComplexMedications.csv"
|> Array.length


open Informedica.Utils.Lib.BCL


ATCGroup.get ()
|> Array.filter (fun g -> g.ATC5 |> String.contains "V04CL")



let gps =
    GenPresProduct.get true
    |> Array.collect (fun gpp -> gpp.GenericProducts)
    |> Array.map (fun gp -> gp.Name)
    |> Array.distinct
    |> Array.length


GenPresProduct.get true
|> Array.collect (fun gpp -> gpp.GenericProducts)
|> fun xs ->
    let x = xs |> Array.head

    xs
//    |> Array.tail
    |> Array.fold (fun acc gp ->
        let acc =
            if acc |> Array.exists (fun (x,_) -> x.Label = gp.Label) then acc
            else acc |> Array.append [| (gp, 0) |]

        acc
        |> Array.map (fun (x, n) ->
            if x.Label = gp.Label then (x, n + 1) else (x, n)
        )
    ) [| (x, 0) |]
    |> Array.filter (fun (_, n) -> n > 1)
    |> Array.map (fun (x, _) ->
        x.Label
    )
    |> Array.sort
    |> Array.iter (printfn "%s")
//    |> Array.length


GenPresProduct.get true
//|> Array.filter (fun gpp -> gpp.Shape |> String.toLower |> String.contains "concentraat" )
|> Array.collect (fun gpp -> gpp.GenericProducts)
|> Array.map (fun gp -> gp.Shape, gp.Substances[0].ShapeUnit)
|> Array.distinct
|> Array.map (fun (s, u) -> $"{s |> String.trim |> String.toLower}\t{u |> String.trim |> String.toLower}")
|> Array.iter (printfn "%s")


GenPresProduct.get true
|> Array.collect (fun gpp -> gpp.GenericProducts)
|> Array.map (fun gp -> gp.Id, gp.Name, gp.Shape, gp.Substances[0].ShapeUnit)
|> Array.filter (fun (_, _, s, u) -> s = "INFUUS" && u = "MILLIGRAM")


Names.getItems Names.Route Names.TwentyFive
|> Array.map snd
|> Array.zip (Names.getItems Names.Route Names.Fifty |> Array.map snd)
|> Array.map (fun (l, s) ->
    l
    |> String.replace "," "/"
    |> String.replace "/ " "/",
    s
    |> String.replace "," "/"
    |> String.replace "/ " "/"
    |> String.toLower
)
|> Array.collect (fun (l, s) ->
    s
    |> String.splitAt '/'
    |> Array.zip (l |> String.splitAt '/')
)
|> Array.distinct
|> Array.sort
|> Array.map (fun (l, s) -> $"{l}\t{s}")
|> Array.iter (printfn "%s")


