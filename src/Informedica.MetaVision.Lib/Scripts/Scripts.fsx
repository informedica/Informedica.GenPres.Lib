

#load "load.fsx"


#load "../MetaVision.fs"


open Informedica.Utils.Lib.BCL
open Informedica.ZIndex.Lib
open Informedica.MetaVision.Lib
open Informedica.ZIndex.Lib.ATCGroup
open Informedica.ZIndex.Lib.GenericProduct


MetaVision.getDrugFamilies "DrugFamilies.csv"


MetaVision.shapeUnits "ShapeUnits.csv"


MetaVision.createRoutes "Routes.csv"


MetaVision.createDoseForms "DoseForms.csv"


GenPresProduct.get true
(*
|> Array.filter (fun gpp ->
    let n = gpp.Name |> String.toLower |> String.trim
    n |> String.contains "cotri" ||
    n |> String.contains "amoxi" ||
    n |> String.contains "parac" ||
    n |> String.contains "norad" ||
    n |> String.contains "morfi"
)
*)
//|> Array.collect (fun gpp -> gpp.GenericProducts |> Array.map (fun gp -> gp.Name))
|> MetaVision.createMedications "Ingredients.csv" "Medications.csv" "ComplexMedications.csv" "Brands.csv" "Products.csv"
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
|> MetaVision.createSolutions "Solutions.csv"
|> Array.length


let rts1 =
    MetaVision.createRoutes "Routes.csv"
    |> Array.map (String.split "\t")
    |> Array.map List.toArray
    |> Array.map (Array.item 1)
    |> Array.skip 1

let rts2 =
    MetaVision.createDoseForms "DoseForms.csv"
    |> Array.map (String.split "\t")
    |> Array.map List.toArray
    |> Array.map (Array.item 2)
    |> Array.collect (String.splitAt ';')
    |> Array.skip 1
    |> Array.distinct

rts1 |> Array.length
rts2 |> Array.length

rts2 |> Array.forall (fun r -> rts1 |> Array.exists ((=) r))


let forms1 =
    MetaVision.createDoseForms "DoseForms.csv"
    |> Array.map (String.split "\t")
    |> Array.map List.toArray
    |> Array.map (Array.item 1)
    |> Array.skip 1
    |> Array.distinct

let meds =
    GenPresProduct.get true
    |> MetaVision.createMedications "Ingredients.csv" "Medications.csv" "ComplexMedications.csv" "Brands.csv" "Products.csv"

let forms2 =
    meds
    |> Array.map (String.split "\t")
    |> Array.map (List.toArray)
    |> Array.map (Array.item 9)
    |> Array.skip 1
    |> Array.distinct

// all forms in meds should exist in forms
forms2 |> Array.forall (fun f -> forms1 |> Array.exists ((=) f))

let rts3 =
    meds
    |> Array.map (String.split "\t")
    |> Array.map List.toArray
    |> Array.map (Array.item 10)
    |> Array.collect (String.splitAt ';')
    |> Array.skip 1
    |> Array.distinct

// all routes in meds should exists in routes
rts3 |> Array.forall (fun r ->
    let b = rts1 |> Array.exists ((=) r)
    if not b then printfn $"||{r}||"
    b
)

