
#load "load.fsx"


open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.ZIndex.Lib
open Informedica.ZForm.Lib


File.exists "data/cache/README.md"


// List all products that have no app route
GenPresProduct.get true
|> Seq.collect (fun gpp ->
    gpp.GenericProducts
    |> Seq.collect (fun gp ->
        gp.Route
        |> Seq.map (fun r ->
            gpp.Name, gp.Id, r |> Mapping.mapRoute Mapping.GStandMap Mapping.AppMap, r
        )
    )
)
|> Seq.filter (fun (_, _, rt, _) -> rt = "")
|> Seq.map (fun (n, _,_, r) -> n, r)
|> Seq.distinct
|> Seq.sort
|> Seq.iter (printfn "%A")


// Process all possible dtos
GenPresProduct.get true
|> Seq.collect (fun gpp ->
    gpp.GenericProducts
    |> Seq.collect (fun gp ->
        gp.Route
        |> Seq.map (fun r ->
            gpp.Name, gp.Id, r |> Mapping.mapRoute Mapping.GStandMap Mapping.AppMap
        )
    )
)
|> Seq.sortBy (fun (gen, _, _) -> gen)
|> Seq.map (fun (gen, id, rt) ->
    printfn "processing %i" id
    {   Dto.dto with
            AgeInMo = 300.
            WeightKg = 80.
            LengthCm = 180.
            GPK = id
            Route = rt
            IsRate = false
    } |> Dto.processDto, gen
)
|> Seq.filter (fun (dto, _) ->
    dto.Rules |> Seq.isEmpty
)
|> Seq.map snd
|> Seq.distinct
|> Seq.toList
|> (fun xs -> xs |> List.length |> printfn "count: %i"; xs)
|> List.iter (printfn "%s")


// BigRational parse failure for: 117714
GenPresProduct.get true
|> Seq.collect (fun gpp ->
    gpp.GenericProducts
    |> Seq.collect (fun gp ->
        gp.Route
        |> Seq.map (fun r ->
            gpp.Name, gp.Id, r |> Mapping.mapRoute Mapping.GStandMap Mapping.AppMap
        )
    )
)
|> Seq.filter (fun (_, id, _) -> id = 117714)
|> Seq.map (fun (gen, id, rt) ->
    printfn "%s" gen
    {   Dto.dto with
            AgeInMo = 300.
            WeightKg = 80.
            LengthCm = 180.
            GPK = id
            Route = rt
            IsRate = false
    } |> Dto.processDto, gen
)
|> Seq.toList


// Write all possible Generic Products to a file
let createFormularium () =
    let headers =
        [
            "GPK"
            "ATC"
            "MainGroup"
            "SubGroup"
            "Generic"
            "Product"
            "Label"
            "Shape"
            "Route"
            "Quantity"
            "QuantityUnit"
            "Multiple"
            "Unit"
        ]

    GenPresProduct.get true
    |> Array.collect (fun gpp ->
        gpp.GenericProducts
        |> Array.collect (fun gp ->
            gp.PrescriptionProducts
            |> Array.collect (fun pp ->
                pp.TradeProducts
                |> function
                | _ when pp.TradeProducts |> Array.isEmpty -> "" |> Array.singleton
                | _ -> pp.TradeProducts
                       |> Array.map (fun tp -> tp.Label)
                |> Array.collect (fun prd ->
                    gp.Route
                    |> function
                    | _ when gp.Route |> Array.isEmpty -> "" |> Array.singleton
                    | _ -> gp.Route
                    |> Array.map (fun rte ->
                        let gpk = gp.Id |> string
                        let atc = gp.ATC.Trim()
                        let grp =
                            ATCGroup.get ()
                            |> Array.tryFind (fun g ->
                                g.ATC5.Trim() |> String.startsWith atc ||
                                atc |> String.startsWith (g.ATC5.Trim())
                            )
                        let mng =
                            match grp with
                            | Some x -> x.TherapeuticMainGroup
                            | None -> ""
                        let sbg =
                            match grp with
                            | Some x -> x.TherapeuticSubGroup
                            | None -> ""
                        let gen = gpp.Name
                        let lbl = gp.Label
                        let shp = gpp.Shape
                        let qty, qun, mun =
                            if gp.Substances |> Array.length = 1 |> not then "", "", ""
                            else
                                let subst =
                                    (gp.Substances |> Array.head)
                                (sprintf "%A" subst.SubstanceQuantity),
                                (sprintf "%s/%s" subst.SubstanceUnit subst.ShapeUnit),
                                subst.SubstanceUnit
                        let qty = qty |> String.replace "." ","
                        let mqt = qty
                        [ gpk; atc; mng; sbg; gen; prd; lbl; shp; rte; qty; qun; mqt; mun ]
                        |> String.concat ";"
                    )
                )
            )
        )
    )
    |> Array.append (headers |> String.concat ";" |> Array.singleton)


// Each entry should have atc groups
createFormularium ()
|> Array.filter (fun r ->
    let xs = r |> String.splitAt ';'
    xs.[2] = "" || xs.[3] = ""
)

// Write all possible Generic Products to a file
let writeFormulariumToFile file =
    createFormularium ()
    |> String.concat "\n"
    |> File.writeTextToFile file


writeFormulariumToFile "formularium.csv"

ATCGroup.get ()
|> Array.map (fun g ->
    g.ATC5, g.TherapeuticMainGroup
)
|> Array.sortBy fst
|> Array.iter (printfn "%A")

// Check if there are GenPresProducts
// without GenericProducts, shouldn't be
GenPresProduct.get true
|> Array.exists (fun gpp ->
    gpp.GenericProducts |> Array.isEmpty
)

// Check whether there are GenericProducts
// without PrescriptionProducts
GenPresProduct.get true
|> Array.exists (fun gpp ->
    gpp.GenericProducts
    |> Array.exists (fun gp ->
        gp.PrescriptionProducts |> Array.isEmpty
    )
)


// Get the PrescriptionProducts
// without TradeProducts
GenPresProduct.get true
|> Array.collect (fun gpp ->
    gpp.GenericProducts
    |> Array.collect (fun gp ->
        gp.PrescriptionProducts
        |> Array.filter (fun pp ->
            pp.TradeProducts
            |> Array.isEmpty
        )
    )
)


GenPresProduct.get true
|> Array.filter (fun gpp ->
    gpp.Name |> String.toLower |> String.contains "meropenem"
)

GenericProduct.get []
|> Array.filter (fun gp ->
    gp.Name |> String.toLower |> String.contains "cidofovir"
)

GenPresProduct.get true |> ignore
ATCGroup.load ()
(fun () -> printfn "Loading dose rules.. "; DoseRule.load (); printfn "ready loading doserules") ()


GenPresProduct.get true
|> Array.filter (fun gpp ->
    gpp.Name |> String.toLower |> String.contains "argipressine"
)
|> Array.collect (fun gpp ->
    gpp.GenericProducts
    |> Array.map (fun gp -> gp.Id, gp.Label, gp.Route)
)


GenPresProduct.get true
|> Array.filter (fun gpp ->
    gpp.Name |> String.toLower |> String.contains "insuline"
)
|> Array.collect (fun gpp ->
    gpp.GenericProducts
    |> Array.map (fun gp -> gp.Id, gp.Label, gp.Route, gp.Substances[0].SubstanceUnit)
)


GenPresProduct.get true
|> Array.filter (fun gpp ->
    gpp.GenericProducts
    |> Array.exists (fun gp -> gp.Id = 175552)
)
|> Array.collect (fun gpp ->
    gpp.GenericProducts
    |> Array.map (fun gp -> gp.Id, gp.Label, gp.Route)
)


{ Dto.dto with
    AgeInMo = 240.
    WeightKg = 30.
    GPK = 00161527
}
|> Dto.processDto
|> fun dto -> dto.Text


"rect"
|> Mapping.mapRoute Mapping.AppMap Mapping.GStandMap


RuleFinder.createFilter None None None (Some 00161527) "" "" ""
|> RuleFinder.find true
|> Array.map DoseRule.toString2



Mapping.MappingTests.tests ()
