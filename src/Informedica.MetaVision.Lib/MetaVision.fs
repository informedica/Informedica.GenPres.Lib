namespace Informedica.MetaVision.Lib

open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.ZForm.Lib.DoseRule.ShapeDosage
open Informedica.ZForm.Lib.Utils
open Informedica.ZIndex.Lib


type GenPresProduct = GenPresProduct.GenPresProduct
type GenericProduct = GenericProduct.GenericProduct


[<AutoOpen>]
module Utils =

    let mappingRouteShape =
        Web.getDataFromSheet "RouteShape2"


    let mappingFreqs =
        Web.getDataFromSheet "Frequencies"


    let mappingUnits =
        Web.getDataFromSheet "Units"


    let mappingShapeUnit =
        Web.getDataFromSheet "ShapeUnit"


    type OrderingType = Both | NonInfuse


    type Status = Active | Inactive | Retired


    [<Literal>]
    let NA = "NIET VAN TOEPASSING"

    let capitalize s =
            match s |> String.trim |> String.splitAt '-' with
            | [|s1; s2|] -> $"{s1 |> String.capitalize}-{s2 |> String.capitalize}"
            | _ -> s |> String.capitalize


    let isMultiple (gpp : GenPresProduct) =
        gpp.GenericProducts
        |> Array.exists (fun gp ->
            gp.Substances
            |> Array.map (fun s -> s.SubstanceName)
            |> Array.distinct
            |> Array.length > 1)


    let hasNoUnit (gpp : GenPresProduct) =
        gpp.GenericProducts
        |> Array.filter (fun gp ->
            gp.Substances
            |> Array.forall (fun s -> s.GenericUnit <> NA)
        )
        |> Array.isEmpty


    let isSolutionShape s =
        mappingRouteShape
        |> Array.filter (fun xs -> s |> String.toLower = xs[1] )
        |> Array.fold (fun acc xs ->
            if not acc then xs[3] = "TRUE"
            else acc
        ) false


    let shapeInDiluent un shape =
        mappingShapeUnit
        |> Array.tryFind (fun xs ->
            shape |> String.trim |> String.equalsCapInsens xs[0] &&
            un |> String.trim |> String.equalsCapInsens xs[1]
        )
        |> function
        | Some xs ->
            xs
            |> Array.item 2
            |> ((=) "TRUE")
        | None ->
            failwith $"cannot find unit: {un}, shape: {shape}"

    let shapeInSolution un shape =
        mappingShapeUnit
        |> Array.tryFind (fun xs ->
            shape |> String.trim |> String.equalsCapInsens xs[0] &&
            un |> String.trim |> String.equalsCapInsens xs[1]
        )
        |> function
        | Some xs ->
            xs
            |> Array.item 3
            |> ((=) "TRUE")
        | None ->
            failwith $"cannot find unit: {un}, shape: {shape}"


    let getATCCodes (gpp : GenPresProduct) =
        gpp.GenericProducts
        |> Array.map (fun gp -> gp.ATC |> String.trim)
        |> Array.distinct


    let mapUnit un =
        let un = un |> String.trim |> String.toLower
        mappingUnits
        |> Array.tryFind (fun r ->
            r[0] = un || r[1] = un
        )
        |> function
        | Some r -> r[2]
        | None   ->
            printfn $"cannot find {un} in mapping"
            ""


    let mapFreq freq =
        mappingFreqs
        |> Array.filter (fun r ->
            r[0] = freq
        )
        |> function
        | [||]   ->
            printfn $"cannot find {freq} in mapping"
            ""
        | xs ->
            xs
            |> Array.collect (fun r -> r[1..2])
            |> Array.filter (String.isNullOrWhiteSpace >> not)
            |> Array.distinct
            |> String.concat ";"


    let filter gpps =
        gpps
        |> Array.filter (hasNoUnit >> not)
        |> Array.map (fun gpp ->
            { gpp with
                GenericProducts =
                    gpp.GenericProducts
                    |> Array.filter (fun gp ->
                        gp.Substances
                        |> Array.forall (fun s -> s.SubstanceUnit <> NA)
                    )
            }
        )


    let removeEmptyUnitSubstances (gp : GenericProduct) =
        { gp with
            Substances =
                gp.Substances
                |> Array.filter (fun s ->
                    s.SubstanceUnit <> NA
                )
        }


    let getSynonyms (gp: GenericProduct) =
        GenPresProduct.get true
        |> Array.filter (fun gpp -> gpp.GenericProducts |> Array.exists ((=) gp))
        |> Array.collect (fun gpp ->
            gpp.GenericProducts
            |> Array.collect (fun gp ->
                gp.PrescriptionProducts
                |> Array.collect (fun pp ->
                    pp.TradeProducts
                    |> Array.map (fun tp -> tp.Brand)
                )
            )
        )
        |> Array.distinct


    let getFrequencies (id : int) =
        RuleFinder.createFilter
            None
            None
            None
            (Some id)
            ""
            ""
            ""
        |> RuleFinder.find true
        |> Array.map (fun dr -> $"{dr.Freq.Frequency} {dr.Freq.Time}")
        |> Array.distinct
        |> Array.map mapFreq
        |> Array.distinct
        |> String.concat ";"


    let print file xs =
        xs
        |> String.concat "\n"
        |> File.writeTextToFile $"data/output/{file}"

        xs


module MetaVision =

    let shapeUnits path =
        GenPresProduct.get true
        //|> Array.filter (fun gpp -> gpp.Shape |> String.toLower |> String.contains "concentraat" )
        |> Array.collect (fun gpp -> gpp.GenericProducts)
        |> Array.map (fun gp -> gp.Shape, gp.Substances[0].ShapeUnit)
        |> Array.distinct
        |> Array.map (fun (s, u) -> $"{s |> String.trim |> String.toLower}\t{u |> String.trim |> String.toLower}")
        |> print path


    let createRoutes path =
        // Get routes and external codes
        // Intra seperated by -
        Names.getItems Names.Route Names.Fifty
        |> Array.filter (fun (_, r) -> r.Contains(",") |> not)
        |> Array.sortBy fst
        |> Array.map (fun (id, r) ->
            {|
                ExternalCode = id
                RouteName =
                    r
                    |> capitalize
                    |> fun s ->
                        if s |> String.startsWith "Intra-" ||
                           s |> String.startsWith "Intra" |> not then s
                        else s |> String.replace "Intra" "Intra-" |> capitalize
                        |> String.replace "-" ""
                OrderType =
                    mappingRouteShape
                    |> Array.filter (fun xs -> r |> String.toLower = xs[0] )
                    |> Array.fold (fun acc xs ->
                        match acc with
                        | NonInfuse ->
                            if xs[2] |> String.contains "NoTime" then NonInfuse
                            else Both
                        | Both -> Both
                    ) NonInfuse
            |}
        )
        |> Array.map (fun r -> $"{r.ExternalCode}\t{r.RouteName}\t{r.OrderType}")
        |> Array.append [|
            [
                "ExternalCode"
                "RouteName"
                "OrderType"
            ] |> String.concat "\t"
        |]

        |> print path


    let createDoseForms path =
        // Get doseforms
        Names.getItems Names.Shape Names.Fifty
        |> Array.distinct
        |> Array.map (fun (id, s) ->
            {|
                ExternalCode = id
                DoseFormName = s |> String.capitalize
                Routes =
                    GenPresProduct.get true
                    |> Array.tryFind (fun gpp -> gpp.Shape = s)
                    |> function
                    | Some gpp -> gpp.Route
                    | None -> [||]
                OrderingType =
                    mappingRouteShape
                    |> Array.filter (fun xs -> s |> String.toLower = xs[1] )
                    |> Array.fold (fun acc xs ->
                        match acc with
                        | NonInfuse ->
                            if xs[2] |> String.contains "NoTime" then acc
                            else Both
                        | Both -> Both
                    ) NonInfuse
                IsDrugInSolution =
                    mappingRouteShape
                    |> Array.filter (fun xs -> s |> String.toLower = xs[1] )
                    |> Array.fold (fun acc xs ->
                        if not acc then xs[3] = "TRUE"
                        else acc
                    ) false
                Category = "G-Standaard"
                IsDispensibleAmountAllowed = false

            |}
        )
        |> Array.filter (fun r -> r.Routes |> Array.isEmpty |> not)
        |> Array.sortBy (fun r -> r.ExternalCode)

        |> Array.map (fun r ->
            let rs =
                r.Routes
                |> Array.map capitalize
                |> String.concat ";"
            $"{r.ExternalCode}\t{r.DoseFormName}\t{rs}\t{r.OrderingType}\t{r.IsDrugInSolution}\t{r.Category}\t{r.IsDispensibleAmountAllowed}"
        )
        |> Array.append [|
            [
                "ExternalCode"
                "DoseFormName"
                "Routes"
                "OrderType"
                "IsDrugInSolution"
                "Category"
                "IsDispensibleAmountAllowed"
            ] |> String.concat "\t"
        |]

        |> print path


    let createIngredients path (gpps : GenPresProduct[]) =
        let substs =
            gpps
            |> Array.collect (fun gpp -> gpp.GenericProducts)
            |> Array.collect (fun gp -> gp.Substances)

        // Ingredients
        substs
        |> Array.map (fun s ->
            s.SubstanceId,
            s.SubstanceName |> String.trim |> String.toLower,
            s.SubstanceUnit |> mapUnit
        )
        |> Array.distinctBy (fun (i, _, _) -> i)
        |> Array.filter (fun (_, s, u) ->
            if u |> String.isNullOrWhiteSpace then
                printfn $"{s} has no unit"
            u |> String.isNullOrWhiteSpace |> not
        )
        |> Array.sortBy (fun (_, s, _) -> s)
        |> Array.map (fun (c, s, u) -> $"{c}\t{s}\t{u}")
        |> fun ingrs ->
            substs
            |> Array.map (fun s ->
                let su = s.ShapeUnit |> mapUnit
                $"\t{su}\t{su}"
            )
            |> Array.distinct
            |> Array.append ingrs
        |> Array.append [|
            [
                "ExternalCode"
                "IngredientName"
                "Unit"
            ] |> String.concat "\t"
        |]
        |> print path
        |> ignore


    let createMedications optN pathIngr pathMed pathCompl =
        let meds =
            GenPresProduct.get true
            |> fun gpps ->
                if optN |> Option.isNone then gpps
                else
                    gpps
                    |> Array.take optN.Value
            |> fun gpps ->
                gpps |> createIngredients pathIngr
                gpps
            |> filter
            |> Array.collect (fun gpp -> gpp.GenericProducts)
            |> Array.map removeEmptyUnitSubstances
            |> Array.filter (fun gp -> gp.Substances |> Array.isEmpty |> not)
            |> Array.distinct
            |> Array.map (fun gp ->
                let n =
                    gp.Name
                    |> String.trim
                    |> String.toLower
                    |> String.replace "'" ""
                let g =
                    ATCGroup.get ()
                    |> Array.filter (fun g ->
                        g.ATC5
                        |> String.contains (gp.ATC |> String.subString 0 4)
                    )
                    |> Array.tryHead
                let su =
                    gp.Substances[0].ShapeUnit
                    |> mapUnit
                let un =
                    gp.Substances[0].SubstanceUnit
                    |> mapUnit

                {|
                    ExternalCode = $"GPK-{gp.Id}"
                    MedicationName = n
                    Unit = un
                    ATC =
                        gp
                        |> getSynonyms
                        |> Array.append [| gp.ATC |]
                        |> Array.map String.trim
                        |> Array.filter (String.isNullOrWhiteSpace >> not)
                        |> Array.distinct
                        |> Array.map (String.replace "'" "")
                        |> String.concat ", "
                    Status = "Active"
                    Format = "1,234.5 (Include Zero)"
                    IncrementValue = 0.1
                    CodeSnippetName = n
                    Frequencies =
                        let freqs = gp.Id |> getFrequencies
                        if freqs |> String.isNullOrWhiteSpace then "[All]"
                        else freqs
                    DoseForms = gp.Shape |> String.capitalize
                    Routes =
                        GenPresProduct.get true
                        |> Array.filter (fun gpp -> gpp.GenericProducts |> Array.exists ((=) gp))
                        |> Array.collect (fun gpp -> gpp.Route)
                        |> Array.distinct
                        |> Array.map capitalize
                        |> String.concat ";"
                    AdditivesGroup = "[None]"
                    DiluentsGroup =
                        if gp.Shape |> shapeInSolution gp.Substances[0].ShapeUnit then "Oplossingen"
                        else ""
                    DrugInDiluentGroup =
                        if gp.Shape |> shapeInDiluent gp.Substances[0].ShapeUnit then "Diluents"
                        else
                            "[None]"
                    DrugFamily = g |> Option.map (fun g -> g.AnatomicalGroup) |> Option.defaultValue ""
                    DrugSubFamily = g |> Option.map (fun g -> g.TherapeuticSubGroup) |> Option.defaultValue ""
                    IsFormulary = true
                    ComplexMedications =
                        if gp.Substances |> Array.length > 4 then [||]
                        else
                            let cms =
                                gp.Substances
                                |> Array.map (fun s ->
                                    {|
                                        ComplexMedictionName = n
                                        IngredientName =
                                            s.SubstanceName
                                            |> String.toLower
                                            |> String.trim
                                        Concentration = s.SubstanceQuantity
                                        ConcentrationUnit =
                                            s.SubstanceUnit
                                            |> mapUnit
                                        In = ""
                                        InUnit = ""
                                    |}
                                )

                            [|
                                {|
                                    ComplexMedictionName = n
                                    IngredientName = su
                                    Concentration = 1.
                                    ConcentrationUnit = su
                                    In = ""
                                    InUnit = ""
                                |}

                            |]
                            |> Array.append cms
                |}
            )
            |> Array.sortBy (fun r -> r.MedicationName)

        meds
        |> Array.collect (fun r -> r.ComplexMedications)
        |> Array.map (fun r ->
            [
                r.ComplexMedictionName
                r.IngredientName
                $"{r.Concentration |> Double.toStringNumberNLWithoutTrailingZeros}"
                r.ConcentrationUnit
                r.In
                r.InUnit
            ]
            |> String.concat "\t"
        )
        |> Array.append
            ([|
                "ComplexMedicationName"
                "IngredientName"
                "Concentration"
                "ConcentrationUnit"
                "In"
                "InUnit"
            |]
            |> String.concat "\t"
            |> Array.singleton)
        |> print pathCompl
        |> ignore

        meds
        |> Array.map (fun r ->
            [
                r.ExternalCode
                r.MedicationName
                r.Unit
                r.ATC
                r.Status
                r.Format
                $"{r.IncrementValue |> Double.toStringNumberNLWithoutTrailingZeros}"
                r.CodeSnippetName
                r.Frequencies
                r.DoseForms
                r.Routes
                r.AdditivesGroup
                r.DiluentsGroup
                r.DrugInDiluentGroup
                r.DrugFamily
                r.DrugSubFamily
                $"""{if r.IsFormulary then "TRUE" else "FALSE"}"""

            ]
            |> String.concat "\t"
        )
        |> Array.append
            ([|
                "ExternalCode"
                "MedicationName"
                "Unit"
                "ATC"
                "Status"
                "Format"
                "IncrementValue"
                "CodeSnippetName"
                "Frequencies"
                "DoseForms"
                "Routes"
                "AdditivesGroup"
                "DiluentsGroup"
                "DrugInDiluentGroup"
                "DrugFamily"
                "DrugSubFamily"
                "IsFormulary"
            |]
            |> String.concat "\t"
            |> Array.singleton)
        |> print pathMed



