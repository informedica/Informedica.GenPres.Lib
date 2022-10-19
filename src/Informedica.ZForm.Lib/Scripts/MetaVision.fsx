
#load "load2.fsx"

open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.ZForm.Lib.DoseRule.ShapeDosage
open Informedica.ZForm.Lib.Utils
open Informedica.ZIndex.Lib


type GenPresProduct = GenPresProduct.GenPresProduct
type GenericProduct = GenericProduct.GenericProduct

let mappingRouteShape =
    Web.getDataFromSheet "RouteShape2"


let mappingFreqs =
    Web.getDataFromSheet "Frequencies"


let mappingUnits =
    Web.getDataFromSheet "Units"


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
        { gpp
            with GenericProducts =
                gpp.GenericProducts
                |> Array.filter (fun gp ->
                    gp.Substances
                    |> Array.forall (fun s -> s.SubstanceUnit <> NA)
                )
        }
    )



let print file xs =
    xs
    |> String.concat "\n"
    |> File.writeTextToFile $"data/output/{file}"

    xs



// Get routes and external codes
Names.getItems Names.Route Names.Fifty
|> Array.filter (fun (_, r) -> r.Contains(",") |> not)
|> Array.sortBy fst
|> Array.map (fun (id, r) ->
    {|
        ExternalCode = id
        RouteName = r |> capitalize
        OrderType =
            mappingRouteShape
            |> Array.filter (fun xs -> r |> String.toLower = xs[0] )
            |> Array.fold (fun acc xs ->
                match acc with
                | NonInfuse ->
                    if xs[2] |> String.contains "NoTime" then acc
                    else Both
                | Both -> Both
            ) NonInfuse
    |}
)
|> Array.iter (fun r -> printfn $"{r.ExternalCode}: {r.RouteName}, {r.OrderType}")


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

|> Array.iter (fun r ->
    let rs =
        r.Routes
        |> Array.map capitalize
        |> String.concat ";"
    $"{r.ExternalCode}, {r.DoseFormName}, {rs}, {r.OrderingType}, {r.IsDrugInSolution}, {r.Category}, {r.IsDispensibleAmountAllowed}"
    |> printfn "%s"
)


// Ingredients
GenPresProduct.get true
|> filter
|> Array.collect (fun gpp -> gpp.GenericProducts)
|> Array.collect (fun gp -> gp.Substances)
|> Array.map (fun s ->
    s.SubstanceId,
    s.SubstanceName |> String.trim |> String.toLower,
    s.SubstanceUnit |> mapUnit
)
|> Array.distinct
|> Array.filter (fun (_, s, u) ->
    if u |> String.isNullOrWhiteSpace then
        printfn $"{s} hasn no unit"
    u |> String.isNullOrWhiteSpace |> not
)
|> Array.sortBy (fun (_, s, _) -> s)
|> Array.map (fun (c, s, u) -> $"{c}\t{s}\t{u}")
|> Array.append [|
    [
        "ExternalCode"
        "IngredientName"
        "Unit"
    ] |> String.concat "\t"
|]
|> print "Ingredients.csv"


let removeEmptyUnitSubstances (gp : GenericProduct) =
    { gp with
        Substances =
            gp.Substances
            |> Array.filter (fun s ->
                s.SubstanceUnit <> NA
            )
    }


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


let getMedication () =
    let meds =
        GenPresProduct.get true
        |> filter
        |> Array.collect (fun gpp -> gpp.GenericProducts)
        |> Array.map removeEmptyUnitSubstances
        |> Array.filter (fun gp -> gp.Substances |> Array.isEmpty |> not)
        |> Array.distinct
        |> Array.map (fun gp ->
            let n = gp.Label |> String.trim |> String.toLower
            let g =
                ATCGroup.get ()
                |> Array.filter (fun g ->
                    g.ATC5
                    |> String.toLower
                    |> String.contains (gp.ATC |> String.toLower |> String.trim)
                )
                |> Array.tryHead
            let su =
                if gp.Shape |> isSolutionShape then "milliliter"
                else gp.Substances[0].ShapeUnit
                |> mapUnit

            {|
                ExternalCode = $"GPK-{gp.Id}"
                MedicationName = n
                Unit = su

                ATC =
                    gp.PrescriptionProducts
                    |> Array.collect (fun pp ->
                        pp.TradeProducts
                        |> Array.map (fun tp -> tp.Brand)
                    ) |> Array.append [| gp.ATC |]
                    |> Array.map String.trim
                    |> Array.filter (String.isNullOrWhiteSpace >> not)
                    |> Array.distinct
                    |> String.concat ", "
                Status = "Active"
                Format = "1,234.56"
                IncrementValue = 1
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
                    if gp.Shape |> isSolutionShape then "Oplossingen"
                    else ""
                DrugInDiluentGroup =
                    if gp.Shape |> isSolutionShape then "[None]"
                    else ""
                DrugFamily = g |> Option.map (fun g -> g.AnatomicalGroup) |> Option.defaultValue ""
                DrugSubFamily = g |> Option.map (fun g -> g.TherapeuticMainGroup) |> Option.defaultValue ""
                IsFormulary = true
                ComplexMedications =
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
                            In = 1.
                            InUnit = su
                        |}
                    )
            |}
        )
        |> Array.sortBy (fun r -> r.MedicationName)

    meds
    |> Array.collect (fun r -> r.ComplexMedications)
    |> Array.map (fun r ->
        [
            r.ComplexMedictionName
            r.IngredientName
            $"%f{r.Concentration}"
            r.ConcentrationUnit
            $"%f{r.In}"
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
    |> print "ComplexMedications.csv"
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
            $"%i{r.IncrementValue}"
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
    |> print "Medications.csv"







GenPresProduct.get true
|> Array.collect (fun gpp -> gpp.GenericProducts)
|> Array.filter (fun gp ->
    gp.Substances
    |> Array.exists (fun s -> s.GenericUnit = "NIET VAN TOEPASSING")
)
|> Array.map (fun gp -> $"{gp.Id}, {gp.Name}")
|> Array.iteri (printfn "%i. %s")


GenPresProduct.get true
|> Array.filter (fun gpp -> gpp.Name |> String.length > 100)


Substance.get ()
|> Array.take 100
