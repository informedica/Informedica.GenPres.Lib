
#load "load.fsx"

open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.ZForm.Lib.Utils
open Informedica.ZIndex.Lib

type GenPresProduct = GenPresProduct.GenPresProduct

let mapping =
    Web.getDataFromSheet "RouteShape2"


type OrderingType = Both | NonInfuse


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


let getATCCodes (gpp : GenPresProduct) =
    gpp.GenericProducts
    |> Array.map (fun gp -> gp.ATC |> String.trim)
    |> Array.distinct


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
            mapping
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
            mapping
            |> Array.filter (fun xs -> s |> String.toLower = xs[1] )
            |> Array.fold (fun acc xs ->
                match acc with
                | NonInfuse ->
                    if xs[2] |> String.contains "NoTime" then acc
                    else Both
                | Both -> Both
            ) NonInfuse
        IsDrugInSolution =
            mapping
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


let getMedication () =
    GenPresProduct.get true
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
    |> Array.groupBy (fun gpp ->

        {|
            MedicationName =
                let n = gpp.Name |> String.trim |> String.toLower
                if not (gpp |> isMultiple) then n
                else
                    $"{n} {gpp.Shape |> String.trim |> String.toLower}"
            IsMultiple = gpp |> isMultiple
        |}
    )
    |> Array.map (fun (r, gpps) ->
        let gps = gpps |> Array.collect (fun gpp -> gpp.GenericProducts)
        let substs = gps |> Array.collect (fun gp -> gp.Substances)
        let su () =
            substs
            |> Array.map (fun s -> s.ShapeUnit)
            |> Array.fold (fun acc su ->
                if acc |> String.isNullOrWhiteSpace then su
                else
                    if acc = su then acc
                    else
                        printfn $"{r.MedicationName} {r.IsMultiple} shape unit diff {acc} {su}"
                        su
            ) ""
            |> String.trim |> String.toLower

        {| r with
            ExternalCode = ""
            Unit =
                if r.IsMultiple then su ()
                else
                    substs
                    |> Array.map (fun s -> s.GenericUnit)
                    |> Array.fold (fun acc u ->
                        if acc |> String.isNullOrWhiteSpace then u
                        else
                            if acc = u then acc
                            else
                                printfn $"{r.MedicationName} diff units {acc} - {u}"
                                u

                    ) ""
                    |> String.trim |> String.toLower
            ATC =
                gpps
                |> Array.collect getATCCodes
                |> String.concat ", "
            Status = "Active"
            Format = "1,234.56"
            IncrementValue = 1
            CodeSnippetName = r.MedicationName
            Frequencies = "[All]"
            DoseForms =
                gpps
                |> Array.map (fun gpp -> gpp.Shape)
                |> Array.distinct
                |> Array.map capitalize
                |> String.concat ";"
            Routes =
                gpps
                |> Array.collect (fun gpp -> gpp.Route)
                |> Array.distinct
                |> Array.map capitalize
                |> String.concat ";"
        |}
    )
    |> Array.sortBy (fun r -> r.MedicationName)
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
        ]
        |> String.concat "\t"
    )
    |> print "Medication.csv"





GenPresProduct.get true
|> Array.collect (fun gpp -> gpp.GenericProducts)
|> Array.filter (fun gp ->
    gp.Substances
    |> Array.exists (fun s -> s.GenericUnit = "NIET VAN TOEPASSING")
)
|> Array.map (fun gp -> $"{gp.Id}, {gp.Name}")
|> Array.iteri (printfn "%i. %s")
