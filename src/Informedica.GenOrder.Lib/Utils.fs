namespace Informedica.GenOrder.Lib


[<AutoOpen>]
module Utils =

    open System
    open System.IO
    open System.Net.Http

    open Informedica.Utils.Lib.BCL


    module Csv =


        type DataType =
            | StringData
            | FloatData
            | FloatOptionData

        let tryCast dt (x: string) =
            match dt with
            | StringData -> box (x.Trim())
            | FloatData ->
                match Double.TryParse(x) with
                | true, n -> n |> box
                | _ ->
                    $"cannot parse {x} to double"
                    |> failwith
            | FloatOptionData ->
                match Double.TryParse(x) with
                | true, n -> n |> Some |> box
                | _ -> None |> box


        let getColumn dt columns sl s =
            columns
            |> Array.tryFindIndex ((=) s)
            |> function
                | None ->
                    $"""cannot find column {s} in {columns |> String.concat ", "}"""
                    |> failwith
                | Some i ->
                    sl
                    |> Array.item i
                    |> tryCast dt


        let getStringColumn columns sl s =
            getColumn StringData columns sl s |> unbox<string>


        let getFloatColumn columns sl s =
            getColumn FloatData columns sl s |> unbox<float>


        let getFloatOptionColumn columns sl s =
            getColumn FloatOptionData columns sl s
            |> unbox<float option>


        let parseCSV (s: string) =
            s.Split("\n")
            |> Array.filter (String.isNullOrWhiteSpace >> not)
            |> Array.map (String.replace "\",\"" "|")
            |> Array.map (String.replace "\"" "")
            |> Array.map (fun s ->
                s.Split("|")
                |> Array.map (fun s -> s.Trim())
            )


    module Web =


        let createUrl sheet id =
            $"https://docs.google.com/spreadsheets/d/%s{id}/gviz/tq?tqx=out:csv&sheet=%s{sheet}"

        //https://docs.google.com/spreadsheets/d/1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g/edit?usp=sharing
        let [<Literal>] constraints = "1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g"


        //https://docs.google.com/spreadsheets/d/1ccW2b6vYVZQsyb8TGpZtJoqtllCbmwrZiIFZ7BN1sB4/edit?usp=sharing
        let [<Literal>] genpres = "1ccW2b6vYVZQsyb8TGpZtJoqtllCbmwrZiIFZ7BN1sB4"


        let client = new HttpClient()


        let download url =
            async {
                use! resp = client.GetAsync(Uri(url)) |> Async.AwaitTask
                use! stream = resp.Content.ReadAsStreamAsync() |> Async.AwaitTask
                use reader = new StreamReader(stream)
                return reader.ReadToEnd()
            }


        let getDataFromExcelSheet excel sheet =
            createUrl sheet excel
            |> download
            |> Async.RunSynchronously
            |> Csv.parseCSV



        let getDataFromConstraints = getDataFromExcelSheet constraints


        let getDataFromGenPres = getDataFromExcelSheet genpres



    module Data =

        open Informedica.GenOrder.Lib


        let toBrs s =
            s
            |> String.splitAt ';'
            |> Array.choose Double.tryParse
            |> Array.choose BigRational.fromFloat


        let toBrOpt brs = brs |> Array.tryHead


        let tupleBrOpt brs1 brs2 =
            brs1 |> Array.tryHead,
            brs2 |> Array.tryHead


        let getSolutions () =
            Web.getDataFromConstraints "Solutions"
            |> fun data ->
                let getColumn =
                    data
                    |> Array.head
                    |> Csv.getStringColumn

                data
                |> Array.tail
                |> Array.map (fun r ->
                    let get = getColumn r
                    {|
                        Medication = get "Medication"
                        MinAge = get "MinAge" |> toBrs
                        MaxAge = get "MaxAge" |> toBrs
                        MinWeight = get "MinWeight" |> toBrs
                        MaxWeight = get "MaxWeight" |> toBrs
                        Solutions = get "Solutions" |> String.split ";"
                        SolQuantities = get "SolQuantities" |> toBrs
                        RateUnit = get "RateUnit"
                        DoseCount = get "DoseCount" |> toBrs
                        SubstName = get "SubstName"
                        SubstQuantities = get "SubstQuantities" |> toBrs
                        MinConcentration = get "MinConcentration" |> toBrs
                        MaxConcentration = get "MaxConcentration" |> toBrs
                    |}
                )
                |> Array.groupBy (fun r ->
                    {
                        Medication = r.Medication
                        Age = tupleBrOpt r.MinAge r.MaxAge
                        Weight = tupleBrOpt r.MinWeight r.MaxWeight
                        Solutions = r.Solutions
                        Quantities = r.SolQuantities |> Array.toList
                        RateUnit = r.RateUnit
                        DoseCount = r.DoseCount |> Array.toList
                        Limits = []
                    }
                )
                |> Array.map (fun (sr, rs) ->
                    { sr with
                        Limits =
                            rs
                            |> Array.map (fun r ->
                                let minC, maxC = tupleBrOpt r.MinConcentration r.MaxConcentration
                                {
                                    SubstanceName = r.SubstName
                                    Quantities = r.SubstQuantities |> Array.toList
                                    MinConcentration = minC
                                    MaxConcentration = maxC
                                }
                            )
                            |> Array.toList

                    }
                )
                |> Array.toList


        let getDoseRules () =
            Web.getDataFromConstraints "DoseRules"
            |> fun data ->
                let getColumn =
                    data
                    |> Array.head
                    |> Csv.getStringColumn

                data
                |> Array.tail
                |> Array.map (fun r ->
                    let get = getColumn r
                    let toBrOpt = toBrs >> toBrOpt
                    {|
                        Indication = get "Indication"
                        Medication = get "Medication"
                        Shape = get "Shape"
                        Route = get "Route"
                        MinAge = get "MinAge" |> toBrOpt
                        MaxAge = get "MaxAge" |> toBrOpt
                        MinWeight = get "MinWeight" |> toBrOpt
                        MaxWeight = get "MaxWeight" |> toBrOpt
                        MinGest = get "MinGest" |> toBrOpt
                        MaxGest = get "MaxGest" |> toBrOpt
                        MinPost = get "MinPost" |> toBrOpt
                        MaxPost = get "MaxPost" |> toBrOpt
                        OrderType = get "OrderType"
                        Frequencies = get "Frequencies" |> toBrs
                        Rates = get "Rates" |> toBrs
                        MinTime = get "MinTime" |> toBrOpt
                        MaxTime = get "MaxTime" |> toBrOpt
                        DoseUnit = get "DoseUnit"
                        AdjUnit = get "AdjUnit"
                        TimeUnit = get "TimeUnit"
                        RateUnit = get "RateUnit"
                        Substance = get "Substance"
                        MinDoseQty = get "MinDoseQty" |> toBrOpt
                        MaxDoseQty = get "MaxDoseQty" |> toBrOpt
                        MinDoseQtyAdj = get "MinDoseQtyAdj" |> toBrOpt
                        MaxDoseQtyAdj = get "MaxDoseQtyAdj" |> toBrOpt
                        MinDoseTot = get "MinDoseTot" |> toBrOpt
                        MaxDoseTot = get "MaxDoseTot" |> toBrOpt
                        MinDoseTotAdj = get "MinDoseTotAdj" |> toBrOpt
                        MaxDoseTotAdj = get "MaxDoseTotAdj" |> toBrOpt
                        MinDoseRate = get "MinDoseRate" |> toBrOpt
                        MaxDoseRate = get "MaxDoseRate" |> toBrOpt
                        MinDoseRateAdj = get "MinDoseRateAdj" |> toBrOpt
                        MaxDoseRateAdj = get "MaxDoseRateAdj" |> toBrOpt
                    |}
                )
                |> Array.groupBy (fun r ->
                    {
                        Indication = r.Indication
                        Medication = r.Medication
                        Shape = r.Shape
                        Route = r.Route
                        Age = r.MinAge, r.MaxAge
                        Weight = r.MinWeight, r.MaxWeight
                        GestAge = r.MinGest, r.MaxGest
                        PostAge = r.MinPost, r.MaxPost
                        OrderType =  r.OrderType
                        Frequencies = r.Frequencies |> Array.toList
                        Rates = r.Rates |> Array.toList
                        MinTime = r.MinTime
                        MaxTime = r.MaxTime
                        DoseUnit = r.DoseUnit
                        AdjUnit = r.AdjUnit
                        TimeUnit = r.TimeUnit
                        RateUnit = r.RateUnit
                        Limits = []
                        Quantities = []
                    }
                )
                |> Array.map (fun (dr, rs) ->
                    { dr with
                        Limits =
                            rs
                            |> Array.map (fun r ->
                                {
                                    SubstanceName = r.Substance
                                    MinDoseQuantity = r.MinDoseQty
                                    MaxDoseQuantity = r.MaxDoseQty
                                    MinDoseQuantityAdjust = r.MinDoseQtyAdj
                                    MaxDoseQuantityAdjust = r.MaxDoseQtyAdj
                                    MinDosePerTime = r.MinDoseTot
                                    MaxDosePerTime = r.MaxDoseTot
                                    MinDosePerTimeAdjust = r.MinDoseTotAdj
                                    MaxDosePerTimeAdjust = r.MaxDoseTotAdj
                                    MinDoseRate = r.MinDoseRate
                                    MaxDoseRate = r.MaxDoseRate
                                    MinDoseRateAdjust = r.MinDoseRateAdj
                                    MaxDoseRateAdjust = r.MaxDoseRateAdj
                                }
                            )
                            |> Array.toList
                    }
                )
                |> Array.toList


        let getProducts () =
            Web.getDataFromConstraints "Products"
            |> fun data ->
                let getColumn =
                    data
                    |> Array.head
                    |> Csv.getStringColumn

                data
                |> Array.tail
                |> Array.map (fun r ->
                    let get = getColumn r
                    let toBrOpt = toBrs >> toBrOpt

                    {|
                        Name = get "Name"
                        Shape = get "Shape"
                        Divisible = "Divisible" |> toBrOpt
                        Unit = get "Unit"
                        Quantities = get "Quantities" |> toBrs
                        SubstanceName = get "SubstanceName"
                        SubstanceConcentrations = get "SubstanceConcentrations" |> toBrs
                        SubstanceUnit = get "SubstanceUnit"
                    |}
                )
                |> Array.groupBy (fun r ->
                    {
                        Name = r.Name
                        Shape = r.Shape
                        Unit = r.Unit
                        Divisible = r.Divisible
                        Quantities = r.Quantities |> Array.toList
                        Substances = []
                    }
                )
                |> Array.map (fun (p, rs) ->
                    { p with
                        Substances =
                            rs
                            |> Array.map (fun r ->
                                {
                                    Name = r.SubstanceName
                                    Unit = r.SubstanceUnit
                                    Concentrations = r.SubstanceConcentrations |> Array.toList
                                    Quantities = []
                                }
                            )
                            |> Array.toList
                    }
                )
                |> Array.toList

