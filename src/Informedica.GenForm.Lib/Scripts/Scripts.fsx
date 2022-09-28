
#time

#load "load.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


[<AutoOpen>]
module Utils =

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
            |> Array.map (String.replace "\",\"" "")
            |> Array.map (String.replace "\"" "")
            |> Array.map (fun s ->
                s.Split("")
                |> Array.map (fun s -> s.Trim())
            )


    module Web =

        let createUrl sheet id =
            $"https://docs.google.com/spreadsheets/d/{id}/gviz/tq?tqx=out:csv&sheet={sheet}"

        //https://docs.google.com/spreadsheets/d/1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g/edit?usp=sharing
        [<Literal>]
        let dataUrlId = "1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g"

        let client = new HttpClient()

        let download url =
            async {
                use! resp = client.GetAsync(Uri(url)) |> Async.AwaitTask
                use! stream = resp.Content.ReadAsStreamAsync() |> Async.AwaitTask
                use reader = new StreamReader(stream)
                return reader.ReadToEnd()
            }


        let getDataFromSheet sheet =
            createUrl sheet dataUrlId
            |> download
            |> Async.RunSynchronously
            |> Csv.parseCSV


    module BigRational =

        open MathNet.Numerics


        let toStringNl (br : BigRational) =
            if br.Denominator = 1I then
                br |> BigRational.ToInt32 |> Int32.toStringNumberNL
            else
                br |> BigRational.toFloat |> Double.toStringNumberNLWithoutTrailingZeros



[<AutoOpen>]
module Types =

    open MathNet.Numerics

    type Substance =
        {
            Name : string
            Unit : string
            Quantity : BigRational option
            MultipleQuantity : BigRational option
            MultipleUnit : string
        }


    type Product =
        {
            GPK : string
            ATC : string
            MainGroup : string
            SubGroup : string
            Generic : string
            TallMan : string
            Synonyms : string array
            Product : string
            Label : string
            Shape : string
            ShapeQuantity : BigRational option
            ShapeUnit : string
            Divisible : BigRational option
            Substances : Substance array
        }


    type Gender = Male | Female | AnyGender


    type MinMax = { Minimum : BigRational option; Maximum : BigRational option }


    type OrderType = | Continuous | DisContinuous | Timed | Once | AnyOrder


    type Frequency = { Count : BigRational; TimeUnit : string }


    type DoseLimit =
        {
            Substance : string
            NormDoseQuantity : BigRational option
            DoseQuantity : MinMax
            NormDoseQuantityAdjust : BigRational option
            DoseQuantityAdjust : MinMax
            NormDoseTotal : BigRational option
            DoseTotal : MinMax
            NormDoseTotalAdjust : BigRational option
            DoseTotalAdjust : MinMax
            NormDoseRate : BigRational option
            DoseRate : MinMax
            NormDoseRateAdjust : BigRational option
            DoseRateAdjust : MinMax
        }


    type Patient =
        {
            Gender : Gender
            Age : MinMax
            Weight : MinMax
            GestAge : MinMax
            PMAge : MinMax
        }


    type DoseRule =
        {
            Indication : string
            Generic : string
            Shape : string
            Route : string
            Patient : Patient
            OrderType : OrderType
            Frequencies : BigRational array
            Rates : BigRational array
            DoseUnit : string
            AdjustUnit : string
            TimeUnit : string
            RateUnit : string
            Time : MinMax
            DoseLimits : DoseLimit array
        }


module OrderType =

    open Informedica.Utils.Lib.BCL

    let fromString s =
        let s = s |> String.toLower |> String.trim

        match s with
        | "cont" -> Continuous
        | "discont" -> DisContinuous
        | "timed" -> Timed
        | "once" -> Once
        | _ ->
            printfn $"couldn't match {s}"
            AnyOrder


    let toString = function
        | Continuous -> "cont"
        | DisContinuous -> "discont"
        | Timed -> "timed"
        | Once -> "once"
        | AnyOrder -> ""


module Gender =

    open Informedica.Utils.Lib.BCL

    let fromString s =
        let s = s |> String.toLower |> String.trim
        match s with
        | "man" -> Male
        | "vrouw" -> Female
        | _ -> AnyGender


    let toString = function
        | Male -> "man"
        | Female -> "vrouw"
        | AnyGender -> ""



module MinMax =

    let fromTuple (min, max) =
        {
            Minimum = min
            Maximum = max
        }


    let toString { Minimum = min; Maximum = max } =
        let min = min |> Option.map BigRational.toStringNl
        let max = max |> Option.map BigRational.toStringNl

        match min, max with
        | None, None -> ""
        | Some min, None -> $"≥ {min}"
        | Some min, Some max -> $"{min} - {max}"
        | None, Some max -> $"< {max}"



module Patient =

    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL


    let sortBy (pat : Patient) =
        let toInt = function
            | Some x -> x |> BigRational.ToInt32
            | None -> 0

        (pat.Age.Minimum |> toInt |> fun i -> if i > 0 then i + 300 else i) +
        (pat.Weight.Minimum |> toInt) +
        (pat.GestAge.Minimum |> toInt) +
        (pat.PMAge.Minimum |> toInt)


    let printAge a =
        let a = a |> BigRational.ToInt32
        match a with
        | _ when a < 7 ->
            if a = 1 then $"%i{a} dag"
            else $"%i{a} dagen"
        | _ when a <= 30 ->
            let a = a / 7
            if a = 1 then $"%i{a} week"
            else $"%i{a} weken"
        | _ when a < 365 ->
            let a = a / 30
            if a = 1 then $"%i{a} maand"
            else $"%i{a} maanden"
        | _ ->
            let a = a / 365
            if a = 1 then $"%A{a} jaar"
            else $"%A{a} jaar"


    let printDaysToWeeks d =
        let d = d |> BigRational.ToInt32
        (d / 7) |> sprintf "%i weken"


    let toString (pat : Patient) =

        let gender = pat.Gender |> Gender.toString

        let age =
            match pat.Age.Minimum, pat.Age.Maximum with
            | Some min, Some max ->
                let min = min |> printAge
                let max = max |> printAge
                $"leeftijd %s{min} tot %s{max}"
            | Some min, None ->
                let min = min |> printAge
                $"leeftijd vanaf %s{min}"
            | None, Some max ->
                let max = max |> printAge
                $"leeftijd tot %s{max}"
            | _ -> ""

        let neonate =
            match pat.GestAge.Minimum, pat.GestAge.Maximum, pat.PMAge.Minimum, pat.PMAge.Maximum with
            | Some min, Some max, _, _ ->
                let min = min |> printDaysToWeeks
                let max = max |> printDaysToWeeks
                $"neonaten zwangerschapsduur %s{min} tot %s{max}"
            | Some min, None, _, _ ->
                let min = min |> printDaysToWeeks
                $"neonaten zwangerschapsduur vanaf %s{min}"
            | None, Some max, _, _ ->
                let max = max |> printDaysToWeeks
                $"neonaten zwangerschapsduur tot %s{max}"
            | _, _, Some min, Some max ->
                let min = min |> printDaysToWeeks
                let max = max |> printDaysToWeeks
                $"neonaten postconceptie leeftijd %s{min} tot %s{max}"
            | _, _, Some min, None ->
                let min = min |> printDaysToWeeks
                $"neonaten postconceptie leeftijd vanaf %s{min}"
            | _, _, None, Some max ->
                let max = max |> printDaysToWeeks
                $"neonaten postconceptie leeftijd tot %s{max}"
            | _ -> ""

        let weight =
            let toStr (v : BigRational) =
                let v = v / 1000N
                if v.Denominator = 1I then v |> BigRational.ToInt32 |> sprintf "%i"
                else
                    v
                    |> BigRational.ToDouble
                    |> sprintf "%A"

            match pat.Weight.Minimum, pat.Weight.Maximum with
            | Some min, Some max -> $"gewicht %s{min |> toStr} tot %s{max |> toStr} kg"
            | Some min, None     -> $"gewicht vanaf %s{min |> toStr} kg"
            | None,     Some max -> $"gewicht tot %s{max |> toStr} kg"
            | None,     None     -> ""

        [
            gender
            neonate
            age
            weight
        ]
        |> List.filter (String.isNullOrWhiteSpace >> not)
        |> String.concat ", "



module Product =

    open Informedica.Utils.Lib.BCL


    let toBrs s =
        s
        |> String.splitAt ';'
        |> Array.choose Double.tryParse
        |> Array.choose BigRational.fromFloat


    let toBrOpt brs = brs |> Array.tryHead


    let tupleBrOpt brs1 brs2 =
        brs1 |> Array.tryHead,
        brs2 |> Array.tryHead


    let products () =
        Web.getDataFromSheet "Products2"
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
                    GPK =  get "GPK"
                    ATC = get "ATC"
                    MainGroup = get "MainGroup"
                    SubGroup = get "SubGroup"
                    Generic = get "Generic"
                    TallMan = get "TallMan"
                    Synonyms = get "Synonyms" |> String.split "||" |> List.toArray
                    Product = get "Product"
                    Label = get "Label"
                    Shape = get "Shape"
                    ShapeQuantity = get "ShapeQuantity" |> toBrOpt
                    ShapeUnit = get "ShapeUnit"
                    Substance = get "Substance"
                    SubstanceQuantity = get "SubstanceQuantity" |> toBrOpt
                    SubstanceUnit = get "SubstanceUnit"
                    MultipleQuantity = get "MultipleQuantity" |> toBrOpt
                    MultipleUnit = get "MultipleUnit"
                    Divisible = get "Divisible" |> toBrOpt
                |}
            )
            |> Array.groupBy (fun r ->
                {
                    GPK =  r.GPK
                    ATC = r.ATC
                    MainGroup = r.MainGroup
                    SubGroup = r.SubGroup
                    Generic = r.Generic
                    TallMan = r.TallMan
                    Synonyms = r.Synonyms
                    Product = r.Product
                    Label = r.Label
                    Shape = r.Shape
                    ShapeQuantity = r.ShapeQuantity
                    ShapeUnit = r.ShapeUnit
                    Divisible = r.Divisible
                    Substances = [||]
                }
            )
            |> Array.map (fun (prod, rs) ->
                { prod with
                    Substances =
                        rs
                        |> Array.map (fun r ->
                            {
                                Name = r.Substance
                                Quantity = r.SubstanceQuantity
                                Unit = r.SubstanceUnit
                                MultipleQuantity = r.MultipleQuantity
                                MultipleUnit = r.MultipleUnit
                            }
                        )
                }
            )

    let generics (products : Product array) =
        products
        |> Array.map (fun p ->
            p.Generic
        )
        |> Array.distinct


    let synonyms (products : Product array) =
        products
        |> Array.collect (fun p ->
            p.Synonyms
        )
        |> Array.append (generics products)
        |> Array.distinct


    let shapes  (products : Product array) =
        products
        |> Array.map (fun p -> p.Shape)
        |> Array.distinct



module DoseRule =

    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL


    let toBrs s =
        s
        |> String.splitAt ';'
        |> Array.choose Double.tryParse
        |> Array.choose BigRational.fromFloat


    let toBrOpt brs = brs |> Array.tryHead


    let tupleBrOpt brs1 brs2 =
        brs1 |> Array.tryHead,
        brs2 |> Array.tryHead


    let printFreqs (r : DoseRule) =
            let frs =
                r.Frequencies
                |> Array.map BigRational.ToInt32
                |> Array.map string
                |> String.concat ", "
            $"{frs} x / {r.TimeUnit}"


    let printMinMaxDose u (minMax : MinMax) =
        let s = minMax |> MinMax.toString
        if s |> String.isNullOrWhiteSpace then ""
        else
            $"{s} {u}"
            |> String.replace "<" "max"


    let printNormDose u br =
        match br with
        | None    -> ""
        | Some br -> $"{br |> BigRational.toStringNl} {u}"


    let printDose (dr : DoseRule) =
        dr.DoseLimits
        |> Array.map (fun dl ->
            let doseQtyAdjUnit = $"{dr.DoseUnit}/{dr.AdjustUnit}"
            let doseTotAdjUnit = $"{doseQtyAdjUnit}/{dr.TimeUnit}"
            let doseTotUnit = $"{dr.DoseUnit}/{dr.TimeUnit}"
            let freqs = $"{dr |> printFreqs}"
            [
                $"- {dl.Substance}"

                $"{dl.NormDoseTotalAdjust |> printNormDose doseTotAdjUnit} " +
                $"{dl.DoseTotalAdjust |> printMinMaxDose doseTotAdjUnit}"

                $"{dl.NormDoseTotal |> printNormDose doseTotUnit} " +
                $"{dl.DoseTotal |> printMinMaxDose doseTotUnit}"

                if freqs |> String.isNullOrWhiteSpace |> not then
                    $"in {freqs}"

                $"{dl.NormDoseQuantityAdjust |> printNormDose doseQtyAdjUnit} " +
                $"{dl.DoseQuantityAdjust |> printMinMaxDose doseQtyAdjUnit}"

                $"{dl.NormDoseQuantity |> printNormDose dr.DoseUnit} " +
                $"{dl.DoseQuantity |> printMinMaxDose dr.DoseUnit}"
            ]
            |> List.map String.trim
            |> List.filter (String.IsNullOrEmpty >> not)
            |> String.concat "\n"
        )

    let doseRules () =
        Web.getDataFromSheet "DoseRules2"
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
                    Generic = get "Generic"
                    Shape = get "Shape"
                    Route = get "Route"
                    Gender = get "Gender" |> Gender.fromString
                    MinAge = get "MinAge" |> toBrOpt
                    MaxAge = get "MaxAge" |> toBrOpt
                    MinWeight = get "MinWeight" |> toBrOpt
                    MaxWeight = get "MaxWeight" |> toBrOpt
                    MinGestAge = get "MinGestAge" |> toBrOpt
                    MaxGestAge = get "MaxGestAge" |> toBrOpt
                    MinPMAge = get "MinPMAge" |> toBrOpt
                    MaxPMAge = get "MaxPMAge" |> toBrOpt
                    OrderType = get "OrderType" |> OrderType.fromString
                    Frequencies = get "Frequencies" |> toBrs
                    Rates = get "Rates" |> toBrs
                    DoseUnit = get "DoseUnit"
                    AdjustUnit = get "AdjustUnit"
                    TimeUnit = get "TimeUnit"
                    RateUnit = get "RateUnit"
                    MinTime = get "MinTime" |> toBrOpt
                    MaxTime = get "MaxTime" |> toBrOpt
                    NormDose = get "NormDose" |> toBrOpt
                    MinDose = get "MinDose" |> toBrOpt
                    MaxDose = get "MaxDose" |> toBrOpt
                    MaxPerDose = get "MaxPerDose" |> toBrOpt
                    AbsMaxDose = get "AbsMaxDose" |> toBrOpt
                    Substance = get "Substance"
                    NormDoseQty = get "NormDoseQty" |> toBrOpt
                    MinDoseQty = get "MinDoseQty" |> toBrOpt
                    MaxDoseQty = get "MaxDoseQty" |> toBrOpt
                    NormDoseQtyAdj = get "NormDoseQtyAdj" |> toBrOpt
                    MinDoseQtyAdj = get "MinDoseQtyAdj" |> toBrOpt
                    MaxDoseQtyAdj = get "MaxDoseQtyAdj" |> toBrOpt
                    NormDoseTot = get "NormDoseTot" |> toBrOpt
                    MinDoseTot = get "MinDoseTot" |> toBrOpt
                    MaxDoseTot = get "MaxDoseTot" |> toBrOpt
                    NormDoseTotAdj = get "NormDoseTotAdj" |> toBrOpt
                    MinDoseTotAdj = get "MinDoseTotAdj" |> toBrOpt
                    MaxDoseTotAdj = get "MaxDoseTotAdj" |> toBrOpt
                    NormDoseRate = get "NormDoseRate" |> toBrOpt
                    MinDoseRate = get "MinDoseRate" |> toBrOpt
                    MaxDoseRate = get "MaxDoseRate" |> toBrOpt
                    NormDoseRateAdj = get "NormDoseRateAdj" |> toBrOpt
                    MinDoseRateAdj = get "MinDoseRateAdj" |> toBrOpt
                    MaxDoseRateAdj = get "MaxDoseRateAdj" |> toBrOpt
                |}
            )
            |> Array.groupBy (fun r ->
                {
                    Indication = r.Indication
                    Generic = r.Generic
                    Shape = r.Shape
                    Route = r.Route
                    Patient =
                        {
                            Gender = r.Gender
                            Age = (r.MinAge, r.MaxAge) |> MinMax.fromTuple
                            Weight = (r.MinWeight, r.MaxWeight) |> MinMax.fromTuple
                            GestAge = (r.MinGestAge, r.MaxGestAge) |> MinMax.fromTuple
                            PMAge = (r.MinPMAge, r.MaxPMAge) |> MinMax.fromTuple
                        }
                    OrderType = r.OrderType
                    Frequencies = r.Frequencies
                    Rates = r.Rates
                    DoseUnit = r.DoseUnit
                    AdjustUnit = r.AdjustUnit
                    TimeUnit = r.TimeUnit
                    RateUnit = r.RateUnit
                    Time = (r.MinTime, r.MaxTime) |> MinMax.fromTuple
                    DoseLimits = [||]
                }
            )
            |> Array.map (fun (dr, rs) ->
                { dr with
                    DoseLimits =
                        rs
                        |> Array.map (fun r ->
                            {
                                Substance = r.Substance
                                NormDoseQuantity = r.NormDoseQty
                                DoseQuantity = (r.MinDoseQty, r.MaxDoseQty) |> MinMax.fromTuple
                                NormDoseQuantityAdjust = r.NormDoseQtyAdj
                                DoseQuantityAdjust = (r.MinDoseQtyAdj, r.MaxDoseQtyAdj) |> MinMax.fromTuple
                                NormDoseTotal = r.NormDoseTot
                                DoseTotal = (r.MinDoseTot, r.MaxDoseTot) |> MinMax.fromTuple
                                NormDoseTotalAdjust = r.NormDoseTotAdj
                                DoseTotalAdjust = (r.MinDoseTotAdj, r.MaxDoseTotAdj) |> MinMax.fromTuple
                                NormDoseRate = r.NormDoseRate
                                DoseRate = (r.MinDoseRate, r.MaxDoseRate) |> MinMax.fromTuple
                                NormDoseRateAdjust = r.NormDoseRateAdj
                                DoseRateAdjust = (r.MinDoseRateAdj, r.MaxDoseRateAdj) |> MinMax.fromTuple
                            }
                        )
                }
            )


    let indications (doseRules : DoseRule array) =
        doseRules
        |> Array.map (fun r -> r.Indication)
        |> Array.distinct


    let routes (doseRules : DoseRule array) =
        doseRules
        |> Array.map (fun r -> r.Route)
        |> Array.distinct


    let patients (doseRules : DoseRule array) =
        doseRules
        |> Array.map (fun r -> r.Patient)
        |> Array.sortBy Patient.sortBy
        |> Array.map Patient.toString
        |> Array.distinct


    let frequencies (doseRules : DoseRule array) =
        doseRules
        |> Array.map printFreqs
        |> Array.distinct


open Informedica.Utils.Lib.BCL
open DoseRule

let drs = doseRules ()


drs
|> Array.collect printDose
|> Array.iter (printfn "%s")