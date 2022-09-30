
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


    type DoseType =
        | Start
        | Once
        | PRN
        | Maintenance
        | Continuous
        | StepDown of int
        | StepUp of int
        | AnyDoseType


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
            Diagnosis : string
            Gender : Gender
            Age : MinMax
            Weight : MinMax
            BSA : MinMax
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
            DoseType : DoseType
            Frequencies : BigRational array
            DoseUnit : string
            AdjustUnit : string
            FreqUnit : string
            RateUnit : string
            Time : MinMax
            TimeUnit : string
            Interval : MinMax
            IntervalUnit : string
            Duration : MinMax
            DurationUnit : string
            DoseLimits : DoseLimit array
            Products : Product array
        }



module DoseType =

    open Informedica.Utils.Lib.BCL

    let fromString s =
        let s = s |> String.toLower |> String.trim

        match s with
        | "start" -> Start
        | "eenmalig" -> Once
        | "prn" -> PRN
        | "onderhoud" -> Maintenance
        | "continu" -> Continuous
        | _ when s |> String.startsWith "afbouw" ->
            match s |> String.split(" ") with
            | [_;i] ->
                match i |> Int32.tryParse with
                | Some i -> StepDown i
                | None ->
                    printfn $"couldn't match {s}"
                    AnyDoseType
            | _ ->
                printfn $"couldn't match {s}"
                AnyDoseType
        | _ when s |> String.startsWith "opbouw" ->
            match s |> String.split(" ") with
            | [_;i] ->
                match i |> Int32.tryParse with
                | Some i -> StepUp i
                | None ->
                    printfn $"couldn't match {s}"
                    AnyDoseType
            | _ ->
                printfn $"couldn't match {s}"
                AnyDoseType
        | _ ->
            printfn $"couldn't match {s}"
            AnyDoseType


    let toString = function
        | Start -> "start"
        | Once -> "eenmalig"
        | PRN -> "prn"
        | Maintenance -> "onderhoud"
        | Continuous -> "continu"
        | StepDown i -> $"afbouw {i}"
        | StepUp i -> $"opbouw {i}"
        | AnyDoseType -> ""



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
        (pat.GestAge.Minimum |> toInt) +
        (pat.PMAge.Minimum |> toInt) +
        (pat.Weight.Minimum |> Option.map (fun w -> w / 1000N) |> toInt)


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
            let s =
                if pat.GestAge.Maximum.IsSome && pat.GestAge.Maximum.Value <= 259N then "prematuren"
                else "neonaten"

            match pat.GestAge.Minimum, pat.GestAge.Maximum, pat.PMAge.Minimum, pat.PMAge.Maximum with
            | Some min, Some max, _, _ ->
                let min = min |> printDaysToWeeks
                let max = max |> printDaysToWeeks
                $"{s} zwangerschapsduur %s{min} tot %s{max}"
            | Some min, None, _, _ ->
                let min = min |> printDaysToWeeks
                $"{s} zwangerschapsduur vanaf %s{min}"
            | None, Some max, _, _ ->
                let max = max |> printDaysToWeeks
                $"{s} zwangerschapsduur tot %s{max}"
            | _, _, Some min, Some max ->
                let min = min |> printDaysToWeeks
                let max = max |> printDaysToWeeks
                $"prematuren postconceptie leeftijd %s{min} tot %s{max}"
            | _, _, Some min, None ->
                let min = min |> printDaysToWeeks
                $"prematuren postconceptie leeftijd vanaf %s{min}"
            | _, _, None, Some max ->
                let max = max |> printDaysToWeeks
                $"prematuren postconceptie leeftijd tot %s{max}"
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


    let filter generic shape (prods : Product array) =
        let eqs s1 s2 =
            let s1 = s1 |> String.trim |> String.toLower
            let s2 = s2 |> String.trim |> String.toLower
            s1 = s2
        prods
        |> Array.filter (fun p -> p.Generic |> eqs generic && p.Shape |> eqs shape)


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
            if frs |> String.isNullOrWhiteSpace then ""
            else
                if r.FreqUnit |> String.isNullOrWhiteSpace then $"{frs} x"
                else
                    $"{frs} x / {r.FreqUnit}"


    let printInterval (dr: DoseRule) =
        if dr.IntervalUnit |> String.isNullOrWhiteSpace then ""
        else
            let s = dr.Interval |> MinMax.toString
            if s |> String.isNullOrWhiteSpace then ""
            else
                let s =
                    s
                    |> String.replace "≥" "min. interval "
                    |> String.replace "<" "max. interval"
                    |> fun s ->
                        if s |> String.contains "-" then $"elke {s}"
                        else s
                $"{s} {dr.IntervalUnit}"



    let printTime (dr: DoseRule) =
        if dr.TimeUnit |> String.isNullOrWhiteSpace then ""
        else
            let s = dr.Time |> MinMax.toString
            if s |> String.isNullOrWhiteSpace then ""
            else
                $"{s} {dr.TimeUnit}"
                |> String.replace "<" "max"


    let printDuration (dr: DoseRule) =
        if dr.DurationUnit |> String.isNullOrWhiteSpace then ""
        else
            let s = dr.Duration |> MinMax.toString
            if s |> String.isNullOrWhiteSpace then ""
            else
                $"{s} {dr.DurationUnit}"
                |> String.replace "<" "max"


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


    let printDose wrap (dr : DoseRule) =
        dr.DoseLimits
        |> Array.map (fun dl ->
            let doseQtyAdjUnit = $"{dr.DoseUnit}/{dr.AdjustUnit}/keer"
            let doseTotAdjUnit = $"{dr.DoseUnit}/{dr.AdjustUnit}/{dr.FreqUnit}"
            let doseTotUnit = $"{dr.DoseUnit}/{dr.FreqUnit}"
            let doseQtyUnit = $"{dr.DoseUnit}/keer"
            let doseRateUnit = $"{dr.DoseUnit}/{dr.RateUnit}"
            let doseRateAdjUnit = $"{dr.DoseUnit}/{dr.AdjustUnit}/{dr.RateUnit}"
            [
                $"{dl.NormDoseRate |> printNormDose doseRateUnit} " +
                $"{dl.DoseRate |> printMinMaxDose doseRateUnit}"

                $"{dl.NormDoseRateAdjust |> printNormDose doseRateAdjUnit} " +
                $"{dl.DoseRateAdjust |> printMinMaxDose doseRateAdjUnit}"

                $"{dl.NormDoseTotalAdjust |> printNormDose doseTotAdjUnit} " +
                $"{dl.DoseTotalAdjust |> printMinMaxDose doseTotAdjUnit}"

                $"{dl.NormDoseTotal |> printNormDose doseTotUnit} " +
                $"{dl.DoseTotal |> printMinMaxDose doseTotUnit}"

                $"{dl.NormDoseQuantityAdjust |> printNormDose doseQtyAdjUnit} " +
                $"{dl.DoseQuantityAdjust |> printMinMaxDose doseQtyAdjUnit}"

                $"{dl.NormDoseQuantity |> printNormDose doseQtyUnit} " +
                $"{dl.DoseQuantity |> printMinMaxDose doseQtyUnit}"
            ]
            |> List.map String.trim
            |> List.filter (String.IsNullOrEmpty >> not)
            |> String.concat " "
            |> fun s -> $"{dl.Substance} {wrap}{s}{wrap}"
        )


    let filter indication generic shape route (patient : Patient option) (dsrs : DoseRule array) =
        dsrs
        |> Array.filter(fun dr ->
            match indication with
            | None -> true
            | Some i -> dr.Indication = i
        )
        |> Array.filter(fun dr ->
            match generic with
            | None -> true
            | Some g -> dr.Generic = g
        )
        |> Array.filter(fun dr ->
            match shape with
            | None -> true
            | Some s -> dr.Shape = s
        )
        |> Array.filter(fun dr ->
            match route with
            | None -> true
            | Some r -> dr.Route = r
        )


    let generics (dsrs : DoseRule array) =
        dsrs
        |> Array.map (fun dr -> dr.Generic)
        |> Array.distinct


    let doseRules () =
        let prods = Product.products ()

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
                    Diagn = get "Diagn"
                    Gender = get "Gender" |> Gender.fromString
                    MinAge = get "MinAge" |> toBrOpt
                    MaxAge = get "MaxAge" |> toBrOpt
                    MinWeight = get "MinWeight" |> toBrOpt
                    MaxWeight = get "MaxWeight" |> toBrOpt
                    MinBSA = get "MinBSA" |> toBrOpt
                    MaxBSA = get "MaxBSA" |> toBrOpt
                    MinGestAge = get "MinGestAge" |> toBrOpt
                    MaxGestAge = get "MaxGestAge" |> toBrOpt
                    MinPMAge = get "MinPMAge" |> toBrOpt
                    MaxPMAge = get "MaxPMAge" |> toBrOpt
                    DoseType = get "DoseType" |> DoseType.fromString
                    Frequencies = get "Freqs" |> toBrs
                    DoseUnit = get "DoseUnit"
                    AdjustUnit = get "AdjustUnit"
                    FreqUnit = get "FreqUnit"
                    RateUnit = get "RateUnit"
                    MinTime = get "MinTime" |> toBrOpt
                    MaxTime = get "MaxTime" |> toBrOpt
                    TimeUnit = get "TimeUnit"
                    MinInterval = get "MinInt" |> toBrOpt
                    MaxInterval = get "MaxInt" |> toBrOpt
                    IntervalUnit = get "IntUnit"
                    MinDur = get "MinDur" |> toBrOpt
                    MaxDur = get "MaxDur" |> toBrOpt
                    DurUnit = get "DurUnit"
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
                            Diagnosis = r.Diagn
                            Gender = r.Gender
                            Age = (r.MinAge, r.MaxAge) |> MinMax.fromTuple
                            Weight = (r.MinWeight, r.MaxWeight) |> MinMax.fromTuple
                            BSA = (r.MinBSA, r.MaxBSA) |> MinMax.fromTuple
                            GestAge = (r.MinGestAge, r.MaxGestAge) |> MinMax.fromTuple
                            PMAge = (r.MinPMAge, r.MaxPMAge) |> MinMax.fromTuple
                        }
                    DoseType = r.DoseType
                    Frequencies = r.Frequencies
                    DoseUnit = r.DoseUnit
                    AdjustUnit = r.AdjustUnit
                    FreqUnit = r.FreqUnit
                    RateUnit = r.RateUnit
                    Time = (r.MinTime, r.MaxTime) |> MinMax.fromTuple
                    TimeUnit = r.TimeUnit
                    Interval = (r.MinInterval, r.MaxInterval) |> MinMax.fromTuple
                    IntervalUnit = r.IntervalUnit
                    Duration = (r.MinDur, r.MaxDur) |> MinMax.fromTuple
                    DurationUnit = r.DurUnit
                    DoseLimits = [||]
                    Products = prods |> Product.filter r.Generic r.Shape
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


    /// See for use of anonymous record in
    /// fold: https://github.com/dotnet/fsharp/issues/6699
    let toMarkdown (ds : DoseRule array) =
        let generic_md generic = $"""
# {generic}
---
"""

        let route_md route products = $"""
### Route: {route}
#### Producten
{products}
"""

        let product_md product =  $"""
* {product}
"""

        let indication_md indication = $"""

## Indicatie: {indication}
---
"""

        let doseCapt_md = """
#### Doseringen
"""

        let dose_md dt dose freqs intv time dur =
            let dt = dt |> DoseType.toString
            let freqs =
                if freqs |> String.isNullOrWhiteSpace then ""
                else
                    $" in {freqs}"

            let s =
                [
                    if intv |> String.isNullOrWhiteSpace |> not then
                        $" {intv}"
                    if time |> String.isNullOrWhiteSpace |> not then
                        $" inloop tijd {time}"
                    if dur |> String.isNullOrWhiteSpace |> not then
                        $" {dur}"
                ]
                |> String.concat ", "
                |> fun s ->
                    if s |> String.isNullOrWhiteSpace then ""
                    else
                        $" ({s |> String.trim})"

            $"*{dt}*\n{dose}{freqs}{s}"

        let patient_md patient doses = $"""
* Patient: **{patient}**
%s{doses}
"""

        ({| md = ""; doses = [||] |}, ds
        |> Array.groupBy (fun d -> d.Generic))
        ||> Array.fold (fun acc (generic, ds) ->
            {| acc with
                md = generic_md generic
                doses = ds
            |}
            |> fun r ->
                if r.doses = Array.empty then r
                else
                    (r, r.doses |> Array.groupBy (fun d -> d.Indication))
                    ||> Array.fold (fun acc (indication, ds) ->
                        {| acc with
                            md = acc.md + (indication_md indication)
                            doses = ds
                        |}
                        |> fun r ->
                            if r.doses = Array.empty then r
                            else
                                (r, r.doses |> Array.groupBy (fun r -> r.Route))
                                ||> Array.fold (fun acc (route, ds) ->

                                    let prods =
                                        ds
                                        |> Array.collect (fun d -> d.Products)
                                        |> Array.sortBy (fun p -> p.ShapeQuantity)
                                        |> Array.map (fun p -> product_md p.Label)
                                        |> Array.distinct
                                        |> String.concat "\n"
                                    {| acc with
                                        md = acc.md + (route_md route prods)
                                                    + doseCapt_md
                                        doses = ds
                                    |}
                                    |> fun r ->
                                        if r.doses = Array.empty then r
                                        else
                                            (r, r.doses
                                                |> Array.sortBy (fun d -> d.Patient |> Patient.sortBy)
                                                |> Array.groupBy (fun d -> d.Patient |> Patient.toString))
                                            ||> Array.fold (fun acc (pat, ds) ->
                                                let doses =
                                                    ("", ds |> Array.groupBy (fun d -> d.DoseType))
                                                    ||> Array.fold (fun acc (dt, ds) ->
                                                        let dose =
                                                            ds
                                                            |> Array.map (printDose "**")
                                                            |> Array.distinct
                                                            |> function
                                                            | [| d |] -> d |> String.concat "\n"
                                                            | _ -> ""

                                                        let freqs =
                                                            if dose = "" then ""
                                                            else
                                                                ds
                                                                |> Array.map printFreqs
                                                                |> Array.distinct
                                                                |> function
                                                                | [| s |] -> s
                                                                | _ -> ""

                                                        let intv =
                                                            if dose = "" then ""
                                                            else
                                                                ds
                                                                |> Array.map printInterval
                                                                |> Array.distinct
                                                                |> function
                                                                | [| s |] -> s
                                                                | _ -> ""

                                                        let time =
                                                            if dose = "" then ""
                                                            else
                                                                ds
                                                                |> Array.map printTime
                                                                |> Array.distinct
                                                                |> function
                                                                | [| s |] -> s
                                                                | _ -> ""

                                                        let dur =
                                                            if dose = "" then ""
                                                            else
                                                                ds
                                                                |> Array.map printDuration
                                                                |> Array.distinct
                                                                |> function
                                                                | [| s |] -> s
                                                                | _ -> ""

                                                        $"{acc}\n{dose_md dt dose freqs intv time dur}"
                                                    )

                                                {| acc with
                                                    md =
                                                        if doses = "" then acc.md
                                                        else
                                                            acc.md + (patient_md pat doses)
                                                |}
                                            )
                                )
                    )
        )
        |> fun r -> r.md



open DoseRule


open System.IO



File.WriteAllText("formularium.md", "")


doseRules ()
|> fun drs ->
    drs
    |> generics
    |> Array.sort
    |> Array.map(fun g ->
        drs
        |> filter None (Some g) None None None
        |> toMarkdown
    )
    |> fun s -> File.AppendAllLines("formularium.md", s)



(doseRules()[0]).Patient
|> Patient.toString

[| doseRules()[0]|]
|> toMarkdown

doseRules()
|> toMarkdown

doseRules () |> ignore

