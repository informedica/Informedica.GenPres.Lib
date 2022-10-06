namespace Informedica.GenForm.Lib



module DoseType =

    open Informedica.Utils.Lib.BCL


    let sortBy = function
        | Start -> 0
        | Once -> 1
        | PRN -> 2
        | Maintenance -> 3
        | Continuous -> 4
        | StepUp n -> 50 + n
        | StepDown n -> 100 + n
        | AnyDoseType -> 200
        | Contraindicated -> -1


    let fromString s =
        let s = s |> String.toLower |> String.trim

        match s with
        | "start" -> Start
        | "eenmalig" -> Once
        | "prn" -> PRN
        | "onderhoud" -> Maintenance
        | "continu" -> Continuous
        | "contra" -> Contraindicated

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
        | Contraindicated -> "contra"
        | AnyDoseType -> ""



module DoseRule =

    open System
    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL


    let toBrs = BigRational.toBrs


    let toBrOpt = BigRational.toBrOpt


    let tupleBrOpt = BigRational.tupleBrOpt


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


    let allFilter =
        {
            Indication = None
            Generic = None
            Shape = None
            Route = None
            Department = None
            Diagnosis = None
            Gender = AnyGender
            Age = None
            Weight = None
            BSA = None
            GestAge = None
            PMAge = None
            DoseType = AnyDoseType
        }


    let filter (filter : Filter) (doseRules : DoseRule array) =
        let eqs a b =
            a
            |> Option.map (fun x -> x = b)
            |> Option.defaultValue true

        [|
            fun (dr : DoseRule) -> dr.Indication |> eqs filter.Indication
            fun (dr : DoseRule) -> dr.Generic |> eqs filter.Generic
            fun (dr : DoseRule) -> dr.Shape |> eqs filter.Shape
            fun (dr : DoseRule) -> dr.Route |> eqs filter.Route
            fun (dr : DoseRule) -> dr.Department |> eqs filter.Department
            fun (dr : DoseRule) -> dr.Patient |> Patient.filter filter
            fun (dr : DoseRule) -> match filter.DoseType with | AnyDoseType -> true | _ -> filter.DoseType = dr.DoseType
        |]
        |> Array.fold (fun (acc : DoseRule[]) pred ->
            acc |> Array.filter pred
        ) doseRules


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
                    Department = get "Dep"
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
                    Department = r.Department
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


    let get getter (doseRules : DoseRule[]) =
        doseRules
        |> Array.map getter
        |> Array.distinct
        |> Array.sort


    let indications = get (fun dr -> dr.Indication)


    let generics = get (fun dr -> dr.Generic)


    let shapes = get (fun dr -> dr.Shape)


    let routes = get (fun dr -> dr.Route)


    let departments = get (fun dr -> dr.Department)


    let diagnoses = get (fun dr -> dr.Patient.Diagnosis)


    let genders = get (fun dr -> dr.Patient.Gender |> Gender.toString)


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
    let toMarkdown (rules : DoseRule array) =
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

            $"* *{dt}*: {dose}{freqs}{s}"

        let patient_md patient diagn =
            $"""

##### Patient: **{patient}**

%s{diagn}
"""

        let printDoses (rules : DoseRule array) =
            ("", rules |> Array.groupBy (fun d -> d.DoseType))
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

                if dt = Contraindicated then $"{acc}\n*gecontra-indiceerd*"
                else
                    $"{acc}\n{dose_md dt dose freqs intv time dur}"
            )

        ({| md = ""; rules = [||] |},
         rules
         |> Array.groupBy (fun d -> d.Generic)
        )
        ||> Array.fold (fun acc (generic, rs) ->
            {| acc with
                md = generic_md generic
                rules = rs
            |}
            |> fun r ->
                if r.rules = Array.empty then r
                else
                    (r, r.rules |> Array.groupBy (fun d -> d.Indication))
                    ||> Array.fold (fun acc (indication, rs) ->
                        {| acc with
                            md = acc.md + (indication_md indication)
                            rules = rs
                        |}
                        |> fun r ->
                            if r.rules = Array.empty then r
                            else
                                (r, r.rules |> Array.groupBy (fun r -> r.Route))
                                ||> Array.fold (fun acc (route, rs) ->

                                    let prods =
                                        rs
                                        |> Array.collect (fun d -> d.Products)
                                        |> Array.sortBy (fun p ->
                                            p.Substances
                                            |> Array.sumBy (fun s -> s.Quantity |> Option.defaultValue 0N)
                                        )
                                        |> Array.map (fun p -> product_md p.Label)
                                        |> Array.distinct
                                        |> String.concat "\n"
                                    {| acc with
                                        md = acc.md + (route_md route prods)
                                                    + doseCapt_md
                                        rules = rs
                                    |}
                                    |> fun r ->
                                        if r.rules = Array.empty then r
                                        else
                                            (r, r.rules
                                                |> Array.sortBy (fun d -> d.Patient |> Patient.sortBy)
                                                |> Array.groupBy (fun d -> d.Patient))
                                            ||> Array.fold (fun acc (pat, rs) ->
                                                let doses =
                                                    rs
                                                    |> Array.sortBy (fun r -> r.DoseType |> DoseType.sortBy)
                                                    |> printDoses
                                                let diagn =
                                                    if pat.Diagnosis |> String.isNullOrWhiteSpace then ""
                                                    else
                                                        $"* Diagnose: **{pat.Diagnosis}**"
                                                let pat = pat |> Patient.toString

                                                {| acc with
                                                    rules = rs
                                                    md = acc.md + (patient_md pat diagn) + $"\n{doses}"
                                                |}
                                            )
                                )
                    )
        )
        |> fun r -> r.md


    let printGenerics (doseRules : DoseRule[]) =
        doseRules
        |> generics
        |> Array.sort
        |> Array.map(fun g ->
            doseRules
            |> filter
                   { allFilter with
                        Generic = Some g
                    }
            |> toMarkdown
        )
