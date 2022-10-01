namespace Informedica.GenForm.Lib




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

    let filter g1 g2 =
        match g1, g2 with
        | AnyGender, _
        | _, AnyGender -> true
        | _ -> g1 = g2


module MinMax =


    let fromTuple (min, max) =
        {
            Minimum = min
            Maximum = max
        }


    let isBetween (minMax : MinMax) = function
        | None -> true
        | Some v ->
            match minMax.Minimum, minMax.Maximum with
            | None, None -> true
            | Some min, None -> v >= min
            | None, Some max -> v < max
            | Some min, Some max -> v >= min && v < max


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


    let filter (filter : Filter) (pat : Patient) =
        ([| pat |]
        |> Array.filter (fun p ->
            match filter.Diagnosis with
            | None -> true
            | Some d -> p.Diagnosis = d
        ),
        [|
            fun (p: Patient) -> filter.Age |> MinMax.isBetween p.Age
            fun (p: Patient) -> filter.Weight |> MinMax.isBetween p.Weight
            fun (p: Patient) -> filter.BSA |> MinMax.isBetween p.BSA
            fun (p: Patient) -> filter.GestAge |> MinMax.isBetween p.GestAge
            fun (p: Patient) -> filter.PMAge |> MinMax.isBetween p.PMAge
            fun (p: Patient) -> filter.Gender |> Gender.filter p.Gender
        |])
        ||> Array.fold(fun acc pred ->
            acc
            |> Array.filter pred
        )
        |> fun xs -> xs |> Array.length > 0


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

    open System
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



        (*
        doseRules
        |> Array.filter(fun dr ->
            match filter.Indication with
            | None -> true
            | Some i -> dr.Indication = i
        )
        |> Array.filter(fun dr ->
            match filter.Generic with
            | None -> true
            | Some g -> dr.Generic = g
        )
        |> Array.filter(fun dr ->
            match filter.Shape with
            | None -> true
            | Some s -> dr.Shape = s
        )
        |> Array.filter(fun dr ->
            match filter.Route with
            | None -> true
            | Some r -> dr.Route = r
        )
        |> Array.filter(fun dr ->
            match filter.Department with
            | None -> true
            | Some d -> dr.Department = d
        )
        *)


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

            $"*{dt}*\n{dose}{freqs}{s}"

        let patient_md patient diagn =
            $"""
* Patient: **{patient}**
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

                $"{acc}\n{dose_md dt dose freqs intv time dur}"
            )

        ({| md = ""; rules = [||] |}, rules
        |> Array.groupBy (fun d -> d.Generic))
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
                                                |> Array.groupBy (fun d -> d.Patient |> Patient.toString, d.Patient.Diagnosis))
                                            ||> Array.fold (fun acc ((pat, diagn), rs) ->
                                                let doses = rs |> printDoses
                                                let diagn =
                                                    if diagn |> String.isNullOrWhiteSpace then ""
                                                    else
                                                        $"* Diagnose: **{diagn}**"

                                                {| acc with
                                                    rules = rs
                                                    md = acc.md + (patient_md pat diagn) + $"\n> {doses}"
                                                |}
                                            )
                                )
                    )
        )
        |> fun r -> r.md


