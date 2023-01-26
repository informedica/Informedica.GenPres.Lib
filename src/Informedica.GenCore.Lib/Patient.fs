namespace Informedica.GenCore.Lib

open System

open MathNet.Numerics
open Informedica.GenUnits.Lib



type VenousAccess =
    | Peripheral
    | Central
    | AnyLocation
    | UnknownLocation


type EnteralAccess =
    | Oral
    | Gastric
    | Duodenal
    | AnyAccess
    | UnknownAccess


type Gender = Male | Female | AnyGender | UnknownGender


type AgeValue =
    {
        Years: int<year> option
        Months: int<month> option
        Weeks: int<week> option
        Days: int<day> option
    }


type YearMonthDay = { Year : int; Month : int option; Day : int option }


type PatientAge =
    | AgeValue of AgeValue
    | BirthDate of YearMonthDay


type WeightValue = | Kilogram of decimal<kg> | Gram of int<gram>


type HeightValue = | Meter of decimal<m> | Centimeter of int<cm>


type Weight =
    {
        Actual : WeightAtDate list
        Calculation : WeightAtDate option
        Birth : WeightAtDate option
        Admission : WeightAtDate list
    }
and WeightAtDate = { Date : DateTime; Weight : WeightValue }


type EstimatedWeight = { Weight: WeightValue; SD : WeightValue }


type Height =
    {
        Actual : HeightAtDate list
        Birth : HeightAtDate option
    }
and HeightAtDate = { Date : DateTime; Height : HeightValue }


type  EstimatedHeight = { Height : HeightValue; SD : HeightValue }


type Patient =
    {
        Department : string option
        Diagnoses : string []
        Gender : Gender
        Age : PatientAge
        Weight : Weight
        Height : Height
        GestationalAge : AgeWeeksDays option
        EnteralAccess : EnteralAccess
        VenousAccess : VenousAccess
    }
and AgeWeeksDays = { Weeks: int<week>; Days : int<week> }



type PatientCategory =
    {
        Department : string option
        Diagnoses : string []
        Gender : Gender
        Age : MinIncrMax
        Weight : MinIncrMax
        BSA : MinIncrMax
        GestAge : MinIncrMax
        PMAge : MinIncrMax
        Location : VenousAccess
    }


module AgeValue =


    let create years months weeks days =
        {
            Years = years
            Months = months
            Weeks = weeks
            Days = days
        }

    let get { Years = y; Months = m; Weeks = w; Days = d} =
        y, m, w, d


    let zero = create None None None None


    module Optics =

        let years =
            (fun (a: AgeValue) -> a.Years),
            (fun y a -> { a with Years = y })

        let months =
            (fun (a: AgeValue) -> a.Months),
            (fun m a -> { a with Months = m })

        let weeks =
            (fun (a: AgeValue) -> a.Weeks),
            (fun w (a: AgeValue) -> { a with Weeks = w })

        let days =
            (fun (a: AgeValue) -> a.Days),
            (fun d (a: AgeValue) -> { a with Days = d })
            
        let intYears =
            (fun y -> y |> Option.map int |> Option.defaultValue 0),
            (fun i -> i |> Conversions.yearFromInt |> Some)

        let intMonths =
            (fun y -> y |> Option.map int |> Option.defaultValue 0),
            (fun i -> i |> Conversions.monthFromInt |> Some)

        let intWeeks =
            (fun y -> y |> Option.map int |> Option.defaultValue 0),
            (fun i -> i |> Conversions.weekFromInt |> Some)

        let intDays =
            (fun d -> d |> Option.map int |> Option.defaultValue 0),
            (fun i -> i |> Conversions.dayFromInt |> Some)



    [<AutoOpen>]
    module SetGet =

        open Aether
        open Aether.Operators


        let getYears = Optic.get Optics.years

        let setYears = Optic.set Optics.years

        let getMonths = Optic.get Optics.months

        let setMonths = Optic.set Optics.months

        let getWeeks = Optic.get Optics.weeks

        let setWeeks = Optic.set Optics.weeks

        let getDays = Optic.get Optics.days

        let setDays = Optic.set Optics.days

        let setIntYears = Optics.years >-> Optics.intYears |> Optic.set 



    let fromBirthDate bd now =
        let y, m, w, d = now |> Calculations.Age.fromBirthData bd
        create (Some y) (Some m) (Some w) (Some d)



    module Validation =

        open Validus

        let [<Literal>] maxYear = 120<year>


        let mapOpt f validator =
            fun s x ->
                validator s x
                |> Result.map (Option.map f)

        let yearValidator =
            let m = int maxYear
            Check.optional (Check.Int.between 0 m)
            |> mapOpt Conversions.yearFromInt


        let monthValidator =
            Check.optional (Check.Int.between 0 12)
            |> mapOpt Conversions.monthFromInt


        let weekValidator =
            Check.optional (Check.Int.between 0 56)
            |> mapOpt Conversions.weekFromInt


        let dayValidator =
            Check.optional (Check.Int.between 0 7)
            |> mapOpt Conversions.dayFromInt



    module Dto =

        open Validus

        type Dto () =
            member val Years : int option = None with get, set
            member val Months : int option = None with get, set
            member val Weeks : int option = None with get, set
            member val Days : int option = None with get, set


        let dto () = Dto ()


        let fromDto (dto : Dto) =
            validate {
                let! y = Validation.yearValidator "Years" dto.Years
                let! m = Validation.monthValidator "Months" dto.Months
                let! w = Validation.weekValidator "Weeks" dto.Weeks
                let! d = Validation.dayValidator "Days" dto.Days
                return create y m w d
            }


        let toDto (a : AgeValue) =
            let dto = dto ()
            dto.Years <- a.Years |> Option.map int
            dto.Months <- a.Months |> Option.map int
            dto.Weeks <- a.Weeks |> Option.map int
            dto.Days <- a.Days |> Option.map int

            dto




(*

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

    let filter pat (filter : Patient) =
        match filter.Gender, pat with
        | AnyGender, _ -> true
        | _ -> filter.Gender = pat



module PatientCategory =


    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL



    let sortBy (pat : PatientCategory) =
        let toInt = function
            | Some x -> x |> BigRational.ToInt32
            | None -> 0

        (pat.Age.Minimum |> toInt |> fun i -> if i > 0 then i + 300 else i) +
        (pat.GestAge.Minimum |> toInt) +
        (pat.PMAge.Minimum |> toInt) +
        (pat.Weight.Minimum |> Option.map (fun w -> w / 1000N) |> toInt)


    let filter (filter : Filter) (pat : PatientCategory) =
        let eqs a b =
            match a, b with
            | None, _
            | _, None -> true
            | Some a, Some b -> a = b

        ([| pat |]
        |> Array.filter (fun p ->
            if filter.Diagnoses |> Array.isEmpty then true
            else
                p.Diagnoses
                |> Array.exists (fun d ->
                    filter.Diagnoses |> Array.exists (String.equalsCapInsens d)
                )
        ),
        [|
            fun (p: PatientCategory) -> filter.Department |> eqs p.Department
            fun (p: PatientCategory) -> filter.Age |> MinMax.isBetween p.Age
            fun (p: PatientCategory) -> filter.Weight |> MinMax.isBetween p.Weight
            fun (p: PatientCategory) -> filter.BSA |> MinMax.isBetween p.BSA
            fun (p: PatientCategory) ->
                // defaults to normal gestation
                filter.GestAge
                |> Option.defaultValue 259N
                |> Some
                |> MinMax.isBetween p.GestAge
            fun (p: PatientCategory) ->
                // defaults to normal postmenstrual age
                filter.PMAge
                |> Option.defaultValue 259N
                |> Some
                |> MinMax.isBetween p.PMAge
            fun (p: PatientCategory) -> filter |> Gender.filter p.Gender
            fun (p: PatientCategory) ->
                match p.Location, filter.Location with
                | AnyAccess, _
                | _, AnyAccess -> true
                | _ -> p.Location = filter.Location
        |])
        ||> Array.fold(fun acc pred ->
            acc
            |> Array.filter pred
        )
        |> fun xs -> xs |> Array.length > 0


    let isAgeWeight a w aMinMax wMinMax =
        a |> MinMax.isBetween aMinMax &&
        w |> MinMax.isBetween wMinMax


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


    let printAgeMinMax (age : MinMax) =
        match age.Minimum, age.Maximum with
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



    let toString (pat : PatientCategory) =

        let gender = pat.Gender |> Gender.toString

        let age = pat.Age |> printAgeMinMax

        let neonate =
            let s =
                if pat.GestAge.Maximum.IsSome && pat.GestAge.Maximum.Value < 259N then "prematuren"
                else "neonaten"

            match pat.GestAge.Minimum, pat.GestAge.Maximum, pat.PMAge.Minimum, pat.PMAge.Maximum with
            | _, _, Some min, Some max ->
                let min = min |> printDaysToWeeks
                let max = max |> printDaysToWeeks
                $"{s} postconceptie leeftijd %s{min} tot %s{max}"
            | _, _, Some min, None ->
                let min = min |> printDaysToWeeks
                $"{s} postconceptie leeftijd vanaf %s{min}"
            | _, _, None, Some max ->
                let max = max |> printDaysToWeeks
                $"{s} postconceptie leeftijd tot %s{max}"

            | Some min, Some max, _, _ ->
                let min = min |> printDaysToWeeks
                let max = max |> printDaysToWeeks
                $"{s} zwangerschapsduur %s{min} tot %s{max}"
            | Some min, None, _, _ ->
                let min = min |> printDaysToWeeks
                if s = "neonaten" then s
                else
                    $"{s} zwangerschapsduur vanaf %s{min}"
            | None, Some max, _, _ ->
                let max = max |> printDaysToWeeks
                $"{s} zwangerschapsduur tot %s{max}"
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
            pat.Department |> Option.defaultValue ""
            gender
            neonate
            age
            weight
        ]
        |> List.filter String.notEmpty
        |> List.filter (String.isNullOrWhiteSpace >> not)
        |> String.concat ", "



module Patient =

    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL


    let patient =
        {
            Department = ""
            Diagnoses = [||]
            Gender = AnyGender
            Age = None
            Weight = None
            Height = None
            GestAge = None
            PMAge = None
            Location = AnyAccess
        }


    let calcBSA (pat: Patient) =
        match pat.Weight, pat.Height with
        | Some w, Some h ->
            let w =(w |> BigRational.toDouble) / 1000.
            let h = h |> BigRational.toDouble

            sqrt (w * h / 3600.)
            |> Double.fixPrecision 2
            |> BigRational.fromFloat
        | _ -> None


    let rec toString (pat: Patient) =
        [
            pat.Department
            pat.Gender |> Gender.toString
            pat.Age
            |> Option.map PatientCategory.printAge
            |> Option.defaultValue ""

            let printDaysToWeeks = PatientCategory.printDaysToWeeks

            let s =
                if pat.GestAge.IsSome && pat.GestAge.Value < 259N then "prematuren"
                else "neonaten"

            match pat.GestAge, pat.PMAge with
            | _, Some a ->
                let a = a |> printDaysToWeeks
                $"{s} postconceptie leeftijd %s{a}"
            | Some a, _ ->
                let a = a |> printDaysToWeeks
                $"{s} zwangerschapsduur %s{a}"
            | _ -> ""

            let toStr (v : BigRational) =
                let v = v / 1000N
                if v.Denominator = 1I then v |> BigRational.ToInt32 |> sprintf "%i"
                else
                    v
                    |> BigRational.toStringNl

            pat.Weight
            |> Option.map (fun w -> $"gewicht %s{w |> toStr} kg")
            |> Option.defaultValue ""

            pat.Height
            |> Option.map (fun h -> $"lengte {h |> BigRational.toStringNl} cm")
            |> Option.defaultValue ""

            pat
            |> calcBSA
            |> Option.map (fun bsa ->
                let bsa =
                    bsa
                    |> BigRational.toDouble
                    |> Double.fixPrecision 2
                $"BSA {bsa} m2"
            )
            |> Option.defaultValue ""
        ]
        |> List.filter String.notEmpty
        |> List.filter (String.isNullOrWhiteSpace >> not)
        |> String.concat ", "

*)