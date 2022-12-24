namespace Informedica.GenForm.Lib


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
            fun (p: PatientCategory) -> filter.GestAge |> MinMax.isBetween p.GestAge
            fun (p: PatientCategory) -> filter.PMAge |> MinMax.isBetween p.PMAge
            fun (p: PatientCategory) -> filter.Gender |> Gender.filter p.Gender
            fun (p: PatientCategory) ->
                match p.Location, filter.Location with
                | AnyLocation, _
                | _, AnyLocation -> true
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
            BSA = None
            GestAge = None
            PMAge = None
            Location = AnyLocation
        }


    let toString (pat: Patient) =

        let gender = pat.Gender |> Gender.toString

        let age =
            pat.Age
            |> Option.map PatientCategory.printAge
            |> Option.defaultValue ""


        let neonate =
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

        let weight =
            let toStr (v : BigRational) =
                let v = v / 1000N
                if v.Denominator = 1I then v |> BigRational.ToInt32 |> sprintf "%i"
                else
                    v
                    |> BigRational.ToDouble
                    |> sprintf "%A"

            pat.Weight
            |> Option.map (fun w -> $"gewicht %s{w |> toStr} kg")
            |> Option.defaultValue ""


        [
            pat.Department
            gender
            neonate
            age
            weight
        ]
        |> List.filter String.notEmpty
        |> List.filter (String.isNullOrWhiteSpace >> not)
        |> String.concat ", "

