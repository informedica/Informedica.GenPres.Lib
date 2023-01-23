namespace Informedica.ZForm.Lib


module Patient =

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL
    open Informedica.GenCore.Lib

    open Aether
    open Aether.Operators


    let create ga age wght bsa gend =
        {
            GestAge = ga
            Age = age
            Weight = wght
            BSA = bsa
            Gender = gend
        }


    let empty = create MinMax.empty MinMax.empty MinMax.empty MinMax.empty Undetermined


    module Optics =

        module MinMax = MinMax.Optics


        let setGender = Optic.set Patient.Gender_


        let inclMinGestAge =
            Patient.GestAge_ >-> MinMax.inclMinLens


        let setInclMinGestAge = Optic.set inclMinGestAge


        let exclMinGestAge =
            Patient.GestAge_ >-> MinMax.exclMinLens


        let setExclMinGestAge = Optic.set exclMinGestAge


        let inclMaxGestAge =
            Patient.GestAge_ >-> MinMax.inclMaxLens


        let setInclMaxGestAge = Optic.set inclMaxGestAge


        let exclMaxGestAge =
            Patient.GestAge_ >-> MinMax.exclMaxLens


        let setExclMaxGestAge = Optic.set exclMaxGestAge


        let inclMinAge =
            Patient.Age_ >-> MinMax.inclMinLens


        let setInclMinAge = Optic.set inclMinAge


        let exclMinAge =
            Patient.Age_ >-> MinMax.exclMinLens


        let setExclMinAge = Optic.set exclMinAge


        let inclMaxAge =
            Patient.Age_ >-> MinMax.inclMaxLens


        let setInclMaxAge = Optic.set inclMaxAge


        let exclMaxAge =
            Patient.Age_ >-> MinMax.exclMaxLens


        let setExclMaxAge = Optic.set exclMaxAge


        let inclMinWeight =
            Patient.Weight_ >-> MinMax.inclMinLens


        let setInclMinWeight = Optic.set inclMinWeight


        let exclMinWeight =
            Patient.Weight_ >-> MinMax.exclMinLens


        let setExclMinWeight = Optic.set exclMinWeight


        let inclMaxWeight =
            Patient.Weight_ >-> MinMax.inclMaxLens


        let setInclMaxWeight = Optic.set inclMaxWeight


        let exclMaxWeight =
            Patient.Weight_ >-> MinMax.exclMaxLens


        let setExclMaxWeight = Optic.set exclMaxWeight


        let inclMinBSA =
            Patient.BSA_ >-> MinMax.inclMinLens


        let setInclMinBSA = Optic.set inclMinBSA


        let exclMinBSA =
            Patient.BSA_ >-> MinMax.exclMinLens


        let setExclMinBSA = Optic.set exclMinBSA


        let inclMaxBSA =
            Patient.BSA_ >-> MinMax.inclMaxLens


        let setInclMaxBSA = Optic.set inclMaxBSA


        let exclMaxBSA =
            Patient.BSA_ >-> MinMax.exclMaxLens


        let setExclMaxBSA = Optic.set exclMaxBSA


    let genderToString = function
    | Male -> "man"
    | Female -> "vrouw"
    | Undetermined -> ""

    let stringToGender s =
        match s with
        | _ when s |> String.toLower |> String.trim = "man" -> Male
        | _ when s |> String.toLower |> String.trim = "vrouw" -> Female
        | _  -> Undetermined


    let toString { GestAge = ga; Age = age; Weight = wght; BSA = bsa; Gender = gen } =
        let (>+) sl sr =
            let l, s = sr

            let s = s |> String.trim
            let sl = sl |> String.trim

            if s |> String.isNullOrWhiteSpace then sl
            else sl + (if sl = "" then " " else  ", ") + l + s

        let mmToStr = MinMax.toString "van" "tot"

        ""
        >+ ("Zwangerschapsduur: ", ga |> MinMax.gestAgeToString)
        >+ ("Leeftijd: ", age |> MinMax.ageToString)
        >+ ("Gewicht: ", wght |> mmToStr)
        >+ ("BSA: ", bsa |> mmToStr)
        >+ ("Geslacht: ", gen |> genderToString)
        |> String.removeTrailing ["\n"]


    module Dto =

        type Dto () =
            member val GestAge = MinMax.Dto.dto() with get ,set
            member val Age = MinMax.Dto.dto () with get ,set
            member val Weight = MinMax.Dto.dto () with get ,set
            member val BSA = MinMax.Dto.dto () with get ,set
            member val Gender = "" with get, set


        let dto () = Dto ()

        let toDto { GestAge = gestAge; Age = age; Weight = wght; BSA = bsa; Gender = gnd } =
            let dto = dto ()

            dto.GestAge <- gestAge |> MinMax.Dto.toDto
            dto.Age <- age |> MinMax.Dto.toDto
            dto.Weight <- wght |> MinMax.Dto.toDto
            dto.BSA <- bsa |> MinMax.Dto.toDto
            dto.Gender <- gnd |> genderToString

            dto


        let fromDto (dto : Dto) =
            let gestAge = dto.GestAge |> MinMax.Dto.fromDto
            let age = dto.Age |> MinMax.Dto.fromDto
            let wght = dto.Weight |> MinMax.Dto.fromDto
            let bsa = dto.BSA |> MinMax.Dto.fromDto
            let gnd = dto.Gender |> stringToGender

            match gestAge, age, wght, bsa with
            | Some gestAge, Some age, Some wght, Some bsa ->
                {
                    GestAge = gestAge
                    Age = age
                    Weight = wght
                    BSA = bsa
                    Gender = gnd
                } |> Some

            | _ -> None

    module PatientTests =

        let tests () =

            let (|>!) x f =
                printfn "result: %A" x
                x |> f

            let dto = Dto.dto ()

            dto
            |> Dto.fromDto
            |>! Option.bind (Dto.toDto >> Some)
            |>! ignore

            dto.Age.HasMin <- true
            dto.Age.Min.Value <- [|1m|]
            dto.Age.Min.Unit <- "maand"
            dto.Age.Min.Group <- "Time"
            dto.Age.Min.Language <- "dutch"
            dto.Age.Min.Short <- true
            dto.Age.MinIncl <- true

            dto.Age
            |> MinMax.Dto.fromDto
            |>! ignore

            dto
            |> Dto.fromDto
            |>! ignore

            // group defaults to general when no unit can be found in group
            // ToDo: need to fix this behaviour
            dto.Age.HasMin <- true
            dto.Age.Min.Value <- [|1m|]
            dto.Age.Min.Unit <- "m"
            dto.Age.Min.Group <- "Time"
            dto.Age.Min.Language <- "dutch"
            dto.Age.Min.Short <- true
            dto.Age.MinIncl <- true

            dto.Age
            |> MinMax.Dto.fromDto
            |>! ignore

            // need to check for the correct units
            // ToDo!!
            dto.Age.HasMin <- true
            dto.Age.Min.Value <- [|1m|]
            dto.Age.Min.Unit <- "g"
            dto.Age.Min.Group <- "Mass"
            dto.Age.Min.Language <- "dutch"
            dto.Age.Min.Short <- true
            dto.Age.MinIncl <- true

            dto.Age
            |> MinMax.Dto.fromDto
            |>! ignore
