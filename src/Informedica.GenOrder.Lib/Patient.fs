namespace Informedica.GenOrder.Lib


module Patient =


    open Informedica.Utils.Lib.BCL
    open Informedica.GenForm.Lib

    let patient : Patient =
        {
            Department = ""
            Diagnoses = [||]
            Gender = Gender.AnyGender
            Age = None
            Weight = None
            Height = None
            GestAge = None
            PMAge = None
            Location = AnyLocation
        }


    type Patient with

        static member Gender_ =
            (fun (p : Patient) -> p.Gender), (fun g (p : Patient) -> { p with Gender = g})

        static member Age_ =
            (fun (p : Patient) -> p.Age), (fun a (p : Patient) -> { p with Age = a})

        static member Weight_ =
            (fun (p : Patient) -> p.Weight), (fun w (p : Patient) -> { p with Weight = w})

        static member Height_ =
            (fun (p : Patient) -> p.Height), (fun b (p : Patient) -> { p with Height = b})

        static member GestAge_ =
            (fun (p : Patient) -> p.GestAge), (fun a (p : Patient) -> { p with GestAge = a})

        static member PMAge_ =
            (fun (p : Patient) -> p.PMAge), (fun a (p : Patient) -> { p with PMAge = a})

        static member Department_ =
            (fun (p : Patient) -> p.Department), (fun d (p : Patient) -> { p with Department = d})


    [<AutoOpen>]
    module Optics =

        open Aether
        open Aether.Operators


        type Age =
            | Years of int
            | Months of int
            | Weeks of int
            | Days of int


        let ageToDec ags =
            ags
            |> List.fold (fun acc a ->
                match a with
                | Years x -> (x |> decimal) * 365m
                | Months x -> (x |> decimal) * 30m
                | Weeks x -> (x |> decimal) * 7m
                | Days x -> (x |> decimal)
                |> fun x -> acc + x
            ) 0m


        let ageFromDec (d : decimal) =
            let yrs = (d / 365m) |> int
            let mos = ((d - (365 * yrs |> decimal)) / 30m) |> int
            let wks = (d - (365 * yrs |> decimal) - (30 * mos |> decimal)) / 7m |> int
            let dys = (d - (365 * yrs |> decimal) - (30 * mos |> decimal) - (7 * wks |> decimal)) |> int
            [
                if yrs > 0 then yrs |> Years
                if mos > 0 then mos |> Months
                if wks > 0 then wks |> Weeks
                if dys > 0 then dys |> Days
            ]

        let ageAgeList =
            Option.map (BigRational.toDecimal >> ageFromDec)
            >> (Option.defaultValue []),
            (ageToDec >> BigRational.fromDecimal >> Some)


        let age_ = Patient.Age_ >-> ageAgeList


        let gestPMAgeList =
            let ageFromDec d =
                d
                |> ageFromDec
                |> List.filter (fun a ->
                    match a with
                    | Years _ | Months _ -> false
                    | _ -> true
                )
            Option.map (BigRational.toDecimal >> ageFromDec)
            >> (Option.defaultValue []),
            (ageToDec >> BigRational.fromDecimal >> Some)


        let gestAge_ = Patient.GestAge_ >-> gestPMAgeList

        let pmAge_ = Patient.PMAge_ >-> gestPMAgeList


        type Weight = | Kilogram of decimal | Gram of int


        let decWeight =
            let get w =
                if w < 300m then w |> Kilogram
                else
                    w
                    |> int
                    |> Gram
            let set = function
                | Kilogram w -> w * 1000m
                | Gram w -> w |> decimal
            Option.map (BigRational.toDecimal >> get),
            Option.map (set >> BigRational.fromDecimal)


        let weight_ = Patient.Weight_ >-> decWeight


        type Height = | Meter of decimal | Centimeter of int


        let decHeight =
            let get h =
                if h < 10m then h |> Meter
                else
                    h
                    |> int
                    |> Centimeter
            let set = function
                | Meter h -> h * 100m
                | Centimeter h -> h |> decimal
            Option.map (BigRational.toDecimal >> get),
            Option.map (set >> BigRational.fromDecimal)


        let height_ = Patient.Height_ >-> decHeight


        let bigRatDec_ =
            Option.map BigRational.toDecimal,
            Option.map BigRational.fromDecimal


        let ageDec_ = Patient.Age_ >-> bigRatDec_

        let weightDec_ = Patient.Weight_ >-> bigRatDec_

        let heightDec_ = Patient.Height_ >-> bigRatDec_

        let gestAgeDec_ = Patient.GestAge_ >-> bigRatDec_

        let pmAgeDec_ = Patient.PMAge_ >-> bigRatDec_


        let getGender = Optic.get Patient.Gender_


        let setGender = Optic.set Patient.Gender_


        let getAge = Optic.get age_


        let setAge = Optic.set age_


        let getAgeDec = Optic.get ageDec_


        let setAgeDec = Optic.set ageDec_


        let getWeight = Optic.get weight_


        let setWeight = Optic.set weight_


        let getWeightDec = Optic.get weightDec_


        let setWeightDec = Optic.set weightDec_


        let getHeight = Optic.get height_


        let setHeight = Optic.set height_


        let getHeightDec = Optic.get heightDec_


        let setHeightDec = Optic.set heightDec_


        let getGestAge = Optic.get gestAge_


        let setGestAge = Optic.set gestAge_


        let getGestAgeDec = Optic.get gestAgeDec_


        let setGestAgeDec = Optic.set gestAgeDec_


        let getPMAge = Optic.get pmAge_


        let setPMAge = Optic.set pmAge_


        let getPMAgeDec = Optic.get pmAgeDec_


        let setPMAgeDec = Optic.set pmAgeDec_


        let getDepartment = Optic.get Patient.Department_


        let setDepartment = Optic.set Patient.Department_

