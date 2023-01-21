namespace Informedica.ZForm.Lib


/// Utility methods to extend the
/// `Informedica.GenUnits.Lib.ValueUnit` library
module ValueUnit =

    open System
    open Informedica.GenCore.Lib.Types.ZForm
    open MathNet.Numerics

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL
    open Informedica.GenUnits.Lib

    open ValueUnit

    let unitFromString = Units.fromString

    let unitToString = Units.toString Units.Localization.English Units.Short


    let readableStringToWeightUnit s =
        $"%s{s}[Weight]"
        |> Units.fromString

    let readableStringToBSAUnit s =
        $"%s{s}[BSA]"
        |> Units.fromString

    let readableStringToTimeUnit s =
        $"%s{s}[Time]"
        |> Units.fromString

    /// Map a unit `u` to a `ValueUnit.Unit`
    /// using a mapping `m`.
    let unitFromMappedString m u =
            u
            |> Mapping.mapUnit m Mapping.ValueUnitMap
            |> Units.fromString

    /// Create a unit from a GStand unit
    let unitFromGStandString = unitFromMappedString Mapping.GStandMap

    /// Create a unit from an App unit
    let unitFromAppString = unitFromMappedString Mapping.AppMap

    /// Create a value unit using a specific mapping `m`
    /// with value `v` and unit `u`.
    let createValueUnit m (d : decimal) u =
        let v = d |> float
        match u |> unitFromMappedString m with
        | None -> None
        | Some u ->
            match v |> BigRational.fromFloat with
            | None -> None
            | Some v  -> create u v |> Some

    /// Create a `ValueUnit` using a float value
    /// and a string unit using the GStand mapping
    let valueUnitFromGStandUnitString = createValueUnit Mapping.GStandMap


    /// Create a `ValueUnit` using a float value
    /// and a string unit using the App mapping
    let valueUnitFromAppUnitString = createValueUnit Mapping.AppMap

    /// Create a `ValueUnit` using a float value
    /// `v` and a `Unit` `u`.
    let fromDecimal (v: decimal) u =
        v
        |> BigRational.fromDecimal
        |> create u


    /// Turn a `ValueUnit` to a float, string tuple.
    /// Where the unit string representation is a
    /// GSTand string.
    let valueUnitToGStandUnitString vu =
        let v, u = get vu

        v |> BigRational.toDecimal,
        u
        |> Units.toString Units.Localization.English Units.Short
        |> Mapping.mapUnit Mapping.ValueUnitMap Mapping.GStandMap

    /// Turn a `ValueUnit` to a float, string tuple.
    /// Where the unit string representation is an
    /// App string.
    let valueUnitToAppUnitString vu =
        let v, u = get vu

        v |> BigRational.toDecimal,
        u
        |> Units.toString Units.Localization.English Units.Short
        |> Mapping.mapUnit Mapping.ValueUnitMap Mapping.AppMap

    let timeInMinute = (fun n -> fromDecimal n Units.Time.minute)


    let timeInHour =  (fun n -> fromDecimal n Units.Time.hour)


    let timeInDay =  (fun n -> fromDecimal n Units.Time.day)


    let timeInWeek =  (fun n -> fromDecimal n Units.Time.week)


    let ageInWk =  (fun n -> fromDecimal n Units.Time.week)


    let ageInMo =  (fun n -> fromDecimal n Units.Time.month)


    let ageInYr =  (fun n -> fromDecimal n Units.Time.year)


    let weightInKg =  (fun n -> fromDecimal n Units.Weight.kiloGram)


    let bsaInM2 =  (fun n -> fromDecimal n Units.BSA.M2)


    /// Create a frequency unit
    /// per `n` days
    let freqUnitPerNday n =
        1N
        |> Units.Count.nTimes
        |> per (Units.Time.nDay n)


    /// Create a frequency unit
    /// per `n` hours
    let freqUnitPerNHour n =
        1N
        |> Units.Count.nTimes
        |> per (Units.Time.nHour n)

    /// Freq unit per 1 hour.
    let freqPerOneHour = freqUnitPerNHour 1N

    /// Create an optional `ValueUnit` using
    /// an optional gestational age `gest` in
    /// weeks and days.
    let gestAgeInDaysAndWeeks gest =
        gest
        |> Option.bind (fun (w, d) ->
            let vu1 = fromDecimal w Units.Time.week
            let vu2 = fromDecimal d Units.Time.day
            vu1 + vu2 |> Some
        )

    /// Turn a frequency `ValueUnit` `freq`
    /// to a valueunit string representation.
    let freqToValueUnitString freq =
        freq |> toStringDutchLong


    /// Check whether a unit `u`
    /// is a time unit.
    let isTimeUnit u =
        (u |> Group.unitToGroup) = Group.TimeGroup


    /// Helper functions to quicly create
    /// combined units
    module Units =

        let perOneHour = freqPerOneHour

        let perFourHour = freqUnitPerNHour 4N

        let perOneDay = freqUnitPerNday 1N

        let mgKgDay = Units.Mass.milliGram   |> per Units.Weight.kiloGram |> per Units.Time.day

        let mgKgHour = Units.Mass.milliGram  |> per Units.Weight.kiloGram |> per Units.Time.hour

        let mgKg4Hour = Units.Mass.milliGram |> per Units.Weight.kiloGram |> per (Units.Time.nHour 4N)

        let mcgKgHour = Units.Mass.microGram |> per Units.Weight.kiloGram |> per Units.Time.hour

        let mcgKgMin = Units.Mass.microGram  |> per Units.Weight.kiloGram |> per Units.Time.minute

        let mcgKgDay = Units.Mass.microGram |> per Units.Weight.kiloGram |> per Units.Time.day

        let mcg = Units.Mass.microGram

        let min = Units.Time.minute

        let hour = Units.Time.hour



    module ValueUnitTests =


        let tests () =

            let (|>!) x f =
                printfn "%A" x
                f x

            createValueUnit Mapping.GStandMap Decimal.Ten "milligram"
            |> printfn "Create value unit 10 milligram using GStand mapping: %A"

            Mapping.allGStandUnits ()
            |> Array.iter (fun s ->
                printfn $"Mapping %s{s}: %A{s |> unitFromGStandString}"
                match s |> unitFromGStandString with
                | Some u ->
                    u
                    |> ValueUnit.Units.toString Units.Localization.English Units.Short
                    |> printfn "ValueUnit unit string: %s"
                | None -> ()
                printfn $"ValueUnit: %A{valueUnitFromGStandUnitString 1.5m s}"
                match (valueUnitFromGStandUnitString 1.5m s) |> (Option.bind (valueUnitToGStandUnitString >> Some)) with
                | Some (_, u) ->
                    if u = "" then printfn $"Cannot parse: %s{s}"
                | None -> ()
            )

            Mapping.allAppUnits ()
            |> Array.iter (fun s ->
                printfn $"Mapping %s{s}: %A{s |> unitFromAppString}"
                match s |> unitFromAppString with
                | Some u ->
                    u
                    |> ValueUnit.Units.toString Units.Localization.English Units.Short
                    |> printfn "ValueUnit unit string: %s"
                | None -> ()
                let vu = valueUnitFromAppUnitString 1.5m s
                match vu with
                | Some vu ->
                    printfn $"ValueUnit: %A{vu}"
                    vu
                    |> Dto.toDtoDutchLong
                    |> (fun dto -> dto |> Dto.toString |> printfn "dto: %s"; dto)
                    |> Dto.fromDto
                    |>! ignore

                | None -> ()
                match vu |> (Option.bind (valueUnitToAppUnitString >> Some)) with
                | Some (_, u) ->
                    if u = "" then printfn $"Cannot parse: %s{s}"
                | None -> ()
            )

