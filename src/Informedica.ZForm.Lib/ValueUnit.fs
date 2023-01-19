namespace Informedica.ZForm.Lib


/// Utility methods to extend the
/// `Informedica.GenUnits.Lib.ValueUnit` library
module ValueUnit =

    open System
    open MathNet.Numerics

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL
    open Informedica.GenUnits.Lib

    module Units= ValueUnit.Units



    /// Get the value of the value unit `vu`, i.e.
    /// the number without the unit.
    let getValue vu = let v, _ = vu |> ValueUnit.get in v

    let getUnit vu = let _, u = vu |> ValueUnit.get in u

    let eqsGroup vu1 vu2 =
        let u1 = vu1 |> getUnit
        let u2 = vu2 |> getUnit
        u1 |> ValueUnit.Group.eqsGroup u2

    let unitFromString = Units.fromString
    let unitToString = Units.toString Units.Localization.English Units.Short

    /// Get the user readable string version
    /// of a unit, i.e. without unit group between
    /// brackets
    let unitToReadableString u =
        u
        |> Units.toString Units.Dutch Units.Short
        |> String.removeBrackets

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
            | Some v  ->
                ValueUnit.create u v |> Some

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
        |> float
        |> BigRational.fromFloat
        |> Option.bind (fun br ->
            ValueUnit.create u br
            |> Some
        )

    /// Turn a `ValueUnit` to a float, string tuple.
    /// Where the unit string representation is a
    /// GSTand string.
    let valueUnitToGStandUnitString vu =
        let v, u = ValueUnit.get vu

        v |> BigRational.toFloat,
        u
        |> ValueUnit.Units.toString Units.Localization.English Units.Short
        |> Mapping.mapUnit Mapping.ValueUnitMap Mapping.GStandMap

    /// Turn a `ValueUnit` to a float, string tuple.
    /// Where the unit string representation is an
    /// App string.
    let valueUnitToAppUnitString vu =
        let v, u = ValueUnit.get vu

        v |> BigRational.toFloat,
        u
        |> ValueUnit.Units.toString Units.Localization.English Units.Short
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


    open ValueUnit


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
            fromDecimal w Units.Time.week
            |> Option.bind (fun vu1 ->
                fromDecimal d Units.Time.day
                |> Option.bind (fun vu2 -> vu1 + vu2 |> Some)
            )
        )

    /// Turn a frequency `ValueUnit` `freq`
    /// to a valueunit string representation.
    let freqToValueUnitString freq =
        freq |> toStringDutchLong

    /// Turn a `ValueUnit` `vu` into
    /// a string using precision `prec`.
    let toStringPrec prec vu =
        let v, u = vu |> ValueUnit.get

        let vs =
            v
            |> BigRational.toFloat
            |> Double.fixPrecision prec
            |> string

        let us =
            u
            |> unitToReadableString

        vs + " " + us

    /// Check whether a unit `u`
    /// is a time unit.
    let isTimeUnit u =
        (u |> ValueUnit.Group.unitToGroup) = ValueUnit.Group.TimeGroup


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


    module Dto =

        [<Literal>]
        let english = "english"

        [<Literal>]
        let dutch = "dutch"

        type Dto () =
            member val Value = Decimal.Zero with get, set
            member val Unit = "" with get, set
            member val Group = "" with get, set
            member val Short = true with get, set
            member val Language = "" with get, set

        let dto () = Dto ()

        let toString (dto : Dto) =
            sprintf "%A %s" dto.Value dto.Unit

        let toDto short lang vu =
            let isLang s l =
                l
                |> String.trim
                |> String.toLower
                |> (fun l -> s |> String.startsWith l)
            let l =
                match lang with
                | _ when lang |> isLang english ->
                    ValueUnit.Units.English |> Some
                | _ when lang |> isLang dutch ->
                    ValueUnit.Units.Dutch |> Some
                | _ -> None

            match l with
            | None -> None
            | Some l ->
                let s =
                    if short then ValueUnit.Units.Short
                    else ValueUnit.Units.Long

                let v, u = vu |> get
                let v = v |> BigRational.toDecimal
                let g =
                    u
                    |> ValueUnit.Group.unitToGroup
                    |> ValueUnit.Group.toString
                let u =
                    u
                    |> ValueUnit.Units.toString l s
                    |> String.removeBrackets

                let dto = dto ()
                dto.Value <- v
                dto.Unit <- u
                dto.Group <- g
                dto.Language <- lang
                dto.Short <- short

                dto |> Some

        let toDtoDutchShort vu  =  vu |>toDto true dutch    |> Option.get
        let toDtoDutcLong vu    =  vu |>toDto false dutch   |> Option.get
        let toDtoEnglisShort vu =  vu |>toDto true english  |> Option.get
        let toDtoEnglisLong vu  =  vu |>toDto false english |> Option.get

        let fromDto (dto: Dto) =
            let v = dto.Value |> BigRational.fromDecimal
            $"%s{dto.Unit}[%s{dto.Group}]"
            |> ValueUnit.Units.fromString
            |> function
            | Some u ->
                v
                |> ValueUnit.create u
                |> Some
            | _ -> None

    module ValueUnitTests =

        let tests () =

            let (|>!) x f =
                printfn "%A" x
                f x

            createValueUnit (Mapping.GStandMap) Decimal.Ten "milligram"
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
                    |> Dto.toDtoDutcLong
                    |> (fun dto -> dto |> Dto.toString |> printfn "dto: %s"; dto)
                    |> Dto.fromDto
                    |>! ignore

                | None -> ()
                match vu |> (Option.bind (valueUnitToAppUnitString >> Some)) with
                | Some (_, u) ->
                    if u = "" then printfn $"Cannot parse: %s{s}"
                | None -> ()
            )

