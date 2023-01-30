namespace Informedica.GenCore.Tests



module Tests =

    open System

    open Expecto
    open Expecto.Flip

    open MathNet.Numerics

    open Informedica.GenUnits.Lib
    open Informedica.GenCore.Lib
    open Informedica.GenCore.Lib.Ranges
    open Informedica.GenCore.Lib.Patients

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL



    module CalculationTests =

        let bsaCalcList =
            [
                Calculations.BSA.duBois, "DuBois", 1.6949m<m2>
                Calculations.BSA.fujimoto, "Fuijimoto", 1.6476m<m2>
                Calculations.BSA.gehanAndGeorge, "Gehan and George", 1.6916m<m2>
                Calculations.BSA.haycock, "Haycock", 1.6804m<m2>
                Calculations.BSA.mosteller, "Mosteller", 1.6833m<m2>
            ]


        let tests = testList "Calculations" [
            testList "BSA" [
                for f, s, e in bsaCalcList do
                    test $"{s}" {
                        Calculations.BSA.calcBSA f (Some 5) 60m<kg> 170m<cm>
                        |> Expect.equal $"should be {e}" e
                    }
            ]

            testList "age" [
                test "born Dec 7, 2022 and current Jan 25, 2023" {
                    let dtNow = DateTime(2023, 1, 25)
                    let dtBd = DateTime(2022, 12, 7)
                    Calculations.Age.adjustedAge 0<day> 36<week> dtBd dtNow
                    |> Expect.equal "should be 21 days" 21<day>
                }
            ]

        ]



    module MinIncrMaxTests =

        module Calculator = MinIncrMax.Calculator

        let maxMultipleOf incr min =
            let b, br = min
            if b then br |> BigRational.maxInclMultipleOf incr
            else
                br |> BigRational.maxExclMultipleOf incr


        let minMultipleOf incr min =
            let b, br = min
            if b then br |> BigRational.minInclMultipleOf incr
            else
                br |> BigRational.minExclMultipleOf incr


        let minGTmax (maxIncl, max) (minIncl, min) =
            if minIncl && maxIncl then min > max
            else
                min >= max


        let calcIncrement brs =
                brs
                |> Set.filter ((<) 0N)
                |> Set.removeBigRationalMultiples
                |> fun brs1 ->
                    if brs1 |> Set.isEmpty then
                        $"No valid increments {brs}"
                        |> Errors.NoValidLimitIncr
                        |> Error
                    else
                        brs1
                        |> Ok

        let validate =
            Calculator.validate
                calcIncrement
                minMultipleOf
                maxMultipleOf
                minGTmax



        let tests = testList "MinIncrMax.validate" [
            fun min incr max ->
                let min =
                    min
                    |> Option.map (fun (minIncl, min) -> minIncl, min |> BigRational.fromInt)
                let max =
                    max
                    |> Option.map (fun (maxIncl, max) -> maxIncl, max |> BigRational.fromInt)
                let incr = incr |> Set.map BigRational.fromInt |> Some

                try
                    validate min incr max
                    |> function
                    | Error _ -> ()
                    | Ok (min, incr, max) ->
                        let toBrStr = BigRational.toFloat >> Double.toStringNumberNLWithoutTrailingZerosFixPrecision 3
                        Calculator.toString toBrStr min incr max
                        |> ignore //printfn "Pass: %s"
                    true
                with
                | _ -> false
            |> Generators.testProp "with simple integers never throws an exception"


            fun min incr max ->
                let min =
                    min
                    |> Option.map (fun (minIncl, min) -> minIncl, min |> BigRational.fromInt)
                let max =
                    max
                    |> Option.map (fun (maxIncl, max) -> maxIncl, max |> BigRational.fromInt)
                let incr = incr |> Set.map BigRational.fromInt |> Some

                try
                    validate min incr max
                    |> function
                    | Error _ -> ()
                    | Ok (min, incr, max) ->
                        let toBrStr = BigRational.toFloat >> Double.toStringNumberNLWithoutTrailingZerosFixPrecision 3
                        Calculator.toStringNL toBrStr min incr max
                        |> ignore // printfn "Pass: %s"
                    true
                with
                | _ -> false
            |> Generators.testProp "with simple integers dutch version never throws an exception"


            fun min incr max ->
                try
                    Calculator.validate min incr max
                    |> ignore
                    true
                with
                | _ -> false
            |> Generators.testProp "never throws an exception"


            fun min incr max ->
                validate min incr max
                |> function
                | Ok (Some min, _, Some max) ->
                    let min = min |> snd
                    let max = max |> snd
                    min <= max
                | _ -> true
            |> Generators.testProp "min always is equal or less than max"
        ]



    module MinMaxTests =

        module MinMax = MinIncrMax.Optics

        let mmToStr = MinIncrMax.toString "van (incl) " "van (incl) " "tot (incl) " "tot (excl) "


        let createValueUnit (d : decimal) u =
            let v = d |> float
            match u |> ValueUnit.Units.fromString with
            | None -> None
            | Some u ->
                match v |> BigRational.fromFloat with
                | None -> None
                | Some v  -> ValueUnit.createSingle u v |> Some


        let mg10, mg20 =
            createValueUnit 10.m "mg[Mass]" |> Option.get ,
            createValueUnit 20.m "mg[Mass]" |> Option.get

        let mgIncl10, mgIncl20 =
            mg10 |> Limit.inclusive,
            mg20 |> Limit.inclusive

        let mgExcl10, mgExcl20 =
            mg10 |> Limit.exclusive,
            mg20 |> Limit.exclusive

        let mg30, mg40 =
            createValueUnit 30.m "mg[Mass]" |> Option.get ,
            createValueUnit 40.m "mg[Mass]" |> Option.get

        let mgIncl30, mgIncl40 =
            mg30 |> Limit.inclusive,
            mg40 |> Limit.inclusive


        let toString () =
            MinIncrMax.empty
            |> MinMax.setMin (createValueUnit 1.m "mg[Mass]"  |> Option.get |> Inclusive)
            |> MinMax.setMax (createValueUnit 10.m "mg[Mass]" |> Option.get |> Inclusive)
            |> mmToStr


        let fromDecimal (v: decimal) u =
            v
            |> BigRational.fromDecimal
            |> ValueUnit.createSingle u


        let ageInMo =  (fun n -> fromDecimal n ValueUnit.Units.Time.month)


        let ageInYr =  (fun n -> fromDecimal n ValueUnit.Units.Time.year)


        let ageInclOneMo, ageExclOneYr =
            1m |> ageInMo |> Inclusive,
            1m |> ageInYr |> Exclusive


        let ageRange =
            MinIncrMax.empty
            |> MinMax.setMin ageInclOneMo
            |> MinMax.setMax ageExclOneYr


        let valueComp =

            [
                 mgIncl10, mgIncl10, Limit.eq, true
                 mgIncl10, mgIncl20, Limit.eq, false
                 mgIncl10, mgIncl20, Limit.st true false, true
                 mgIncl10, mgIncl10, Limit.st true false, false
                 mgIncl10, mgIncl20, Limit.ste true false, true
                 mgIncl10, mgIncl10, Limit.ste true false, true
                 mgIncl10, mgIncl20, Limit.gt true false, false
                 mgIncl10, mgIncl10, Limit.gt true false, false
                 mgIncl10, mgIncl20, Limit.gte true false, false
                 mgIncl10, mgIncl10, Limit.gte true false, true

                 mgExcl10, mgExcl10, Limit.eq, false
                 mgExcl10, mgExcl20, Limit.st true false, true
                 mgExcl10, mgExcl10, Limit.st true false, false
                 mgExcl10, mgExcl20, Limit.ste true false, true
                 mgExcl10, mgExcl10, Limit.ste true false, false //Min Excl 10 mg <= Max Excl 10 mg
                 mgExcl10, mgExcl20, Limit.gt true false, false
                 mgExcl10, mgExcl10, Limit.gt true false, true //Min Excl 10 mg > Max Excl 10 mg
                 mgExcl10, mgExcl20, Limit.gte true false, false
                 mgExcl10, mgExcl10, Limit.gte true false, true

                 mgIncl10, mgExcl10, Limit.eq, false
                 mgIncl10, mgExcl10, Limit.gt true false, true //Min Incl 10 mg > Max Excl 10 mg
                 mgIncl10, mgExcl10, Limit.st true false, false //Min Incl 10 mg < Max Excl 10 mg

                 ageInclOneMo, ageExclOneYr, Limit.eq, false
                 ageInclOneMo, ageExclOneYr, Limit.st true false, true
                 ageInclOneMo, ageExclOneYr, Limit.ste true false, true
                 ageInclOneMo, ageExclOneYr, Limit.gt true false, false
                 ageInclOneMo, ageExclOneYr, Limit.gte true false, false

            ]


        // ToDo handle None cases correctly?
        let testFold () =
            let mms =
                [
                    MinIncrMax.empty
                    MinIncrMax.empty |> MinMax.setMin mgIncl10
                    MinIncrMax.empty |> MinMax.setMin mgIncl20
                    MinIncrMax.empty |> MinMax.setMax mgIncl30
                    MinIncrMax.empty |> MinMax.setMax mgIncl40
                    MinIncrMax.empty |> MinMax.setMin mgIncl10 |> MinMax.setMax mgIncl30
                    MinIncrMax.empty |> MinMax.setMin mgIncl20 |> MinMax.setMax mgIncl30
                    MinIncrMax.empty |> MinMax.setMin mgIncl30 |> MinMax.setMax mgIncl30
                    MinIncrMax.empty |> MinMax.setMin mgIncl40 |> MinMax.setMax mgIncl40
                ]

            mms
            |> MinIncrMax.foldMaximize
            |> mmToStr,
            mms
            |> MinIncrMax.foldMinimize
            |> mmToStr


        let inRange =
            let mm1 = MinIncrMax.empty
            let mm2 =
                MinIncrMax.empty
                |> MinMax.setMin mgIncl10
            let mm3 =
                MinIncrMax.empty
                |> MinMax.setMax mgIncl40
            let mm4 =
                MinIncrMax.empty
                |> MinMax.setMin mgIncl20
                |> MinMax.setMax mgIncl30

            [
                (mg10, mm1, true)
                (mg20, mm1,true)
                (mg30, mm1, true)
                (mg40, mm1, true)
                (mg10, mm2, true)
                (mg20, mm2, true)
                (mg30, mm2, true)
                (mg40, mm2, true)
                (mg10, mm3, true)
                (mg20, mm3, true)
                (mg30, mm3, true)
                (mg40, mm3, true)
                (mg10, mm4, false)
                (mg20, mm4, true)
                (mg30, mm4, true)
                (mg40, mm4, false)
            ]



        let tests = testList "MinMax" [
            test "minGTmax" {
                mgIncl20 |> Limit.minGTmax mgIncl10
                |> Expect.isTrue $"{mgIncl20} > {mgIncl10}"
            }

            test "toString" {
                toString()
                |> Expect.equal "should equal" "van (incl) 1 mg - tot (incl) 10 mg"
            }


            testList "Validate" [
                test "cannot set have limits with different unit groups" {
                    { ageRange with
                        Max = Some mgIncl10
                    }
                    |> MinIncrMax.validate
                    |> function
                        | Ok mm -> false |> Expect.isTrue $"{mm |> mmToStr} is not valid!"
                        | Error msg ->
                            true
                            |> Expect.isTrue $"{msg}"
                }
            ]

            testList "valueComparison" [

                for v1, v2, cp, exp in valueComp do
                    test $"comparing {v1 |> Limit.toString true} {cp |> Limit.cmpToStr} {v2 |> Limit.toString false}" {
                        v1 |> cp <| v2
                        |> Expect.equal $"should be {exp}" exp
                    }
            ]

            test "minimize, maximize" {
                testFold ()
                |> Expect.equal "should equal" ("van (incl) 10 mg - tot (incl) 40 mg", "van (incl) 30 mg - tot (incl) 30 mg")
            }

            testList "in range" [
                for v, mm, b in inRange do
                    test $"%s{v |> ValueUnit.toStringPrec 0} in range: %s{mm |> mmToStr} = %A{MinIncrMax.inRange v mm}" {
                        MinIncrMax.inRange v mm
                        |> Expect.equal $"should be {b}" b
                    }
            ]
        ]


        module DtoTests =

            module Dto = MinIncrMax.Dto

            let dto () =
                Dto.dto ()



            let tests  =
                testList "Dto" [
                    test "MinIncrMax from Dto is the same as empty" {
                        dto ()
                        |> Dto.fromDto
                        |> function
                            | Some mm ->
                                mm
                                |> Expect.equal "should be an empty mm" MinIncrMax.empty
                            | None -> false |> Expect.isTrue "could not create an empty dto"
                    }

                    test "MinIncrMax dto setting min and max" {
                        // Add min and max to dto and there and back again
                        let dto = dto ()
                        dto.Min.Value <- [|1m|]
                        dto.Min.Unit <- "mg"
                        dto.Min.Group <- "mass"
                        dto.HasMin <- true
                        dto.MinIncl <- false
                        dto.Max.Value <- [|2m|]
                        dto.Max.Unit <- "g"
                        dto.Max.Group <- "mass"
                        dto.HasMax <- true
                        dto
                        |> Dto.fromDto
                        |> function
                            | Some _ -> true |> Expect.isTrue "can create dto with min and max"
                            | None -> false |> Expect.isTrue "cannot set min and max"
                    }

                    test "MinIncrMax that is not valid will not return from dto" {
                        let dto = dto ()

                        dto.Min.Value <- [|1m|]
                        dto.Min.Unit <- "g"
                        dto.Min.Group <- "mass"
                        dto.HasMin <- true
                        dto.MinIncl <- false
                        dto.Max.Value <- [|1m|]
                        dto.Max.Unit <- "mg"
                        dto.Max.Group <- "mass"
                        dto.HasMax <- true
                        dto
                        |> Dto.fromDto
                        |> Option.bind (Dto.toDto >> Some)
                        |> function
                            | Some _ -> false |> Expect.isTrue "can create dto with min and max"
                            | None -> true |> Expect.isTrue "cannot set min > than max"
                    }
                ]



    module PatientTests =


        module AgeTests =



            let tests =
                testList "AgeTests" [
                    fun x ->
                        let dto = AgeValue.Dto.dto ()
                        dto.Years <- Some x

                        dto
                        |> AgeValue.Dto.fromDto
                        |> function
                            | Ok a ->
                                a.Years.Value <= AgeValue.Validation.maxYear &&
                                a.Years.Value >= 0<year>
                            | Error _ -> true
                    |> Generators.testProp "years should never be > 120 years"

                    fun x ->
                        let dto = AgeValue.Dto.dto ()
                        dto.Months <- Some x

                        dto
                        |> AgeValue.Dto.fromDto
                        |> function
                            | Ok a ->
                                a.Months.Value <= 12<month> &&
                                a.Months.Value >= 0<month>
                            | Error _ -> true
                    |> Generators.testProp "months should never be > 12"

                    fun x ->
                        let dto = AgeValue.Dto.dto ()
                        dto.Weeks <- Some x

                        dto
                        |> AgeValue.Dto.fromDto
                        |> function
                            | Ok a ->
                                a.Weeks.Value <= 4<week> &&
                                a.Weeks.Value >= 0<week>
                            | Error _ -> true
                    |> Generators.testProp "weeks should never be > 4"

                    fun x ->
                        let dto = AgeValue.Dto.dto ()
                        dto.Days <- Some x

                        dto
                        |> AgeValue.Dto.fromDto
                        |> function
                            | Ok a ->
                                a.Days.Value <= 7<day> &&
                                a.Days.Value >= 0<day>
                            | Error _ -> true
                    |> Generators.testProp "days should never be > 7"


                    fun y m w d ->
                        let dto = AgeValue.Dto.dto ()
                        dto.Days <- Some d
                        dto.Weeks <- Some w
                        dto.Months <- Some m
                        dto.Years <- Some y

                        dto
                        |> AgeValue.Dto.fromDto
                        |> function
                            | Ok a ->
                                let y, m, w, d = a |> AgeValue.get
                                Calculations.Age.yearsMonthsWeeksToDaysOpt y m w d
                                |> fun n -> n <= (Constants.daysInYear * 120 |> Conversions.dayFromInt)
                            | Error _ -> true
                    |> Generators.testProp "should never be > 120 years"
                ]

        module YearMonthDayTests =
            open FsCheck
            open Microsoft.VisualBasic.CompilerServices

            let intGenerator s n =
                Gen.sample s n Arb.generate<int>


            let tests = testList "YearMonthDay" [
                test "should always return a valid date" {
                    let ys = intGenerator 2200 100
                    let ms = intGenerator 20 100
                    let ds = intGenerator 40 100

                    try
                        [
                            for y in ys do
                                for m in ms do
                                    for d in ds do
                                        let dto = BirthDate.Dto.dto ()
                                        dto.Year <- y
                                        dto.Month <- Some m
                                        dto.Day <- Some d

                                        dto
                        ]
                        |> List.forall (fun dto ->
                            match dto |> BirthDate.Dto.fromDto with
                            | Ok ymd ->
                                try
                                    let y = ymd.Year |> int
                                    let m = ymd.Month |> Option.defaultValue 1<month> |> int
                                    let d = ymd.Day |> Option.defaultValue 1<day> |> int
                                    DateTime(y, m, d) |> ignore
                                    true
                                with
                                | _ ->
                                    printfn $"cannot create datetime with {dto.Year}-{dto.Month}-{dto.Day}"
                                    false
                            | Error _ -> true

                        )
                    with
                    | _ -> true
                    |> Expect.isTrue "should never fail"
                }

                fun (dto : BirthDate.Dto.Dto) ->
                    let y =
                        intGenerator 2200 1
                        |> List.filter (fun x -> x >= 1900)
                    dto.Year <- y |> List.tryHead |> Option.defaultValue 2000

                    dto
                    |> BirthDate.Dto.fromDto
                    |> function
                        | Ok ymd1 ->
                            let s = $"{dto.Year}-{dto.Month |> Option.defaultValue 1}-{dto.Day |> Option.defaultValue 1}"

                            ymd1
                            |> BirthDate.Dto.toDto
                            |> BirthDate.Dto.fromDto
                            |>function
                                | Ok ymd2 -> ymd2 = ymd1
                                | Error _ -> false
                        | Error _ ->
                            true
                |> testProperty "there and back again"
            ]


    [<Tests>]
    let tests =

        [
            CalculationTests.tests
            MinIncrMaxTests.tests
            MinMaxTests.tests
            MinMaxTests.DtoTests.tests
            PatientTests.AgeTests.tests
            PatientTests.YearMonthDayTests.tests
        ]
        //|> List.skip 5
        //|> List.take 1
        |> testList "GenCore"
