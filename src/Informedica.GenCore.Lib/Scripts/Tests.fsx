
#load "../../../scripts/Expecto.fsx"
#load "load.fsx"
#load "../MinIncrMax.fs"



module Tests =

    open Expecto
    open Expecto.Flip

    open MathNet.Numerics

    open Informedica.GenUnits.Lib
    open Informedica.GenCore.Lib

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL


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
                        |> MinIncrMax.Errors.NoValidIncrement
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


        [<Tests>]
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
                        |> printfn "Pass: %s"
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
                        |> printfn "Pass: %s"
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

        let mmToStr = MinIncrMax.toString "van" "tot"


        let createValueUnit (d : decimal) u =
            let v = d |> float
            match u |> ValueUnit.Units.fromString with
            | None -> None
            | Some u ->
                match v |> BigRational.fromFloat with
                | None -> None
                | Some v  -> ValueUnit.createSingle u v |> Some


        let v1, v2 =
            createValueUnit 10.m "mg[Mass]" |> Option.get ,
            createValueUnit 20.m "mg[Mass]" |> Option.get

        let incl1, incl2 =
            v1 |> MinIncrMax.inclusive,
            v2 |> MinIncrMax.inclusive

        let v3, v4 =
            createValueUnit 30.m "mg[Mass]" |> Option.get ,
            createValueUnit 40.m "mg[Mass]" |> Option.get

        let incl3, incl4 =
            v3 |> MinIncrMax.inclusive,
            v4 |> MinIncrMax.inclusive


        let toString () =
            MinIncrMax.empty
            |> MinMax.setMin (createValueUnit 1.m "mg[Mass]"  |> Option.get |> MinIncrMax.Inclusive)
            |> MinMax.setMax (createValueUnit 10.m "mg[Mass]" |> Option.get |> MinIncrMax.Inclusive)
            |> mmToStr


        let fromDecimal (v: decimal) u =
            v
            |> BigRational.fromDecimal
            |> ValueUnit.createSingle u


        let ageInMo =  (fun n -> fromDecimal n ValueUnit.Units.Time.month)


        let ageInYr =  (fun n -> fromDecimal n ValueUnit.Units.Time.year)


        let a1, a2 =
            0.1m |> ageInMo |> MinIncrMax.Inclusive,
            0.1m |> ageInYr |> MinIncrMax.Exclusive


        let ageToString () =
            MinIncrMax.empty
            |> MinMax.setMin a1
            |> MinMax.setMax a2
            |> MinIncrMax.ageToString


        let valueComp () =
            [
                 $"%A{incl1} < %A{incl2} = %A{MinIncrMax.valueST incl1 incl2}"
                 $"%A{incl1} < %A{incl1} = %A{MinIncrMax.valueST incl1 incl1}"
                 $"%A{incl1} <= %A{incl2} = %A{MinIncrMax.valueSTE incl1 incl2}"
                 $"%A{incl1} <= %A{incl1} = %A{MinIncrMax.valueSTE incl1 incl1}"
                 $"%A{incl1} > %A{incl2} = %A{MinIncrMax.valueGT incl1 incl2}"
                 $"%A{incl1} > %A{incl1} = %A{MinIncrMax.valueGT incl1 incl1}"
                 $"%A{incl1} >= %A{incl2} = %A{MinIncrMax.valueGTE incl1 incl2}"
                 $"%A{incl1} >= %A{incl1} = %A{MinIncrMax.valueGTE incl1 incl1}"
            ]



        // ToDo handle None cases correctly?
        let testFold () =
            let mms =
                [
                    MinIncrMax.empty
                    MinIncrMax.empty |> MinMax.setMin incl1
                    MinIncrMax.empty |> MinMax.setMin incl2
                    MinIncrMax.empty |> MinMax.setMax incl3
                    MinIncrMax.empty |> MinMax.setMax incl4
                    MinIncrMax.empty |> MinMax.setMin incl1 |> MinMax.setMax incl3
                    MinIncrMax.empty |> MinMax.setMin incl2 |> MinMax.setMax incl3
                    MinIncrMax.empty |> MinMax.setMin incl3 |> MinMax.setMax incl3
                    MinIncrMax.empty |> MinMax.setMin incl4 |> MinMax.setMax incl4
                ]

            mms
            |> List.iter (fun mm -> printfn $"""{mm |> MinIncrMax.toString "from" "to"}""" )

            mms
            |> MinIncrMax.foldMaximize
            |> mmToStr,
            mms
            |> MinIncrMax.foldMinimize
            |> mmToStr


        let inRange () =
            let mm1 = MinIncrMax.empty
            let mm2 =
                MinIncrMax.empty
                |> MinMax.setMin incl1
            let mm3 =
                MinIncrMax.empty
                |> MinMax.setMax incl4
            let mm4 =
                MinIncrMax.empty
                |> MinMax.setMin incl2
                |> MinMax.setMax incl3

            let test v mm =
                $"%s{v |> MinIncrMax.valueToString} in range: %s{mm |> mmToStr} = %A{MinIncrMax.inRange v mm}"


            [
                (incl1, mm1)
                (incl2, mm1)
                (incl3, mm1)
                (incl4, mm1)
                (incl1, mm2)
                (incl2, mm2)
                (incl3, mm2)
                (incl4, mm2)
                (incl1, mm3)
                (incl2, mm3)
                (incl3, mm3)
                (incl4, mm3)
                (incl1, mm4)
                (incl2, mm4)
                (incl3, mm4)
                (incl4, mm4)
            ]
            |> List.map (fun (v, mm) -> test v mm)


        let tests = testList "MinMax" [
            test "minGTmax" {
                incl2 |> MinIncrMax.minGTmax incl1
                |> Expect.isTrue $"{incl2} > {incl1}"
            }

            test "toString" {
                toString()
                |> Expect.equal "should equal" "1 mg - 10 mg"
            }

            test "ageToString" {
                ageToString()
                |> Expect.equal "should equal" "3.0 dag - 1.2 mnd"
            }

            test "valueComparison" {
                valueComp ()
                |> Expect.equal "should equal" [
                    "Inclusive (ValueUnit ([|10N|], Mass (MilliGram 1N))) < Inclusive (ValueUnit ([|20N|], Mass (MilliGram 1N))) = true";
                    "Inclusive (ValueUnit ([|10N|], Mass (MilliGram 1N))) < Inclusive (ValueUnit ([|10N|], Mass (MilliGram 1N))) = false";
                    "Inclusive (ValueUnit ([|10N|], Mass (MilliGram 1N))) <= Inclusive (ValueUnit ([|20N|], Mass (MilliGram 1N))) = true";
                    "Inclusive (ValueUnit ([|10N|], Mass (MilliGram 1N))) <= Inclusive (ValueUnit ([|10N|], Mass (MilliGram 1N))) = true";
                    "Inclusive (ValueUnit ([|10N|], Mass (MilliGram 1N))) > Inclusive (ValueUnit ([|20N|], Mass (MilliGram 1N))) = false";
                    "Inclusive (ValueUnit ([|10N|], Mass (MilliGram 1N))) > Inclusive (ValueUnit ([|10N|], Mass (MilliGram 1N))) = false";
                    "Inclusive (ValueUnit ([|10N|], Mass (MilliGram 1N))) >= Inclusive (ValueUnit ([|20N|], Mass (MilliGram 1N))) = false";
                    "Inclusive (ValueUnit ([|10N|], Mass (MilliGram 1N))) >= Inclusive (ValueUnit ([|10N|], Mass (MilliGram 1N))) = true"
                ]
            }

            test "minimize, maximize" {
                testFold ()
                |> Expect.equal "should equal" ("10 mg - 40 mg", "30 mg - 30 mg")
            }

            test "in range" {
                inRange ()
                |> Expect.equal "should equal" [
                    "incl 10 mg in range:  = true"
                    "incl 20 mg in range:  = true";
                    "incl 30 mg in range:  = true"
                    "incl 40 mg in range:  = true";
                    "incl 10 mg in range: van 10 mg = true";
                    "incl 20 mg in range: van 10 mg = true";
                    "incl 30 mg in range: van 10 mg = true";
                    "incl 40 mg in range: van 10 mg = true";
                    "incl 10 mg in range: tot 40 mg = true";
                    "incl 20 mg in range: tot 40 mg = true";
                    "incl 30 mg in range: tot 40 mg = true";
                    "incl 40 mg in range: tot 40 mg = true";
                    "incl 10 mg in range: 20 mg - 30 mg = false";
                    "incl 20 mg in range: 20 mg - 30 mg = true";
                    "incl 30 mg in range: 20 mg - 30 mg = true";
                    "incl 40 mg in range: 20 mg - 30 mg = false"
                ]
            }
        ]



open Expecto


testList "GenCore" [
//    Tests.MinIncrMaxTests.tests
    Tests.MinMaxTests.tests
]
|> Expecto.run

