
#load "../../../scripts/Expecto.fsx"
#load "load.fsx"
#load "../MinIncrMax.fs"
#load "../MinMax.fs"



module Tests =

    open Expecto
    open Expecto.Flip

    open Informedica.GenUnits.Lib
    open Informedica.GenCore.Lib
    open Informedica.Utils.Lib.BCL


    module MinIncrMaxTests =

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
                    MinIncrMax.validate min incr max
                    |> function
                    | Error _ -> ()
                    | Ok (min, incr, max) ->
                        let toBrStr = BigRational.toFloat >> Double.toStringNumberNLWithoutTrailingZerosFixPrecision 3
                        MinIncrMax.toString toBrStr min incr max
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
                    MinIncrMax.validate min incr max
                    |> function
                    | Error _ -> ()
                    | Ok (min, incr, max) ->
                        let toBrStr = BigRational.toFloat >> Double.toStringNumberNLWithoutTrailingZerosFixPrecision 3
                        MinIncrMax.toStringNL toBrStr min incr max
                        |> printfn "Pass: %s"
                    true
                with
                | _ -> false
            |> Generators.testProp "with simple integers dutch version never throws an exception"


            fun min incr max ->
                try
                    MinIncrMax.validate min incr max
                    |> ignore
                    true
                with
                | _ -> false
            |> Generators.testProp "never throws an exception"


            fun min incr max ->
                MinIncrMax.validate min incr max
                |> function
                | Ok (Some min, _, Some max) ->
                    let min = min |> snd
                    let max = max |> snd
                    min <= max
                | _ -> true
            |> Generators.testProp "min always is equal or less than max"
        ]



    module MinMaxTests =

        module MinMax = MinMax.Optics

        let mmToStr = MinMax.toString "van" "tot"


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
            v1 |> MinMax.inclusive,
            v2 |> MinMax.inclusive

        let v3, v4 =
            createValueUnit 30.m "mg[Mass]" |> Option.get ,
            createValueUnit 40.m "mg[Mass]" |> Option.get

        let incl3, incl4 =
            v3 |> MinMax.inclusive,
            v4 |> MinMax.inclusive


        let toString () =
            MinMax.empty
            |> MinMax.setMin (createValueUnit 1.m "mg[Mass]"  |> Option.get |> MinMax.Inclusive)
            |> MinMax.setMax (createValueUnit 10.m "mg[Mass]" |> Option.get |> MinMax.Inclusive)
            |> mmToStr


        let fromDecimal (v: decimal) u =
            v
            |> BigRational.fromDecimal
            |> ValueUnit.createSingle u


        let ageInMo =  (fun n -> fromDecimal n ValueUnit.Units.Time.month)


        let ageInYr =  (fun n -> fromDecimal n ValueUnit.Units.Time.year)


        let a1, a2 =
            0.1m |> ageInMo |> MinMax.Inclusive,
            0.1m |> ageInYr |> MinMax.Exclusive


        let ageToString () =
            MinMax.empty
            |> MinMax.setMin a1
            |> MinMax.setMax a2
            |> MinMax.ageToString


        let valueComp () =
            [
                 $"%A{incl1} < %A{incl2} = %A{MinMax.valueST incl1 incl2}"
                 $"%A{incl1} < %A{incl1} = %A{MinMax.valueST incl1 incl1}"
                 $"%A{incl1} <= %A{incl2} = %A{MinMax.valueSTE incl1 incl2}"
                 $"%A{incl1} <= %A{incl1} = %A{MinMax.valueSTE incl1 incl1}"
                 $"%A{incl1} > %A{incl2} = %A{MinMax.valueLT incl1 incl2}"
                 $"%A{incl1} > %A{incl1} = %A{MinMax.valueLT incl1 incl1}"
                 $"%A{incl1} >= %A{incl2} = %A{MinMax.valueLTE incl1 incl2}"
                 $"%A{incl1} >= %A{incl1} = %A{MinMax.valueLTE incl1 incl1}"
            ]



        // ToDo handle None cases correctly?
        let testFold () =
            let mms =
                [
                    MinMax.empty
                    MinMax.empty |> MinMax.setMin incl1
                    MinMax.empty |> MinMax.setMin incl2
                    MinMax.empty |> MinMax.setMax incl3
                    MinMax.empty |> MinMax.setMax incl4
                    MinMax.empty |> MinMax.setMin incl1 |> MinMax.setMax incl3
                    MinMax.empty |> MinMax.setMin incl2 |> MinMax.setMax incl3
                    MinMax.empty |> MinMax.setMin incl3 |> MinMax.setMax incl3
                    MinMax.empty |> MinMax.setMin incl4 |> MinMax.setMax incl4
                ]

            mms
            |> MinMax.foldMaximize
            |> mmToStr,
            mms
            |> MinMax.foldMinimize
            |> mmToStr


        let inRange () =
            let mm1 = MinMax.empty
            let mm2 =
                MinMax.empty
                |> MinMax.setMin incl1
            let mm3 =
                MinMax.empty
                |> MinMax.setMax incl4
            let mm4 =
                MinMax.empty
                |> MinMax.setMin incl2
                |> MinMax.setMax incl3

            let test v mm =
                $"%s{v |> MinMax.valueToString} in range: %s{mm |> mmToStr} = %A{MinMax.inRange v mm}"


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

