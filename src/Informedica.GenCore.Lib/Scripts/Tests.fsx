

#load "load.fsx"

#load "../Library.fs"


open Informedica.GenCore.Lib


/// Create the necessary test generators
module Generators =

    open Expecto
    open FsCheck
    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL


    let bigRGen (n, d) =
        let d = if d = 0 then 1 else d
        let n = abs(n) |> BigRational.FromInt
        let d = abs(d) |> BigRational.FromInt
        n / d


    let bigRGenerator =
        gen {
            let! n = Arb.generate<int>
            let! d = Arb.generate<int>
            return bigRGen(n, d)
        }


    type BigRGenerator () =
        static member BigRational () =
            { new Arbitrary<BigRational>() with
                override x.Generator = bigRGenerator
            }


    let config = {
        FsCheckConfig.defaultConfig with
            arbitrary = [
                typeof<BigRGenerator>
            ]
            maxTest = 1000
        }


    let testProp testName prop =
        prop |> testPropertyWithConfig config testName


    let run = runTestsWithCLIArgs [] [|"--summary" |]



module Tests =

    open Informedica.Utils.Lib.BCL
    open Expecto


    module MinIncrMaxTests =

        [<Tests>]
        let tests = testList "MinIncrMax.toIncrement" [
            fun min incr max ->
                let min =
                    min
                    |> Option.map (fun (minIncl, min) -> minIncl, min |> BigRational.fromInt)
                let max =
                    max
                    |> Option.map (fun (maxIncl, max) -> maxIncl, max |> BigRational.fromInt)
                let incr = incr |> Set.map (BigRational.fromInt) |> Some

                try
                    MinIncrMax.toIncrement min incr max
                    |> function
                    | None -> ()
                    | Some (min, incr, max) ->
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
                let incr = incr |> Set.map (BigRational.fromInt) |> Some

                try
                    MinIncrMax.toIncrement min incr max
                    |> function
                    | None -> ()
                    | Some (min, incr, max) ->
                        let toBrStr = BigRational.toFloat >> Double.toStringNumberNLWithoutTrailingZerosFixPrecision 3
                        MinIncrMax.toStringNL toBrStr min incr max
                        |> printfn "Pass: %s"
                    true
                with
                | _ -> false
            |> Generators.testProp "with simple integers dutch version never throws an exception"


            fun min incr max ->
                try
                    MinIncrMax.toIncrement min incr max
                    |> ignore
                    true
                with
                | _ -> false
            |> Generators.testProp "never throws an exception"


        ]



Tests.MinIncrMaxTests.tests
|> Generators.run


"vanaf -11 per 1  tot en met 6"