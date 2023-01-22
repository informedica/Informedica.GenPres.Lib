
#load "../../../scripts/Expecto.fsx"
#load "load.fsx"
#load "../MinIncrMax.fs"




module Tests =

    open Expecto
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



open Expecto

Tests.MinIncrMaxTests.tests
|> Expecto.run

