
#load "Expecto.fsx"
#load "load.fsx"
#load "../Library.fs"

open Expecto
open Informedica.GenCore.Lib




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
|> Expecto.run

