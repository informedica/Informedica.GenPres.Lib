namespace Informedica.GenOrder.Lib


module Variable =

    open Informedica.GenSolver.Lib.Types

    module ValueRange =

        open Informedica.GenSolver.Lib.Variable.ValueRange


        let scale n vr =
            let calc = (*) n
            let mapMin = Minimum.map calc calc
            let mapMax = Maximum.map calc calc

            let fVs vs =
                vs
                |> ValueSet.map calc

            let fMinMax (min, max) =
                min |> mapMin,
                max |> mapMax

            let fIncr = Increment.map calc

            let fMinIncr (min, incr) =
                min |> mapMin,
                incr |> Increment.map calc

            let fIncrMax (incr, max) =
                incr |> Increment.map calc,
                max |> mapMax

            let fMinIncrMax (min, incr, max) =
                min |> mapMin,
                incr |> Increment.map calc,
                max |> mapMax

            vr |> map mapMin mapMax fMinMax fIncr fMinIncr fIncrMax fMinIncrMax fVs


        let inline setOpt m set vr =
            match m with
            | Some m -> vr |> set true m
            | None   -> vr


        let setOptMin min vr = vr |> setOpt min setMin


        let setOptMax max vr = vr |> setOpt max setMax


        let setOptIncr incr vr = vr |> setOpt incr setIncr


        let setOptVs vs vr =
            match vs with
            | Some vs -> vr |> setValueSet vs
            | None    -> vr


    let scale n (var : Variable) =
        { var with Values = var.Values |> ValueRange.scale n }



