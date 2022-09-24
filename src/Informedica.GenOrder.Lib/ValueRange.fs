namespace Informedica.GenOrder.Lib

module ValueRange =

    open Informedica.GenUnits.Lib
    open Informedica.GenSolver.Lib.Variable.ValueRange


    /// Convert a `ValueRange` to a `string`.
    let toStringWithUnit exact un vr =
        let toUnit = ValueUnit.create un >> ValueUnit.toUnit

        let fVs vs =
            vs
            |> ValueSet.map toUnit
            |> Some
            |> print exact None None None

        let unr = print exact None None None None

        let print min incr max = print exact min incr max None

        let fMin min =
            print (min |> Minimum.map toUnit toUnit |> Some) None None

        let fMax max =
            print None None (max |> Maximum.map toUnit toUnit |> Some)

        let fMinMax (min, max) =
            print
                (min |> Minimum.map toUnit toUnit |> Some)
                None
                (max |> Maximum.map toUnit toUnit |> Some)

        let fIncr incr =
            print None (incr |> Increment.map toUnit |> Some) None

        let fMinIncr (min, incr) =
            print
                (min |> Minimum.map toUnit toUnit |> Some)
                (incr |> Increment.map toUnit |> Some)
                None

        let fIncrMax (incr, max) =
            print
                None
                (incr |> Increment.map toUnit |> Some)
                (max |> Maximum.map toUnit toUnit |> Some)

        let fMinIncrMax (min, incr, max) =
            print
                (min |> Minimum.map toUnit toUnit |> Some)
                (incr |> Increment.map toUnit |> Some)
                (max |> Maximum.map toUnit toUnit |> Some)

        vr |> apply unr fMin fMax fMinMax fIncr fMinIncr fIncrMax fMinIncrMax fVs


