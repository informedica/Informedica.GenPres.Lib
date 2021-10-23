namespace Informedica.GenOrder.Lib

module ValueRange =

    open Informedica.GenUnits.Lib
    open Informedica.GenSolver.Lib.Variable.ValueRange


    /// Convert a `ValueRange` to a `string`.
    let toStringWithUnit exact un vr =
        let fVs vs = 
            let vs = 
                vs 
                |> Set.toList
                |> List.map (ValueUnit.create un)
                |> List.map ValueUnit.toUnit

            print exact false vs None false [] None false
    
        let some =
            ValueUnit.create un
            >> ValueUnit.toUnit
            >> Some

        let fRange =
            let print min minincl incr max maxincl = print exact false [] min minincl incr max maxincl

            let fMin min =
                let incl, min = min |> Minimum.minToBoolBigRational
                print (some min) incl [] None false

            let fMax max =
                let incl, max = max |> Maximum.maxToBoolBigRational

                print None false [] (some max) incl

            let fMinIncr (min, incr)  = 
                let incl, min = min |> Minimum.minToBoolBigRational
                let incr = incr |> Increment.incrToBigRationalSet |> Set.toList
    
                print (some min) incl incr None false

            let fIncrMax (incr, max)  = 
                let incl, max = max |> Maximum.maxToBoolBigRational
                let incr = incr |> Increment.incrToBigRationalSet |> Set.toList
    
                print None false incr (some max) incl

            let fMinMax (min, max) =
                let maxincl, min = min |> Minimum.minToBoolBigRational
                let minincl, max = max |> Maximum.maxToBoolBigRational

                print (some min) minincl [] (some max) maxincl

            let fMinIncrMax (min, incr, max) =
                let maxincl, min = min |> Minimum.minToBoolBigRational
                let minincl, max = max |> Maximum.maxToBoolBigRational
                let incr = incr |> Increment.incrToBigRationalSet |> Set.toList

                print (some min) minincl incr (some max) maxincl

            applyRange fMin fMax fMinIncr fIncrMax fMinMax fMinIncrMax

        let unr = print exact true [] None false [] None false
    
        vr |> apply unr fVs fRange 


