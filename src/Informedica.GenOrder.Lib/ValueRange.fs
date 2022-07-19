namespace Informedica.GenOrder.Lib

module ValueRange =

    open Informedica.GenUnits.Lib
    open Informedica.GenSolver.Lib.Types
    open Informedica.GenSolver.Lib.Variable.ValueRange


    /// Convert a `ValueRange` to a `string`.
    let toStringWithUnit exact un vr =
        let fVs (ValueSet vs) = 
            let vs = 
                vs 
                |> Set.toList
                |> List.map (ValueUnit.create un)
                |> List.map ValueUnit.toUnit

            print exact false None false [] None false vs 
    
        let some =
            ValueUnit.create un
            >> ValueUnit.toUnit
            >> Some

        let unr = print exact true None false [] None false []

        let print min minincl max maxincl = print exact false min minincl [] max maxincl []

        let fMin min =
            let incl, min = min |> Minimum.toBoolBigRational
            print (some min) incl None false

        let fMax max =
            let incl, max = max |> Maximum.toBoolBigRational

            print None false (some max) incl

        let fMinMax (min, max) =
            let maxincl, min = min |> Minimum.toBoolBigRational
            let minincl, max = max |> Maximum.toBoolBigRational

            print (some min) minincl (some max) maxincl
        // ToDo: replace print nothing
        let printNothing _ = ""

        let fIncr = printNothing

        let fMinIncr = printNothing

        let fIncrMax = printNothing
    
        vr |> apply unr fMin fMax fMinMax fIncr fMinIncr fIncrMax fVs 


