namespace Informedica.GenSolver.Lib


[<AutoOpen>]
module Utils =

    module Constants =


        let MAX_LOOP_COUNT = 10


        let MAX_CALC_COUNT = 5000


        let MAX_BIGINT =
            999999999999999999999999999999999999999999999999I



    module BigRational =

        open MathNet.Numerics


        let denominator (br: BigRational) = br.Denominator

        let numerator (br: BigRational) = br.Numerator



    module Array =

        open Informedica.Utils.Lib.BCL

        let removeBigRationalMultiples xs =
            if xs |> Array.isEmpty then
                xs
            else
                xs
                |> Array.fold
                    (fun acc x1 ->
                        acc
                        |> Array.filter (fun x2 -> x1 = x2 || x2 |> BigRational.isMultiple x1 |> not)
                    )
                    xs



    module ValueUnit =

        open MathNet.Numerics

        open Informedica.Utils.Lib
        open Informedica.Utils.Lib.BCL

        open Informedica.GenUnits.Lib
        open ValueUnit


        let getBaseValue = toBase >> getValue


        let isEmpty = getValue >> Array.isEmpty


        let isZero =
            getValue >> Array.forall ((=) 0N)

        let gtZero =
            getValue >> Array.forall ((<) 0N)

        let gteZero =
            getValue >> Array.forall ((<=) 0N)

        let stZero =
            getValue >> Array.forall ((>) 0N)

        let steZero =
            getValue >> Array.forall ((>=) 0N)


        let minElement =
            applyToValue (Array.min >> Array.singleton)


        let maxElement =
            applyToValue (Array.max >> Array.singleton)


        let multipleOf f incr vu =
            vu
            |> toBase
            |> applyToValue (fun vs ->
                let incr =
                    incr |> getBaseValue |> Set.ofArray

                vs |> Array.map (f incr) |> Array.map snd
            )
            |> toUnit


        let minInclMultipleOf =
            multipleOf BigRational.minInclMultipleOf

        let minExclMultipleOf =
            multipleOf BigRational.minExclMultipleOf


        let maxInclMultipleOf =
            multipleOf BigRational.maxInclMultipleOf

        let maxExclMultipleOf =
            multipleOf BigRational.maxExclMultipleOf


        let denominator =
            getValue >> (Array.map BigRational.denominator)

        let numerator =
            getValue >> (Array.map BigRational.numerator)


        let filter pred =
            toBase
            >> applyToValue (Array.filter pred)
            >> toUnit

        let removeBigRationalMultiples =
            toBase
            >> applyToValue (Array.removeBigRationalMultiples)
            >> toUnit


        let intersect vu1 vu2 =
            vu1
            |> toBase
            |> applyToValue (fun vs ->
                vu2
                |> getBaseValue
                |> Set.ofArray
                |> Set.intersect (vs |> Set.ofArray)
                |> Set.toArray
            )
            |> toUnit


        let isSubset vu1 vu2 =
            let s1 = vu1 |> getBaseValue |> Set.ofArray
            let s2 = vu2 |> getBaseValue |> Set.ofArray
            Set.isSubset s1 s2


        let containsValue vu2 vu1 =
            vu2
            |> toBase
            |> getValue
            |> Array.forall (fun v -> vu1 |> toBase |> getValue |> Array.exists ((=) v))

        let takeFirst n = applyToValue (Array.take n)

        let takeLast n =
            applyToValue (Array.rev >> Array.take n >> Array.rev)


        let setSingleValue v vu = vu |> getUnit |> withValue [| v |]


        let setValue v vu = vu |> getUnit |> withValue v


        // ToDo replace with this
        let valueCount = getValue >> Array.length


        let toStr exact =
            if exact then
                getValue
                >> Array.toReadableString
                >> String.removeBrackets
            else
                toReadableDutchStringWithPrec 3



        module Operators =

            /// Constant 0
            let zero =
                [| 0N |] |> create Units.Count.times

            /// Constant 1
            let one =
                [| 1N |] |> create Units.Count.times

            /// Constant 2
            let two =
                [| 2N |] |> create Units.Count.times

            /// Constant 3
            let three =
                [| 3N |] |> create Units.Count.times

            /// Check whether the operator is subtraction
            let opIsSubtr op = (three |> op <| two) = three - two // = 1

            /// Check whether the operator is addition
            let opIsAdd op = (three |> op <| two) = three + two // = 5

            /// Check whether the operator is multiplication
            let opIsMult op = (three |> op <| two) = three * two // = 6

            /// Check whether the operator is divsion
            let opIsDiv op = (three |> op <| two) = three / two // = 3/2



            /// Match an operator `op` to either
            /// multiplication, division, addition
            /// or subtraction, returns `NoOp` when
            /// the operation is neither.
            let (|Mult|Div|Add|Subtr|) op =
                match op with
                | _ when op |> opIsMult -> Mult
                | _ when op |> opIsDiv -> Div
                | _ when op |> opIsAdd -> Add
                | _ when op |> opIsSubtr -> Subtr
                | _ -> failwith "Operator is not supported"