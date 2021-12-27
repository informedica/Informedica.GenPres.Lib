namespace Informedica.GenSolver.Lib



/// Contains functions and types to represent
/// a `Variable` in an `Equation`:
///
/// A `Variable` is the basic calculation unit in an `Equation`. The 'Variable` 
/// identified by its name stores the range of possible values a variable can have 
/// in an equation. 
/// 
//// The notation for this is:
/// * <..>   : meaning a variable can be any rational number
/// * [0N..> : meaning that the variable can be any number larger than or equal to 0N
/// * <0N..> : meaning that the variable can be any number larger than but excluding 0N
/// * [0N..10N> : meaning that the variable must be between 0N up to but excluding 10N
/// * [0N..10N] : meaning that the variable can be 0N up to and including 10N
/// * [1N,3N,4N,5N] : meaning that the variable can only be one of the numbers
///
/// * `Name` : A `Variable` is identified by its `Name`
module Variable =

    open System
    open MathNet.Numerics

    open Types



    module Name =

        open System
        open Informedica.Utils.Lib.BCL


        /// Eceptions that `Name` functions can raise
        module Exceptions =

            type Message =
            | NullOrWhiteSpaceException
            | LongerThan1000 of string

            /// `ValueRange` exception type
            exception NameException of Message

            /// Raise a `ValueRangeException` with `Message` **m**.
            let raiseExc m = m |> NameException |> raise

        /// Create with continuation with **succ** function
        /// when success and **fail** function when failure.
        /// Creates a `Name` from a`string`.
        let create succ fail n =
            if n |> String.IsNullOrWhiteSpace then
                Exceptions.NullOrWhiteSpaceException
                |> fail
            else
                match n |> String.trim with
                | n' when n' |> String.length <= 1000 -> n' |> Name |> succ
                | n' ->
                    n'
                    |> Exceptions.LongerThan1000
                    |> fail

        /// Returns a `Name` option if creation
        /// succeeds else `None`.
        let createOpt = create Some Option.none

        /// Create a `Name` that, raises
        /// an `NameException` when it fails.
        let createExc = create id Exceptions.raiseExc

        /// Return the `string` value of a `Name`.
        let toString (Name s) = s



    /// Functions and types to create and handle `ValueRange`.
    module ValueRange =

        open Informedica.Utils.Lib.BCL



        module Exceptions =


            exception ValueRangeException of Exceptions.Message


            let raiseExc m = m |> ValueRangeException |> raise


            let raiseMinLargerThanMax min max =
                (min, max)
                |> Exceptions.ValueRangeMinLargerThanMax
                |> raiseExc



        module Minimum =

            /// Apply **f** to the bigrational
            /// value of `Minimum`
            let apply fincl fexcl = function
                | MinIncl(m) -> m |> fincl
                | MinExcl(m) -> m |> fexcl

            /// Checks whether `Minimum` **m2** > **m1**
            /// Note that the fact that a Minimum is inclusive or exclusive 
            /// must be taken into account.
            let minLTmin m1 m2 =
                match m2, m1 with
                | MinIncl m2', MinIncl m1'
                | MinExcl m2', MinExcl m1'
                | MinIncl m2', MinExcl m1' -> m2' > m1'
                | MinExcl m2', MinIncl m1' -> m2' >= m1'

            /// Checks whether `Minimum` **m2** <= **m1**
            let minSTEmin m1 m2 = m2 |> minLTmin m1 |> not

            /// Checks whether `Minimum` is exclusive.
            let isMinExcl = function | MinIncl _ -> false | MinExcl _ -> true

            /// Checks whether `Minimum` is inclusive.
            let isMinIncl = isMinExcl >> not

            /// Creates a `Minimum` from a `BigRational` set. 
            /// Returns `None` if an empty set.
            let getSetMin s =
                if s |> Set.isEmpty then None
                else
                    s
                    |> Set.minElement
                    |> MinIncl
                    |> Some

            /// Convert a `Minimum` to a `BigRational`.
            let minToBigRational = function | MinIncl v | MinExcl v -> v

            /// Convert a `Minimum` to a `BigRational` and a `bool`
            /// that signifies inclusive or exclusive
            let minToBoolBigRational =
                apply (fun br -> true, br) (fun br -> false, br)

            /// Create a `Minimum` that is
            /// either inclusive or exclusive.
            let createMin isIncl m = if isIncl then m |> MinIncl else m |> MinExcl



        module Maximum =

            /// Apply **f** to the bigrational
            /// value of `Maximum`
            let apply fincl fexcl = function
            | MaxIncl(m) -> m |> fincl
            | MaxExcl(m) -> m |> fexcl

            /// Checks whether `Maximum` **m2** > **m1**
            /// Note that the fact that a maximum is inclusive or exclusive
            /// must be taken into account.
            let maxLTmax m1 m2 =
                match m2, m1 with
                | MaxIncl m2', MaxIncl m1'
                | MaxExcl m2', MaxExcl m1'
                | MaxExcl m2', MaxIncl m1' -> m2' > m1'
                | MaxIncl m2', MaxExcl m1' -> m2' >= m1'

            /// Checks whether `Maximum` **m2** <= **m1**
            let maxSTEmax m1 m2 = m2 |> maxLTmax m1 |> not

            /// Get the maximum value in a `BigRational` set. 
            /// Returns `None` if an empty set.
            let getSetMax s =
                if s |> Set.isEmpty then None
                else
                    s
                    |> Set.maxElement
                    |> MaxIncl
                    |> Some

            /// Convert a `Maximum` to a `BigRational`.
            let maxToBigRational = function | MaxIncl v | MaxExcl v -> v

            /// Checks whether `Maximum` is exclusive.
            let isMaxExcl = function | MaxIncl _ -> false | MaxExcl _ -> true

            /// Checks whether `Maximum` is inclusive.
            let isMaxIncl = isMaxExcl >> not

            /// Turn a `Maximum` into a `BigRational` and a `bool` to indicate
            /// inclusive or exclusive.
            let maxToBoolBigRational =
                apply (fun m -> true, m) (fun m -> false, m)

            /// Create a `Maximum` that is
            /// either inclusive or exclusive.
            let createMax isIncl m = if isIncl then m |> MaxIncl else m |> MaxExcl



        module Range =


            let create delta multiples = 
                {
                    Multiples = multiples
                    Delta = delta
                }


            let getMin r =
                r.Multiples
                |> Set.minElement
                |> BigRational.fromBigInt
                |> fun x -> x * r.Delta
                |> Minimum.createMin true


            let getMax r =
                r.Multiples
                |> Set.maxElement
                |> BigRational.fromBigInt
                |> fun x -> x * r.Delta
                |> Maximum.createMax true



        module ValueSet =


            /// Create a `ValueSet` from a set of `BigRational`.
            let create s =
                if s |> Seq.isEmpty then
                    Exceptions.ValueRangeEmptyValueSet
                    |>Exceptions.raiseExc

                else
                    s
                    |> Set.ofSeq 
                    |> ValueSet


            let getMin vs =
                vs 
                |> Set.minElement
                |> Minimum.createMin true


            let getMax vs =
                vs 
                |> Set.maxElement
                |> Maximum.createMax true



        /// Aply the give functions to `Values`
        let apply unr fMin fMax fMinMax fRange fValueSet = function
            | Unrestricted      -> unr
            | Min min           -> min |> fMin
            | Max max           -> max |> fMax
            | MinMax (min, max) -> (min, max) |> fMinMax
            | Range r           -> r  |> fRange
            | ValueSet vs       -> vs |> fValueSet


        /// Count the number of values in a `ValueRange`.
        /// Returns 0 if no count is possible.
        let count =
            let zero _ = 0
            let countRange r = r.Multiples |> Set.count 
            apply 0 zero zero zero countRange Set.count


        /// Checks whether a `ValueRange` is `Unrestricted`
        let isUnrestricted =
            let returnFalse = Boolean.returnFalse
            apply true returnFalse returnFalse returnFalse returnFalse returnFalse


        /// Checks whether a `ValueRange` is `Min`
        let isMin =
            let returnFalse = Boolean.returnFalse
            apply false Boolean.returnTrue returnFalse returnFalse returnFalse returnFalse


        /// Checks whether a `ValueRange` is `Max`
        let isMax =
            let returnFalse = Boolean.returnFalse
            apply true returnFalse Boolean.returnTrue returnFalse returnFalse returnFalse


        /// Checks whether a `ValueRange` is `MinMax`
        let isMinMax =
            let returnFalse = Boolean.returnFalse
            apply true returnFalse returnFalse Boolean.returnTrue returnFalse returnFalse


        /// Checks whether a `ValueRange` is a `Range`
        let isRange =
            let returnFalse = Boolean.returnFalse
            apply true returnFalse returnFalse returnFalse Boolean.returnTrue returnFalse


        /// Checks whether a `ValueRange` is a `ValueSet`
        let isValueSet =
            let returnFalse = Boolean.returnFalse
            apply false returnFalse returnFalse returnFalse returnFalse Boolean.returnTrue


        /// Checks whether a `BigRational` is between an optional 
        /// **min** and an optional **max** 
        let isBetween min max v =
            let fMin = function
            | None -> true
            | Some(Minimum.MinIncl m) -> v >= m
            | Some(Minimum.MinExcl m) -> v > m

            let fMax  = function
            | None -> true
            | Some(Maximum.MaxIncl m) -> v <= m
            | Some(Maximum.MaxExcl m) -> v < m

            (fMin min) && (fMax max)


        /// Checks whether `Minimum` **min** > `Maximum` **max**.
        /// Note that inclusivity or exclusivity of a minimum and maximum must be
        /// accounted for.
        let minLTmax max min =
            match min, max with
            | Minimum.MinIncl min', Maximum.MaxIncl max' -> min' > max'
            | Minimum.MinExcl min', Maximum.MaxIncl max'
            | Minimum.MinExcl min', Maximum.MaxExcl max'
            | Minimum.MinIncl min', Maximum.MaxExcl max' -> min' >= max'


        /// Checks whether `Minimum` **min** <= `Maximum` **max**
        let minSTEmax max min = min |> minLTmax max |> not


        /// Checks whether `Minimum` **min** = `Maximum` **max**.
        /// Note that when one or both minimum and maximum are exclusive, they 
        /// cannot equal each other!
        let minEQmax max min =
            match min, max with
            | Minimum.MinIncl min', Maximum.MaxIncl max' -> min' = max'
            | _ -> false


        /// Filter a set of `BigRational` according
        /// to **min** and **max** constraints
        let filter min max = Set.filter (isBetween min max)


        /// Create a string (to print) representation of a `ValueRange`. 
        /// `Exact` true prints exact bigrationals, when false
        /// print as floating numbers
        let print exact unr min minincl max maxincl vals =

            let printVals vals =
                let vals =
                    vals
                    |> List.sort
                    |> List.map (if exact then BigRational.toString
                                 else BigRational.toFloat >> sprintf "%A")

                "[" + (vals |> List.fold (fun s v -> if s = "" then v else s + ", " + v) "") + "]"

            let printRange min max =
                if unr then "<..>"
                else
                    let left  = if minincl then "[" else "<"
                    let right = if maxincl then "]" else ">"

                    let brToStr br =
                        if exact then
                            br
                            |> sprintf "%A"
                        else
                            br
                            |> BigRational.toFloat
                            |> sprintf "%A"

                    match min, max with
                    | Some min, None ->
                        sprintf "%s%s..>" left (min |> brToStr)
                    | Some min, Some max ->
                        sprintf "%s%s..%s%s" left (min |> brToStr) (max |> brToStr) right
                    | None,     Some max ->
                        sprintf "<..%s%s" (max |> brToStr) right
                    | _ -> "[]"

            let vals =
                if vals |> List.isEmpty |> not then vals |> printVals
                else
                    printRange min max

            sprintf "%s" vals


        /// Convert a `ValueRange` to a `string`.
        let toString exact vr =
            let fVs vs =
                print exact false None false None false (vs |> Set.toList) 

            let print min minincl max maxincl = print exact false min minincl max maxincl []

            let fMin min =
                let incl, min = min |> Minimum.minToBoolBigRational
                print (Some min) incl None false

            let fMax max =
                let incl, max = max |> Maximum.maxToBoolBigRational
                print None false (Some max) incl

            let fMinMax (min, max) =
                let minincl, min = min |> Minimum.minToBoolBigRational
                let maxincl, max = max |> Maximum.maxToBoolBigRational
                print (Some min) minincl (Some max) maxincl

            let unr = print None false None false

            vr |> apply unr fMin fMax fMinMax (fun _ -> "") fVs 


        /// An `Unrestricted` `ValueRange`.
        let unrestricted = Unrestricted

        
        /// Create a `Minimum` `Range` that is
        /// either inclusive or exclusive.
        let createMinRange isIncl m = m |> Minimum.createMin isIncl |> Min

        
        /// Create a `Maximum` `Range` that is
        /// either inclusive or exclusive.
        let createMaxRange isIncl m = m |> Maximum.createMax isIncl |> Max


        /// Create a `MinMax` `ValueRange`. If **min** > **max** raises 
        /// an `MinLargetThanMax` exception. If min equals max, a `ValueSet` with
        /// value min (= max).
        let minMaxToValueRange min max =
            if min |> minLTmax max then 
                Exceptions.raiseMinLargerThanMax min max
            
            elif min |> minEQmax max then
                min 
                |> Minimum.minToBigRational
                |> Set.singleton
                |> ValueSet.create

            else (min, max) |> MinMax



        /// Create a `ValueRange` using a `ValueSet` **vs**
        /// an optional `Minimum` **min** and `Maximum` **max**.
        /// If both **min** and **max** are `None` an `Unrestricted` 
        /// `ValueRange` is created. 
        let create min max vs =
            match vs with
            | None ->
                match min, max with
                | None,      None      -> unrestricted
                | Some min', None      -> min' |> Min
                | None,      Some max' -> max' |> Max
                | Some min', Some max' -> minMaxToValueRange min' max'
            | Some vs -> vs |> ValueSet.create


        /// Get an optional `Minimum` in a `ValueRange`
        let getMin = 
            apply None 
                  Some
                  Option.none
                  (fst >> Some)
                  (Range.getMin >> Some)
                  (ValueSet.getMin >> Some)


        
        /// Get an optional `Maximum` in a `ValueRange`
        let getMax = 
            apply None 
                  Option.none
                  Some
                  (snd >> Some)
                  (Range.getMax >> Some)
                  (ValueSet.getMax >> Some)


        /// Get a set of `BigRational` from a `ValueRange`,
        /// returns an `None` when `ValueRange` is not
        /// a `ValueSet` 
        let getValueSet = apply None Option.none Option.none Option.none Option.none Some

        
        /// Check whether a `ValueRange` **vr** contains
        /// a `BigRational` **v**.
        let contains v vr =
            match vr with
            | ValueSet vs -> vs |> Set.contains v
            | Range _ -> "not supported" |> failwith
            | _ ->
                let min = vr |> getMin
                let max = vr |> getMax

                v |> isBetween min max


        /// Apply a `Minimum` **min** to a `ValueRange` **vr**.
        /// If minimum cannot be set the original `Minimum` is returned.
        /// So, it always returns a more restrictive, i.e. larger, or equal `Minimum`.
        let setMin min (vr: ValueRange) =
            // Check whether the new min is more restrictive than the old min
            let checkMin min' = 
                if min |> Minimum.minLTmin min' then min else min'

            let fMin = checkMin >> Min

            let fMax max = minMaxToValueRange min max

            let fMinMax (min', max) = minMaxToValueRange (min' |> checkMin) max

            let fValueSet =
                let max = vr  |> getMax
                filter (Some min) max >> ValueSet.create

            let fRange _ = vr

            vr
            |> apply (min |> Min) fMin fMax fMinMax fRange fValueSet


        /// Apply a `Maximum` **max** to a `ValueRange` **vr**.
        /// If maximum cannot be set the original is returned.
        /// So, it always returns a more restrictive, i.e. smaller, or equal `Maximum`.
        let setMax max vr =
            // Check whether the new min is more restrictive than the old min
            let checkMax max' = 
                if max |> Maximum.maxLTmax max' then max' else max

            let fMin min = minMaxToValueRange min max

            let fMax = checkMax >> Max

            let fMinMax (min, max') = minMaxToValueRange min (max' |> checkMax) 

            let fValueSet =
                let min = vr  |> getMin
                filter min (Some max) >> ValueSet.create

            let fRange _ = vr

            vr
            |> apply (max |> Max) fMin fMax fMinMax fRange fValueSet



        /// Appy a set of `BigRational` to a `ValueRange` **vr**.
        /// the result is a filtered or the intersect of
        /// the set of `BigRational` and **vr**.
        let setValues vs vr =

            let vs1, min, max =
                vr |> getValueSet,
                vr |> getMin,
                vr |> getMax

            let vs2 =
                match vs1 with
                | None ->
                    vs
                    |> filter min max
                | Some vs1 ->
                    vs
                    |> filter min max
                    |> Set.intersect vs1

            create min max (Some vs2) 


        /// Functions to calculate the `Minimum`
        /// and `Maximum` in a `ValueRange`.
        /// I.e. what happens when you mult, div, add or subtr 
        /// a `Range`, for example:
        /// <1N..3N> * <4N..5N> = <4N..15N>
        module MinMaxCalcultor =

            /// Exceptions that a MinMaxCalculator function can raise.
            module Exceptions =

                type Message = | NotAValidOperator

                exception MinMaxCalculatorException of Message

                let raiseExc = MinMaxCalculatorException >> raise


            /// Calculate **x1** and **x2** with operator **op**
            /// and use **incl1** and **inc2** to determine whether
            /// the result is inclusive. Use constructor **c** to
            /// create the optional result.
            let calc c op (x1, incl1) (x2, incl2) =
                let incl = 
                    match incl1, incl2 with
                    | true, true -> true
                    | _          -> false

                match x1, x2 with
                | Some (v1), Some (v2) ->
                    if op |> BigRational.opIsDiv && v2 = 0N then None
                    else 
                        v1 |> op <| v2 |> c incl |> Some
                | _ -> None


            /// Calculate an optional `Minimum`
            let calcMin = calc Minimum.createMin


            /// Calculate an optional `Maximum`
            let calcMax = calc Maximum.createMax


            /// Match a min, max tuple **min**, **max**
            /// to:
            ///
            /// * `PP`: both positive
            /// * `NN`: both negative
            /// * `NP`: one negative, the other positive
            let (|PP|NN|NP|) (min, max) =
                match min, max with
                | Some(min), _ when min >= 0N -> PP
                | _, Some(max) when max < 0N  -> NN
                | Some(min), Some(max) when min < 0N && max >= 0N ->  NP
                | _ -> NP


            /// Calculate `Minimum` option and
            /// `Maximum` option for addition of
            /// (**min1**, **max1**) and (**min2**, **max2)
            let addition min1 max1 min2 max2 =
                let min = calcMin (+) min1 min2
                let max = calcMax (+) max1 max2
                min, max


            /// Calculate `Minimum` option and
            /// `Maximum` option for subtraction of
            /// (**min1**, **max1**) and (**min2**, **max2)
            let subtraction min1 max1 min2 max2 =
                let min = calcMin (-) min1 max2
                let max = calcMax (-) max1 min2
                min, max


            /// Calculate `Minimum` option and
            /// `Maximum` option for multiplication of
            /// (**min1**, **max1**) and (**min2**, **max2)
            let multiplication min1 max1 min2 max2 =
                match ((min1 |> fst), (max1 |> fst)), ((min2 |> fst), (max2 |> fst)) with
                | PP, PP ->  // min = min1 * min2, max = max1 * max2
                    calcMin (*) min1 min2, calcMax (*) max1 max2
                | PP, NN -> // min = max1 * min2, max = min1 * max2
                    calcMin (*) max1 min2, calcMax (*) min1 max2
                | PP, NP -> // min = min1 * min2, max = max1 * max2
                    calcMin (*) min1 min2, calcMax (*) max1 max2
                | NN, PP -> // min = min1 * max2, max = max1 * min2
                    calcMin (*) min1 max2, calcMax (*) max1 min2
                | NN, NN -> // min = max1 * max2, max = min1 * min2
                    calcMin (*) max1 max2, calcMax (*) min1 min2
                | NN, NP -> // min = min1 * max2, max = min1 * min2
                    calcMin (*) min1 max2, calcMax (*) min1 min2
                | NP, PP -> // min = min1 * max2, max = max1 * max2
                    calcMin (*) min1 max2, calcMax (*) max1 max2
                | NP, NN -> // min = max1 * min2, max = min1 * min2
                    calcMin (*) max1 min2, calcMax (*) min1 min2
                | NP, NP -> // min = min1 * max2, max = max1 * max2
                    calcMin (*) min1 max2, calcMax (*) max1 max2


            /// Calculate `Minimum` option and
            /// `Maximum` option for division of
            /// (**min1**, **max1**) and (**min2**, **max2)
            let division min1 max1 min2 max2 =
                match (min1 |> fst, max1 |> fst), (min2 |> fst, max2 |> fst) with
                | PP, PP -> // min = min1 / max2, max =	max1 / min2
                    calcMin (/) min1 max2, calcMax (/) max1 min2
                | PP, NN -> // min = max1 / max2	, max = min1 / min2
                    calcMin (/) max1 max2, calcMax (/) min1 min2
                | NN, PP -> // min = min1 / min2, max = max1 / max2
                    calcMin (/) min1 min2, calcMax (/) max1 max2
                | NN, NN -> // min = max1 / min2	, max = min1 / max2
                    calcMin (/) max1 min2, calcMax (/) min1 max2
                | NP, PP -> // min = min1 / min2, max = max1 / min2
                    calcMin (/) min1 min2, calcMax (/) max1 min2
                | NP, NN -> // min = max1 / max2, max = min1 / max2
                    calcMin (/) max1 max2, calcMax (/) min1 max2
                // division by range containing zero
                | NN, NP
                | PP, NP
                | NP, NP -> None, None


            /// Match the right minmax calcultion
            /// according to the operand
            let calcMinMax = function
                | BigRational.Mult  -> multiplication
                | BigRational.Div   -> division
                | BigRational.Add   -> addition
                | BigRational.Subtr -> subtraction
                | BigRational.NoMatch ->
                    Exceptions.NotAValidOperator
                    |> Exceptions.raiseExc



        /// Applies an infix operator **op**
        /// to `ValueRange` **x1** and **x2**.
        /// Calculates `Minimum`, increment or `Maximum`
        /// if either **x1** or **x2** is not a `ValueSet`.
        /// Doesn't perform any calculation when both
        /// **x1** and **x2** are `Unrestricted`.
        let calc op (x1, x2) =

            match x1, x2 with
            | Unrestricted, Unrestricted -> unrestricted
            | ValueSet s1, ValueSet s2 ->
                // When one of the sets does not contain any value then the result of
                // of the calculation cannot contain any value either
                if s1 |> Set.isEmpty || s2 |> Set.isEmpty then
                    Exceptions.ValueRangeEmptyValueSet
                    |> Exceptions.raiseExc

                else
                    Seq.allPairs s1 s2
                    |> Seq.map (fun (x1, x2) -> x1 |> op <| x2)
                    |> ValueSet.create


            // In any other case calculate min, incr and max
            | _ ->
                let min1, max1 = x1 |> getMin, x1 |> getMax
                let min2, max2 = x2 |> getMin, x2 |> getMax

                let min, max =
                    let getMin m =
                        let incl =
                            match m with
                            | Some v -> v |> Minimum.isMinIncl
                            | None   -> false
                        m |> Option.bind (Minimum.minToBigRational >> Some), incl

                    let getMax m =
                        let incl =
                            match m with
                            | Some v -> v |> Maximum.isMaxIncl
                            | None   -> false
                        m |> Option.bind (Maximum.maxToBigRational >> Some), incl

                    MinMaxCalcultor.calcMinMax
                        op
                        (min1 |> getMin)
                        (max1 |> getMax)
                        (min2 |> getMin)
                        (max2 |> getMax)


                match min, max with
                | None, None -> unrestricted
                | _ -> create min max None


        /// Checks whether a `ValueRange` vr1 is a subset of 
        /// `ValueRange` vr2.
        let isSubSetOf vr2 vr1 =
            match vr1, vr2 with
            | ValueSet s1, ValueSet s2 ->
                s2 |> Set.isSubset s1
            | _ -> false


        /// Set a `ValueRange` expr to a `ValueRange` y.
        /// So, the result is equal to or more restrictive than the original `y`. 
        let applyExpr y expr =
            let set get set vr =
                match expr |> get with
                | Some m -> vr |> set m
                | None   -> vr

            match expr with
            | Unrestricted -> y
            | ValueSet vs  ->
                if vs |> Set.isEmpty then
                    Exceptions.ValueRangeEmptyValueSet
                    |> Exceptions.raiseExc

                else y |> setValues vs
            | _ ->
                y
                |> set getMin setMin
                |> set getMax setMax


        // Extend type with basic arrhythmic operations.
        type ValueRangeCalc =
             | Mult 
             | Div
             | Add 
             | Subtr
             | Expr with

            static member inline (?<-) (op, vr1, vr2) = 
                match op with
                | Mult  -> calc (*) (vr1, vr2) 
                | Div   -> calc (/) (vr1, vr2) 
                | Add   -> calc (+) (vr1, vr2) 
                | Subtr -> calc (-) (vr1, vr2) 
                | Expr  -> applyExpr vr1 vr2


        module Operators =

            let inline (^*) vr1 vr2 = (?<-) Mult vr1 vr2
        
            let inline (^/) vr1 vr2 = (?<-) Div vr1 vr2
        
            let inline (^+) vr1 vr2 = (?<-) Add vr1 vr2
        
            let inline (^-) vr1 vr2 = (?<-) Subtr vr1 vr2
        
            let inline (<==) vr1 vr2 = (?<-) Expr vr1 vr2



    open Informedica.Utils.Lib.BCL
    open ValueRange.Operators

    module Minimum = ValueRange.Minimum
    module Maximum = ValueRange.Maximum

    type ValueRangeException = ValueRange.Exceptions.ValueRangeException


    module Exceptions =

        exception VariableException of Exceptions.Message

        let raiseExc m = m |> VariableException |> raise


    /// Create a `Variable` and passes
    /// the result to **succ**
    let create succ n vs = { Name = n; Values = vs } |> succ


    /// Create a `Variable` and directly
    /// return the result.
    let createSucc = create id


    /// Helper create function to
    /// store the result of a `Variable`
    /// calculation before applying to
    /// the actual result `Variable`.
    let createRes = createSucc ("Result" |> Name.createExc)


    /// Apply **f** to `Variable` **var**.
    let apply f (var: Variable) = var |> f


    /// Helper function for type inference
    let get = apply id


    let toString exact ({ Name = n; Values = vs }: Variable) =
        vs
        |> ValueRange.toString exact
        |> sprintf "%s %s" (n |> Name.toString)


    /// Get the `Name` of a `Variable`.
    let getName v = (v |> get).Name


    /// Get the `ValueRange of a `Variable`.
    let getValueRange v = (v |> get).Values


    let contains v vr =
        vr
        |> getValueRange
        |> ValueRange.contains v


    /// Change `Name` to **n**.
    let setName n v : Variable = { v with Name = n }


    /// Apply a `ValueRange` **vr** to
    /// `Variable` **v**.
    let setValueRange v vr =
        try
            let vr' =
                (v |> get).Values <== vr

            { v with Values = vr' }

        with
        | :? ValueRangeException ->
            (v, vr)
            |> Exceptions.VariableCannotSetValueRange
            |> Exceptions.raiseExc


    /// Set the values to a `ValueRange`
    /// that prevents zero or negative values.
    let setNonZeroOrNegative v =
        let vr =
            (v |> get).Values
            |> ValueRange.setMin (BigRational.zero |> Minimum.createMin false)

        { v with Values = vr }


    /// Get the number of distinct values
    let count v = v |> getValueRange |> ValueRange.count


    /// Checks whether **v1** and **v2** have the
    /// same `Name`
    let eqName v1 v2 = v1 |> getName = (v2 |> getName)


    /// Checks whether a `Variable` **v** is solved,
    /// i.e. there is but one possible value left.
    let isSolved v =
        (v |> count = 1) &&
        (v |> getValueRange |> ValueRange.isValueSet)


    /// Checks whether a `Variable` is *solvable*
    /// i.e. can be further restricted to one value
    /// (or no values at all)
    let isSolvable = isSolved >> not


    /// Checks whether there are no restrictions to
    /// possible values a `Variable` can contain
    let isUnrestricted = getValueRange >> ValueRange.isUnrestricted


    /// Apply the operator **op** to **v1** and **v2**
    /// return an intermediate *result* `Variable`.
    let calc op (v1, v2) =

        (v1 |> getValueRange) |> op <| (v2 |> getValueRange) |> createRes


    /// Extend type with basic arrhythmic operations.
    type VariableCalc =
         | Mult 
         | Div
         | Add 
         | Subtr
         | Expr with



        static member inline (?<-) (op, v1, v2) = 
            match op with
            | Mult  -> calc (^*) (v1, v2) 
            | Div   -> calc (^/) (v1, v2) 
            | Add   -> calc (^+) (v1, v2) 
            | Subtr -> calc (^-) (v1, v2) 
            | Expr  -> 
                if v1 |> isSolvable then
                    v2
                    |> getValueRange
                    |> setValueRange v1
                else v1


    module Operators =

        let inline (^*) v1 v2 = (?<-) Mult v1 v2

        let inline (^/) v1 v2 = (?<-) Div v1 v2

        let inline (^+) v1 v2 = (?<-) Add v1 v2

        let inline (^-) v1 v2 = (?<-) Subtr v1 v2

        let inline (<==) v1 v2 = (?<-) Expr v1 v2


    /// Handle the creation of a `Variable` from a `Dto` and
    /// vice versa.
    module Dto =

        /// The `Dto` representation of a `Variable`
        type Dto =
            {
                Name: string
                Unr: bool
                Min: BigRational option
                MinIncl: bool
                Max: BigRational option
                MaxIncl: bool
                Vals: BigRational list
            }

        /// Create a `Dto`
        let createDto n unr min minincl max maxincl vals =  
            { Name = n; Unr = unr; Vals = vals; Min = min; MinIncl = minincl; Max = max; MaxIncl = maxincl }

        /// Create an *empty* *new* `Dto` with only a name **n**
        let createNew n = createDto n true None false None false []

        /// Apply `f` to an `Dto` `d`
        let apply f (d: Dto) = f d

        /// Apply an array of `vals` to an **dto**
        /// making sure the `Unr` is set to `false`.
        let setVals vals dto = { dto with Unr = false; Vals = vals }

        /// Set a `min` to an **dto** that is either inclusive `incl` true or exclusive `false`
        let setMin  min incl dto = { dto with Unr = false; Min = min; MinIncl = incl }

        /// Set a `max` to an **dto** that is either inclusive `incl` true or exclusive `false`
        let setMax  max incl dto = { dto with Unr = false; Max = max; MaxIncl = incl }

        /// Match a string **p** to a field of `Dto`
        let (|Vals|MinIncl|MinExcl|MaxIncl|MaxExcl|NoProp|) p =
            match p |> String.toLower with
            | "vals"     -> Vals
            | "minincl"  -> MinIncl
            | "minexcl"  -> MinExcl
            | "maxincl"  -> MaxIncl
            | "maxexcl"  -> MaxExcl
            | _          -> NoProp


        /// Set a `Dto` member **p** with a value `v` to a `Dto` **dto**.
        /// If no field can be matched the **dto** is returned unchanged.
        let setProp p vs dto =
            let getVal vs =
                match vs with
                | [v] -> v |> Some
                | _   -> None

            match p with
            | Vals     -> dto |> setVals vs
            | MinIncl  -> dto |> setMin  (vs |> getVal) true
            | MinExcl  -> dto |> setMin  (vs |> getVal) false
            | MaxIncl  -> dto |> setMax  (vs |> getVal) true
            | MaxExcl  -> dto |> setMax  (vs |> getVal) false
            | _   -> dto

        /// Return a `string` representation of a `Dto`
        let toString
                    exact
                    { Name = name
                      Unr = unr
                      Vals = vals
                      Min = min
                      MinIncl = minincl
                      Max = max
                      MaxIncl = maxincl } =

            let vals = ValueRange.print exact unr min minincl max maxincl vals 
            sprintf "%s%s" name vals


        /// Create a `Variable` from a `Dto` and
        /// raise a `DtoException` if this fails.
        let fromDto (dto: Dto) =
            let succ = id

            let n = dto.Name |> Name.create succ (fun m -> m |> Name.Exceptions.raiseExc)

            let vs =
                match dto.Vals with
                | [] -> None
                | _ ->
                    dto.Vals |> Set.ofList |> Some

            let min = dto.Min |> Option.bind (fun v -> v |> Minimum.createMin dto.MinIncl |> Some)
            let max = dto.Max |> Option.bind (fun v -> v |> Maximum.createMax dto.MaxIncl |> Some)

            let vr = ValueRange.create min max vs  

            create succ n vr


        /// Create a `Variable` option from a `Dto` and
        /// return `None` when this fails.
        let fromDtoOpt (dto: Dto) =
            let succ = Some
            let fail = Option.none

            let n = dto.Name |> Name.create succ (fun m -> m |> fail)

            let vs =
                match dto.Vals with
                | [] -> None
                | _ ->
                    dto.Vals |> Set.ofList |> Some

            let min = dto.Min |> Option.bind (fun v -> v |> Minimum.createMin dto.MinIncl |> Some)
            let max = dto.Max |> Option.bind (fun v -> v |> Maximum.createMax dto.MaxIncl |> Some)


            try
                let vr = ValueRange.create min max vs

                match n with
                | Some n' -> create succ n' vr
                | _ -> None
            with _ -> None


        /// Create a `Dto` from a `Variable`.
        let toDto (v: Variable) =

            let dto = createNew (let (Name.Name n) = v.Name in n)

            let unr = v.Values |> ValueRange.isUnrestricted

            let minincl =
                match v.Values |> ValueRange.getMin with
                | Some m -> m |> Minimum.isMinExcl |> not | None -> false

            let maxincl =
                match v.Values |> ValueRange.getMax with
                | Some m -> m |> Maximum.isMaxExcl |> not | None -> false

            let min  =
                v.Values
                |> ValueRange.getMin
                |> Option.bind (Minimum.minToBigRational >> Some)

            let max  =
                v.Values
                |> ValueRange.getMax
                |> Option.bind (Maximum.maxToBigRational >> Some)

            let vals =
                v.Values
                |> ValueRange.getValueSet
                |> function
                | Some vs -> vs |> Set.toList
                | None -> []

            { dto with
                Unr = unr
                Vals = vals
                Min = min
                MinIncl = minincl
                Max = max
                MaxIncl = maxincl }

