namespace Informedica.GenSolver.Lib

open System
open MathNet.Numerics
open Informedica.GenSolver.Utils


module Set =

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL

    let removeBigRationalMultiples xs =
        xs
        |> Set.fold (fun acc x1 ->
            acc 
            |> Set.filter (fun x2 ->
                x1 = x2 ||
                x2 |> BigRational.isMultiple x1 |> not
            )
        ) xs



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
/// * <..[1N]..> : meaning that the variable must be a number that is a multiple of 1N
/// * <..[2N,3N]..> : meaning that the variable must be either a multiple of 2N or 3N
/// * [1N,3N,4N,5N] : meaning that the variable can only be one of the numbers
///
/// * `Name` : A `Variable` is identified by its `Name`
/// * `Range` : Defines a range by a `Minimum` and/or `Maximum` and/or `Increment` 
/// * `ValueRange` : Is either a `Unrestricted` or a `Range` or a set of values.
module Variable =

    open Types

    module Name =

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
    ///
    /// * `Minimum`
    /// * `Maximum`
    /// * `Range`
    /// * `ValueRange`
    module ValueRange =

        open Informedica.Utils.Lib.BCL


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


        module Increment =


            module Exceptions =

                exception IncrementException of Exceptions.Message

                let raiseExc m =
                    m
                    |>IncrementException
                    |> raise


            /// Apply **f** to the bigrational
            /// values of `Increment`
            let applyIncr f = function
            | Increment(i) ->
                i
                |> f
                |> function
                | r when r |> Set.isEmpty ->
                    r
                    |> Exceptions.IncrementZeroNegativeOrEmpty
                    |> Exceptions.raiseExc
                | r -> r |>Increment

            /// Convert an `Increment` to a `BigRational`
            let incrToBigRationalSet (Increment i) = i

            /// Create an `Increment` that is a set of multiples
            /// that cannot be zero or negative values.
            let createIncr i =
                if i |> Set.exists ((>) 0N) || i |> Set.isEmpty then
                    i
                    |> Exceptions.IncrementZeroNegativeOrEmpty
                    |> Exceptions.raiseExc
                else
                    i
                    |> Set.removeBigRationalMultiples
                    |> fun xs ->
                        if xs |> Set.isEmpty then 
                            Set.empty 
                            |> Exceptions.IncrementZeroNegativeOrEmpty 
                            |> Exceptions.raiseExc
                        else
                            xs |> Increment


        module Exceptions =


            exception ValueRangeException of Exceptions.Message


            let raiseExc m = m |> ValueRangeException |> raise


            let raiseMinLargerThanMax min max =
                (min, max)
                |> Exceptions.ValueRangeMinLargerThanMax
                |> raiseExc



        /// Create a `ValueSet` from a set of `BigRational`.
        let createValueSet s =
            let s = s |> Set.ofSeq
            if s |> Set.isEmpty then
                Exceptions.ValueRangeEmptyValueSet
                |>Exceptions.raiseExc

            else
                s |> ValueSet


        /// Aply the give functions to `Values`
        /// where **unr** is used for an `Unrestricted`
        /// `ValueRange`, **fv** is used for `ValueSet` and
        /// **fr** is used for `Range`
        let apply unr fValueSet fRange = function
            | Unrestricted        -> unr
            | ValueSet vs         -> vs |> fValueSet
            | Range r             -> r  |> fRange


        /// Aply the give functions to `Range`
        /// where **fMin** is used for a range with only
        /// a `Minimum`, **fMax** is used for range `Maximum`,
        /// **fMinIncr** is used for `MinIncr` , **fIncrMax**
        /// for `IncrMax` and **fMinMax** for `MinMax`.
        /// Finally, `fMinIncrMax` is applied to a `Minimum`, `Increment` and `Maximum`.
        let applyRange fMin fMax fMinIncr fIncrMax fMinMax fMinIncrMax = function
            | Min m               -> m |> fMin
            | Max m               -> m |> fMax
            | MinIncr (min, incr) -> (min, incr) |> fMinIncr
            | IncrMax (incr, max) -> (incr, max) |> fIncrMax
            | MinMax (min, max)   -> (min, max)  |> fMinMax
            | MinIncrMax (min, incr, max) -> (min, incr, max) |> fMinIncrMax


        /// Count the number of values in a `ValueRange`.
        /// Returns 0 if not a `ValueSet`.
        let count =
            let zero _ = 0
            apply 0 (fun s -> s |> Set.count) zero


        /// Checks whether a `ValueRange` is `Unrestricted`
        let isUnrestricted =
            let false' _ = false
            apply true false' false'


        /// Checks whether a `ValueRange` is a `ValueSet`
        let isValueSet =
            let false' _ = false
            apply false (fun _ -> true) false'


        /// Checks whether a `ValueRange` is a `Range`
        let isRange =
            let false' _ = false
            apply false false' (fun _ -> true)


        /// Checks whether a `BigRational` is between an optional **min**
        /// an optional **max** and is a multiple of **incr**.
        /// When min, incr and max are `None` then this will
        /// return `true` for any value. I.e. this will check if a `BigRational` is a 
        /// member of a `Range`.
        let isBetweenAndMultOf min incr max v =
            let fTrue = fun _ -> true

            let fMin  = function
            | None -> fTrue
            | Some(Minimum.MinIncl m) -> (<=) m
            | Some(Minimum.MinExcl m) -> (<) m

            let fMax  = function
            | None -> fTrue
            | Some(Maximum.MaxIncl m) -> (>=) m
            | Some(Maximum.MaxExcl m) -> (>) m

            let fIncr i v =
                match i with
                | None              -> true
                | Some(Increment.Increment i) ->
                    i
                    |> Set.exists (fun i ->
                        v
                        |> BigRational.isMultiple i
                    )

            v |> fIncr incr &&
            v |> fMin min &&
            v |> fMax max


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
        /// to **min**, **incr** and **max** constraints
        let filter min incr max = Set.filter (isBetweenAndMultOf min incr max)


        /// Calculate `Minimum` as a multiple of `Increment` **incr**.
        /// This assumes that the smallest increment will calculate the smallest 
        /// minimum as a multiple of that increment.
        let minMultipleOf incr min =
            let n  = min  |> Minimum.minToBigRational
            let d  = incr |> Increment.incrToBigRationalSet |> Set.minElement
            let n' = n    |> BigRational.toMinMultipleOf d
            
            if min |> Minimum.isMinExcl && n' <= n then n' + d else n'

        /// Calculate the smallest possible `Minimum` that is a multiple of 
        /// an increment in the set of `Increment`.
        let minMultipleOf2 incr min =
            let m = min |> Minimum.minToBigRational

            incr
            |> Increment.incrToBigRationalSet
            |> Set.fold (fun acc i ->
                let m' = m |> BigRational.toMinMultipleOf i
                let m' = 
                    if min |> Minimum.isMinExcl && m' <= m then m' + i else m'

                match acc with
                | Some m'' -> if m' < m'' then Some m' else acc
                | None     ->  Some m'
            
            ) (None)


        // Calculate `Maximum` **max** as a multiple of **incr**.
        let maxMultipleOf incr max =
            let n  = max  |> Maximum.maxToBigRational
            let d  = incr |> Increment.incrToBigRationalSet |> Set.minElement
            let n' = n    |> BigRational.toMaxMultipleOf d

            if max |> Maximum.isMaxExcl && n' >= n then n' - d else n'


        /// Calculate the largest possible `Maximum` that is a multiple of 
        /// an increment in the set of `Increment`.
        let maxMultipleOf2 incr max =
            let m = max |> Maximum.maxToBigRational

            incr
            |> Increment.incrToBigRationalSet
            |> Set.fold (fun acc i ->
                let m' = m |> BigRational.toMaxMultipleOf i
                let m' = 
                    if max |> Maximum.isMaxExcl && m' >= m then m' - i else m'

                match acc with
                | Some m'' -> if m' > m'' then Some m' else acc
                | None     -> Some m'
                    
            ) (None)


        /// Create a set of `BigRational` using **min**, **incr** and a **max**.
        let minIncrMaxToValueSet min incr max =
            let min' =  min  |> minMultipleOf incr
            let max' =  max  |> maxMultipleOf incr
            let incr' = incr |> Increment.incrToBigRationalSet

            [
                for i in incr' do
                    [ min'..i..max' ]
            ]
            |> List.collect id
            |> Set.ofList
            |> createValueSet


        /// Create a string (to print) representation of a `ValueRange`. 
        /// `Exact` true prints exact bigrationals, when false
        /// print as floating numbers
        let print exact unr vals min minincl incr max maxincl =

            let printVals vals =
                let vals =
                    vals
                    |> List.sort
                    |> List.map (if exact then BigRational.toString
                                 else BigRational.toFloat >> sprintf "%A")

                "[" + (vals |> List.fold (fun s v -> if s = "" then v else s + ", " + v) "") + "]"

            let printRange min incr max =
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

                    match min, incr, max with
                    | Some min, _, None     when incr |> List.isEmpty ->
                        sprintf "%s%s..>" left (min |> brToStr)
                    | Some min, _, Some max when incr |> List.isEmpty ->
                        sprintf "%s%s..%s%s" left (min |> brToStr) (max |> brToStr) right
                    | None,     _, Some max when incr |> List.isEmpty ->
                        sprintf "<..%s%s" (max |> brToStr) right
                    | Some min, incr, None     ->
                        sprintf "%s%s..%s..>" left (min |> brToStr) (incr |> printVals)
                    | None,     incr, Some max ->
                        sprintf "<..%s..%s%s" (incr |> printVals) (max |> brToStr) right
                    | Some min, incr, Some max ->
                        sprintf "%s%s..%s..%s%s" left (min |> brToStr) (incr |> printVals) (max |> brToStr) right
                    | _ -> "[]"

            let vals =
                if vals |> List.isEmpty |> not then vals |> printVals
                else
                    printRange min incr max

            sprintf "%s" vals


        /// Convert a `ValueRange` to a `string`.
        let toString exact vr =
            let fVs vs =
                print exact false (vs |> Set.toList) None false [] None false

            let fRange =
                let print min minincl incr max maxincl = print exact false [] min minincl incr max maxincl

                let fMin min =
                    let incl, min = min |> Minimum.minToBoolBigRational
                    print (Some min) incl [] None false

                let fMax max =
                    let incl, max = max |> Maximum.maxToBoolBigRational

                    print None false [] (Some max) incl

                let fMinIncr (min, incr)  =
                    let incl, min = min |> Minimum.minToBoolBigRational
                    let incr = incr |> Increment.incrToBigRationalSet |> Set.toList

                    print (Some min) incl incr None false

                let fIncrMax (incr, max)  =
                    let incl, max = max |> Maximum.maxToBoolBigRational
                    let incr = incr |> Increment.incrToBigRationalSet |> Set.toList

                    print None false incr (Some max) incl

                let fMinMax (min, max) =
                    let maxincl, min = min |> Minimum.minToBoolBigRational
                    let minincl, max = max |> Maximum.maxToBoolBigRational

                    print (Some min) minincl [] (Some max) maxincl

                let fMinIncrMax (min, incr, max) =
                    let maxincl, min = min |> Minimum.minToBoolBigRational
                    let minincl, max = max |> Maximum.maxToBoolBigRational
                    let incr = incr |> Increment.incrToBigRationalSet |> Set.toList

                    print (Some min) minincl incr (Some max) maxincl

                applyRange fMin fMax fMinIncr fIncrMax fMinMax fMinIncrMax

            let unr = print exact true [] None false [] None false

            vr |> apply unr fVs fRange


        /// An `Unrestricted` `ValueRange`.
        let unrestricted = Unrestricted


        /// Create a `Range` with a `Minimum`, `Increment` and a `Maximum`.
        let createMinIncrRange vs =
            let incr =
                vs
                |> Increment.createIncr

            let min =
                incr
                |> Increment.incrToBigRationalSet
                |> Set.minElement
                |> Minimum.createMin true

            (min, incr)
            |> MinIncr

        
        /// Create a `Minimum` `Range` that is
        /// either inclusive or exclusive.
        let createMinRange isIncl m = m |> Minimum.createMin isIncl |> Min

        
        /// Create a `Maximum` `Range` that is
        /// either inclusive or exclusive.
        let createMaxRange isIncl m = m |> Maximum.createMax isIncl |> Max


        /// Create a `MinIncr` `ValueRange`.
        let minIncrToValueRange min incr =
            let min' = min |> minMultipleOf incr |> Minimum.MinIncl
            (min', incr) |> MinIncr |> Range


        /// Create an `IncrMax` `ValueRange`.
        let incrMaxToValueRange incr max =
            let max' = max |> maxMultipleOf incr |> Maximum.MaxIncl
            (incr, max') |> IncrMax |> Range


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
                |> createValueSet

            else (min, max) |> MinMax |> Range

        /// Create a `MinIncrMax` `ValueRange`. If **min** > **max** raises 
        /// an `MinLargetThanMax` exception. If min equals max, a `ValueSet` with 
        /// value min (=max).
        let minIncrMaxToValueRange min incr max =
            let min' = min |> minMultipleOf incr |> Minimum.createMin true
            let max' = max |> maxMultipleOf incr |> Maximum.createMax true

            if min' |> minLTmax max' then 
                Exceptions.raiseMinLargerThanMax min' max'

            elif min |> minEQmax max then
                min 
                |> Minimum.minToBigRational
                |> Set.singleton
                |> createValueSet

            else
                (min', incr, max') |> MinIncrMax |> Range


        /// Create a `ValueRange` using a `ValueSet` **vs**
        /// an optional `Minimum` **min**, **incr** and `Maximum` **max**.
        /// If both **min**, **incr** and **max** are `None` an `Unrestricted` 
        /// `ValueRange` is created. 
        let create vs min incr max =
            match vs with
            | None ->
                match min, incr, max with
                | None,      None,       None      -> unrestricted
                | Some min', None,       None      -> min' |> Min |> Range
                | None,      None,       Some max' -> max' |> Max |> Range
                | Some min', None,       Some max' -> minMaxToValueRange  min'  max'
                | Some min', Some incr', None      -> minIncrToValueRange min'  incr'
                | None,      Some incr', Some max' -> incrMaxToValueRange incr' max'
                | None,      Some incr', None      ->
                    incr'
                    |> Increment.incrToBigRationalSet
                    |> Set.minElement
                    |> Minimum.createMin true
                    |> fun min -> minIncrToValueRange min incr'

                | Some min', Some incr', Some max' ->
                    minIncrMaxToValueRange min' incr' max'

            | Some vs ->
                vs
                |> filter min incr max
                |> createValueSet


        /// Get a set of `BigRational` from a `ValueRange`,
        /// returns an `None` when `ValueRange` is not
        /// a `ValueSet` 
        let getValueSet = apply None Some (fun _ -> None)


        /// Get a `Minimum` option in a `Range`
        let getRangeMin =
            applyRange Some 
                       Option.none 
                       (fst >> Some) 
                       Option.none 
                       (fst >> Some) 
                       (fun (min, _, _) -> min |> Some)


        /// Get a `Maximum` option in a `Range`
        let getRangeMax =
            applyRange  Option.none 
                        Some 
                        Option.none 
                        (snd >> Some) 
                        (snd >> Some) 
                        (fun (_, _, max) -> max |> Some)


        /// Get an optional `Minimum` in a `ValueRange`
        let getMin = apply None Minimum.getSetMin getRangeMin


        // Get an optional `Increment` in a `ValueRange`
        let getIncr =
            let fRange =
                applyRange Option.none
                           Option.none
                           (snd >> Some)
                           (fst >> Some)
                           Option.none
                           (fun (_, incr, _) -> incr |> Some)

            apply None Option.none fRange

        
        /// Get an optional `Maximum` in a `ValueRange`
        let getMax = apply None Maximum.getSetMax getRangeMax

        
        /// Check whether a `ValueRange` **vr** contains
        /// a `BigRational` **v**.
        let contains v vr =
            match vr with
            | ValueSet vs -> vs |> Set.contains v
            | _ ->
                let min = vr |> getMin
                let max = vr |> getMax

                let incr = vr |> getIncr
                v |> isBetweenAndMultOf min incr max


        /// Apply a `Minimum` **min** to a `ValueRange` **vr**.
        /// If minimum cannot be set the original `Minimum` is returned.
        /// So, it always returns a more restrictive, i.e. larger, or equal `Minimum`.
        let setMin min vr =
            // Check whether the new min is more restrictive than the old min
            let checkMin f min' = if min |> Minimum.minLTmin min' then min |> f else vr

            let fValueSet =
                let max = vr  |> getMax
                let incr = vr |> getIncr
                filter (Some min) incr max >> createValueSet

            let fRange =
                let fMax max  = minMaxToValueRange min max
                let fMin min' = min' |> checkMin (Min >> Range)
                let fMinIncr (min', incr) = 
                    min' 
                    |> checkMin (fun m -> minIncrToValueRange m incr)
                let fIncrMax (incr, max) = create None (Some min) (Some incr) (Some max)
                let fMinMax (min', max)  = 
                    min' 
                    |> checkMin (fun min -> minMaxToValueRange min max)
                let fMinIncrMax (min', incr, max) =
                    min' |> checkMin (fun m -> minIncrMaxToValueRange m incr max)

                applyRange fMin fMax fMinIncr fIncrMax fMinMax fMinIncrMax

            vr |> apply (Min min |> Range) fValueSet fRange


        /// Apply a `Maximum` **max** to a `ValueRange` **vr**.
        /// If maximum cannot be set the original is returned.
        /// So, it always returns a more restrictive, i.e. smaller, or equal `Maximum`.
        let setMax max vr =
            // Check whether the new max is more restrictive than the old max
            let checkMax f max' = if max' |> Maximum.maxLTmax max then max |> f else vr

            let fValueSet =
                let min = vr |> getMin
                let incr = vr |> getIncr
                filter min incr (Some max) >> createValueSet

            let fRange =
                let fMin min = minMaxToValueRange min max
                let fMax max' = max' |> checkMax (Max >> Range)
                let fMinMax (min, max') = 
                    max' 
                    |> checkMax (fun max -> minMaxToValueRange min max)
                let fIncrMax (incr, max') = 
                    max' 
                    |> checkMax (fun max -> incrMaxToValueRange incr max)
                let fMinIncr (min, incr) = create None (Some min) (Some incr) (Some max)
                let fMinIncrMax (min, incr, max') =
                    max' |> checkMax (fun m -> minIncrMaxToValueRange min incr m)

                applyRange fMin fMax fMinIncr fIncrMax fMinMax fMinIncrMax

            vr |> apply (Max max |> Range) fValueSet fRange


        /// Apply a **incr** to a `ValueRange` **vr**.
        /// If increment cannot be set the original is returned.
        /// So, the resulting increment is always more restrictive as the previous one
        let setIncr incr vr =
            let cr = create None

            // Check whether the new incr is more restrictive than the old incr
            // ToDo needs testing!!
            let checkIncr f incr' =
                if incr
                   |> Increment.incrToBigRationalSet
                   |> Set.forall (fun i ->
                        incr'
                        |> Increment.incrToBigRationalSet
                        |> Set.exists (fun i' -> i |> BigRational.isMultiple i')
                    ) then
                    incr |> f

                else vr

            let unr =
                minIncrToValueRange
                    (Minimum.createMin true (incr |> Increment.incrToBigRationalSet 
                                                  |> Set.minElement))
                    incr

            let fValueSet =
                let min = vr |> getMin
                let max = vr |> getMax
                filter min (Some incr) max >> createValueSet

            let fRange =
                let fMin min = minIncrToValueRange min incr
                let fMax max = cr None (Some incr) (Some max)
                let fMinMax (min, max) = cr (Some min) (Some incr) (Some max)
                let fMinIncr (min, incr') = 
                    incr' 
                    |> checkIncr (fun i -> minIncrToValueRange min i)
                let fIncrMax (incr', max) = 
                    incr' 
                    |> checkIncr (fun i -> incrMaxToValueRange i max)
                let fMinIncrMax (min, incr', max) =
                    incr' |> checkIncr (fun i -> minIncrMaxToValueRange min i max)

                applyRange fMin fMax fMinIncr fIncrMax fMinMax fMinIncrMax

            vr |> apply unr fValueSet fRange


        /// Appy a set of `BigRational` to a `ValueRange` **vr**.
        /// the result is a filtered or the intersect of
        /// the set of `BigRational` and **vr**.
        let setValues vs vr =

            let vs1, min, incr, max =
                vr |> getValueSet,
                vr |> getMin,
                vr |> getIncr,
                vr |> getMax

            let vs2 =
                match vs1 with
                | None ->
                    vs
                    |> filter min incr max
                | Some vs1 ->
                    vs
                    |> filter min incr max
                    |> Set.intersect vs1

            create (Some vs2) min incr max


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
                match x1, x2 with
                | Some (v1), Some (v2) ->
                    if op |> BigRational.opIsDiv && v2 = 0N then None
                    else v1 |> op <| v2 |> c (incl1 && incl2) |> Some
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


        /// Safely calculate **v1** and **v2** using operator **op**,
        /// returns None if operator is division and **v2** is 0.
        let calcOpt op c v1 v2 =
            match op with
            | BigRational.Mult
            | BigRational.Subtr
            | BigRational.Add  -> v1 |> op <| v2 |> c |> Some
            // prevent division by zero
            | BigRational.Div  ->
                if v2 <> BigRational.zero then
                    (v1 |> op <| v2) |> c |> Some
                else None
            | BigRational.NoMatch ->
                Exceptions.ValueRangeNotAValidOperator
                |> Exceptions.raiseExc


        /// Calculate an increment with
        /// **incr1** of x1 and **incr2** of x2
        /// in an equation: y = x1 **op** x2
        let calcIncr op incr1 incr2 =
            match incr1, incr2 with
            | Some (Increment.Increment i1), Some (Increment.Increment i2) ->
                match op with
                // y.incr = x1.incr * x2.incr
                | BigRational.Mult ->
                    [
                        for x in i1 do
                            for y in i2 do
                                x * y
                    ]
                    |> Set.ofList
                    |> Increment.createIncr
                    |> Some

                // when y = x1 + x2 then y.incr = gcd of x1.incr and x2.incr
                | BigRational.Add | BigRational.Subtr ->
                    [
                        for x in i1 do
                            for y in i2 do
                                BigRational.gcd x y
                    ]
                    |> Set.ofList
                    |> Increment.createIncr
                    |> Some

                |  _ -> None

            | _ -> None


        /// Applies an infix operator **op**
        /// to `ValueRange` **x1** and **x2**.
        /// Calculates `Minimum`, increment or `Maximum`
        /// if either **x1** or **x2** is not a `ValueSet`.
        /// Doesn't perform any calculation when both
        /// **x1** and **x2** are `Unrestricted`.
        let calc op (x1, x2) =
            let calcOpt = calcOpt op
            let toVS vr =
                match vr with
                | Range(MinIncrMax (min, inr, max)) -> minIncrMaxToValueSet min inr max
                | _ -> vr
            // first check if range can be changed to valueset
            let x1 = x1 |> toVS
            let x2 = x2 |> toVS

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
                    |> Set.ofSeq
                    |> createValueSet

            // A set with an increment results in a new set of increment
            | ValueSet s, Range(MinIncr(_, i))
            | Range(MinIncr(_, i)), ValueSet s

            | ValueSet s, Range(IncrMax(i, _))
            | Range(IncrMax(i, _)), ValueSet s

            | ValueSet s, Range(MinIncr(_, i))
            | Range(IncrMax(i, _)), ValueSet s

            | ValueSet s, Range(MinIncr(_, i))
            | Range(IncrMax(i, _)), ValueSet s ->

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

                let incr1 = i |> Some
                let incr2 = s |> Increment.createIncr |> Some

                let incr = calcIncr op incr1 incr2

                match min, incr, max with
                | None, None, None -> unrestricted
                | _ -> create None min incr max

            // In any other case calculate min, incr and max
            | _ ->
                let min1, incr1, max1 = x1 |> getMin, x1 |> getIncr, x1 |> getMax
                let min2, incr2, max2 = x2 |> getMin, x2 |> getIncr, x2 |> getMax

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

                let incr = calcIncr op incr1 incr2

                match min, incr, max with
                | None, None, None -> unrestricted
                | _ -> create None min incr max


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
                | None -> vr

            match expr with
            | Unrestricted -> y
            | ValueSet vs  ->
                if vs |> Set.isEmpty then
                    Exceptions.ValueRangeEmptyValueSet
                    |> Exceptions.raiseExc

                else y |> setValues vs
            | _ ->
                y
                |> set getMin  setMin
                |> set getIncr setIncr
                |> set getMax  setMax


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
    module Increment = ValueRange.Increment

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


    let toString exact { Name = n; Values = vs } =
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

        let hello x = x * 2

    /// Handle the creation of a `Variable` from a `Dto` and
    /// vice versa.
    module Dto =

        /// The `Dto` representation of a `Variable`
        type Dto =
            {
                Name: string
                Unr: bool
                Vals: BigRational list
                Min: BigRational option
                MinIncl: bool
                Incr: BigRational list
                Max: BigRational option
                MaxIncl: bool
            }

        /// Create a `Dto`
        let createDto n unr vals min minincl incr max maxincl =  { Name = n; Unr = unr; Vals = vals; Min = min; MinIncl = minincl; Incr = incr; Max = max; MaxIncl = maxincl }

        /// Create an *empty* *new* `Dto` with only a name **n**
        let createNew n = createDto n true [] None false [] None false

        /// Apply `f` to an `Dto` `d`
        let apply f (d: Dto) = f d

        /// Apply an array of `vals` to an **dto**
        /// making sure the `Unr` is set to `false`.
        let setVals vals dto = { dto with Unr = false; Vals = vals }

        /// Set a `min` to an **dto** that is either inclusive `incl` true or exclusive `false`
        let setMin  min incl dto = { dto with Unr = false; Min = min; MinIncl = incl }

        /// Set a `max` to an **dto** that is either inclusive `incl` true or exclusive `false`
        let setMax  max incl dto = { dto with Unr = false; Max = max; MaxIncl = incl }

        /// Set an `incr` to a **dto**
        let setIncr incr dto = { dto with Unr = false; Incr = incr }

        /// Match a string **p** to a field of `Dto`
        let (|Vals|MinIncl|MinExcl|Incr|MaxIncl|MaxExcl|NoProp|) p =
            match p |> String.toLower with
            | "vals"     -> Vals
            | "minincl"  -> MinIncl
            | "minexcl"  -> MinExcl
            | "incr"     -> Incr
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
            | Incr     -> dto |> setIncr vs
            | MaxIncl  -> dto |> setMax  (vs |> getVal) true
            | MaxExcl  -> dto |> setMax  (vs |> getVal) false
            | NoProp   -> dto

        /// Return a `string` representation of a `Dto`
        let toString
                    exact
                    { Name = name
                      Unr = unr
                      Vals = vals
                      Min = min
                      MinIncl = minincl
                      Incr = incr
                      Max = max
                      MaxIncl = maxincl } =

            let vals = ValueRange.print exact unr vals min minincl incr max maxincl
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
            let incr =
                dto.Incr
                |> function
                | [] -> None
                | _ ->
                    dto.Incr
                    |> Set.ofList
                    |> Increment.createIncr
                    |> Some

            let vr = ValueRange.create vs min incr max

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

            let incr =
                match dto.Incr with 
                | [] -> None
                | _ -> 
                    dto.Incr
                    |> Set.ofList
                    |> Increment.createIncr
                    |> Some

            try
                let vr = ValueRange.create vs min incr max

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

            let incr =
                v.Values
                |> ValueRange.getIncr
                |> function
                | Some i -> i |> Increment.incrToBigRationalSet |> Set.toList
                | None   -> []

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
                Incr = incr
                Max = max
                MaxIncl = maxincl }