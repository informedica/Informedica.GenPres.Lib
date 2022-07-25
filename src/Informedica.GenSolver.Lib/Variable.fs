namespace Informedica.GenSolver.Lib


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
        let create succ fail s =
            let s = s |> String.trim
            if s |> String.IsNullOrWhiteSpace then
                Exceptions.NullOrWhiteSpaceException
                |> fail
            else
                if s |> String.length <= 1000 then s |> Name |> succ
                else
                    s
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

            /// Create a `Minimum` that is
            /// either inclusive or exclusive.
            let create isIncl br = if isIncl then br |> MinIncl else br |> MinExcl

            /// Apply **f** to the bigrational
            /// value of `Minimum`
            let apply fincl fexcl = function
                | MinIncl(m) -> m |> fincl
                | MinExcl(m) -> m |> fexcl


            let map fIncl fExcl =
                apply
                    (fIncl >> (create true))
                    (fExcl >> (create false))

            /// Checks whether `Minimum` **m2** > **m1**
            /// Note that the fact that a Minimum is inclusive or exclusive 
            /// must be taken into account.
            let minGTmin min1 min2 =
                match min2, min1 with
                | MinIncl m2, MinIncl m1
                | MinExcl m2, MinExcl m1
                | MinIncl m2, MinExcl m1 -> m2 > m1
                | MinExcl m2, MinIncl m1 -> m2 >= m1

            /// Checks whether `Minimum` **m2** <= **m1**
            let minSTEmin m1 m2 = m2 |> minGTmin m1 |> not


            let minGTEmin min1 min2 = min1 = min2 || minGTmin min1 min2


            let minSTmin min1 min2 = min2 |> minGTEmin min1 |> not

            /// Checks whether `Minimum` is exclusive.
            let isExcl = function | MinIncl _ -> false | MinExcl _ -> true

            /// Checks whether `Minimum` is inclusive.
            let isIncl = isExcl >> not

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
            let toBigRational = function | MinIncl v | MinExcl v -> v

            /// Convert a `Minimum` to a `BigRational` and a `bool`
            /// that signifies inclusive or exclusive
            let toBoolBigRational =
                apply (fun br -> true, br) (fun br -> false, br)


            let multipleOf incr min =
                match min |> toBoolBigRational with
                | true, br -> br |> BigRational.minInclMultipleOf incr
                | false, br -> br |> BigRational.minExclMultipleOf incr
                |> fun (b, br) -> create b br

                
            let restrict newMin oldMin =
                if newMin |> minGTmin oldMin then
                    newMin
                else
                    oldMin


            let toString exact min =
                let toStr =
                    if exact then BigRational.toString
                    else
                        (BigRational.fixPrecision 3) >> string
                let b, br =
                    min |> toBoolBigRational
                $"""{if b then "[" else "<"}{br |> toStr}"""


        module Maximum =


            /// Create a `Maximum` that is
            /// either inclusive or exclusive.
            let create isIncl m = if isIncl then m |> MaxIncl else m |> MaxExcl

            /// Apply **f** to the bigrational
            /// value of `Maximum`
            let apply fIncl fExcl = function
            | MaxIncl(m) -> m |> fIncl
            | MaxExcl(m) -> m |> fExcl


            let map fIncl fExcl =
                apply
                    (fIncl >> (create true))
                    (fExcl >> (create false))


            /// Checks whether `Maximum` **m2** > **m1**
            /// Note that the fact that a maximum is inclusive or exclusive
            /// must be taken into account.
            let maxGTmax max1 max2 =
                match max2, max1 with
                | MaxIncl m2, MaxIncl m1
                | MaxExcl m2, MaxExcl m1
                | MaxExcl m2, MaxIncl m1 -> m2 > m1
                | MaxIncl m2, MaxExcl m1 -> m2 >= m1

            /// Checks whether `Maximum` **m2** <= **m1**
            let maxSTEmax m1 m2 = m2 |> maxGTmax m1 |> not


            let maxGTEmax max1 max2 = max1 = max2 || maxGTmax max1 max2


            let maxSTmax max1 max2 = max2 |> maxGTEmax max1 |> not

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
            let toBigRational = function | MaxIncl v | MaxExcl v -> v

            /// Checks whether `Maximum` is exclusive.
            let isExcl = function | MaxIncl _ -> false | MaxExcl _ -> true

            /// Checks whether `Maximum` is inclusive.
            let isIncl = isExcl >> not

            /// Turn a `Maximum` into a `BigRational` and a `bool` to indicate
            /// inclusive or exclusive.
            let toBoolBigRational =
                apply (fun m -> true, m) (fun m -> false, m)


            let multipleOf incr max =
                match max |> toBoolBigRational with
                | true, br -> br |> BigRational.maxInclMultipleOf incr
                | false, br -> br |> BigRational.maxExclMultipleOf incr
                |> fun (b, br) -> create b br


            let restrict newMax oldMax =
                if newMax |> maxSTmax oldMax then
                    newMax
                else
                    oldMax


            let toString exact max =
                let toStr =
                    if exact then BigRational.toString
                    else
                        (BigRational.fixPrecision 3) >> string
                let b, br =
                    max |> toBoolBigRational
                $"""{br |> toStr}{if b then "]" else ">"}"""


        module Increment =


            let create brs =
                brs
                |> Set.filter ((<) 0N)
                |> Set.removeBigRationalMultiples
                |> fun brs ->
                    if brs |> Set.isEmpty |> not then brs |> Increment
                    else
                        Exceptions.ValueRangeEmptyIncrement
                        |> Exceptions.raiseExc


            let map f (Increment incr) = incr |> Set.map f |> create


            let intersect (Increment incr1) (Increment incr2) =
                incr1 |> Set.intersect incr2 |> create


            let calc op incr1 incr2 =
                match op with
                // y.incr = x1.incr * x2.incr
                | BigRational.Mult ->
                    Seq.allPairs incr1 incr2
                    |> Seq.map (fun (x1, x2) -> x1 |> op <| x2)
                    |> Set.ofSeq
                    |> create
                    |> Some

                // when y = x1 + x2 then y.incr = x1.incr and x2.incr
                | BigRational.Add | BigRational.Subtr ->
                    Seq.append incr1 incr2
                    |> Set.ofSeq
                    |> create
                    |> Some
                // incr cannot be calculated based on division
                |  _ -> None

            /// Calculate an increment with
            /// **incr1** of x1 and **incr2** of x2
            /// in an equation: y = x1 **op** x2
            let calcOpt op incr1 incr2 =
                match incr1, incr2 with
                | Some (Increment i1), Some (Increment i2) ->
                    calc op i1 i2
                | _ -> None


            let toList (Increment incr) = incr |> Set.toList


            let isEmpty (Increment incr) = incr |> Set.isEmpty


            let count (Increment incr) = incr |> Set.count


            let restrict newIncr oldIncr =
                match oldIncr |> intersect newIncr with
                | i when i |> isEmpty -> oldIncr
                | i -> i


            let toString exact (Increment incr) =
                let toStr =
                    if exact then BigRational.toString
                    else
                        (BigRational.fixPrecision 3) >> string
                $"""{incr |> Set.map toStr |> String.concat ", "}"""


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


            let toSet (ValueSet vs) = vs


            let map f (ValueSet vs) = vs |> Set.map f |> create


            let getMin (ValueSet vs) = vs |> Minimum.getSetMin


            let getMax (ValueSet vs) = vs |> Maximum.getSetMax


            let count (ValueSet vs) = vs |> Set.count


            let isEmpty (ValueSet vs) = vs |> Set.isEmpty


            let contains v (ValueSet vs) = vs |> Set.contains v


            let intersect (ValueSet vs1) (ValueSet vs2) =
                vs1 |> Set.intersect vs2
                |> create


            let isSubset (ValueSet s1) (ValueSet s2) = Set.isSubset s1 s2


            let calc op (ValueSet s1) (ValueSet s2) =
                // When one of the sets does not contain any value then the result of
                // of the calculation cannot contain any value either
                if s1 |> Set.isEmpty || s2 |> Set.isEmpty then
                    Exceptions.ValueRangeEmptyValueSet
                    |> Exceptions.raiseExc
                // make sure the calculation doesn't take too long
                if (s1 |> Set.count) + (s2 |> Set.count) > Constants.MAX_CALC_COUNT then
                    (s1 |> Set.count) + (s2 |> Set.count)
                    |> Exceptions.ValueRangeTooManyValues
                    |> Exceptions.raiseExc

                else
                    Seq.allPairs s1 s2
                    |> Seq.map (fun (x1, x2) -> x1 |> op <| x2)
                    |> create


            let toString exact (ValueSet vs) =
                let toStr =
                    if exact then BigRational.toString
                    else
                        (BigRational.fixPrecision 3) >> string
                $"""[{vs |> Set.map toStr |> String.concat ", "}]"""


        module Property =


            let createMinProp b v = v |> Minimum.create b |> MinProp
            let createMinInclProp = createMinProp true
            let createMinExclProp = createMinProp false
            let createMaxProp b v = v |> Maximum.create b |> MaxProp
            let createMaxInclProp = createMaxProp true
            let createMaxExclProp = createMaxProp false
            let createIncrProp vs = vs |> Increment.create |> IncrProp
            let createValsProp vs = vs |> ValueSet.create |> ValsProp


            let mapValue f = function
                | MinProp min -> min |> Minimum.map f f |> MinProp
                | MaxProp max -> max |> Maximum.map f f |> MaxProp
                | IncrProp incr -> incr |> Increment.map f |> IncrProp
                | ValsProp vs -> vs |> ValueSet.map f |> ValsProp


            let toValueRange p =

                match p with
                | MinProp min -> min |> Min
                | MaxProp max -> max |> Max
                | IncrProp incr -> incr |> Incr
                | ValsProp vs -> vs |> ValSet


            let getMin = function
            | MinProp min -> min |> Some
            | _ -> None


            let getMax = function
            | MaxProp max -> max |> Some
            | _ -> None


            let getIncr = function
            | IncrProp incr -> incr |> Some
            | _ -> None


            let toString exact = function
                | MinProp min -> $"{min |> Minimum.toString exact}.."
                | MaxProp max -> $"..{max |> Maximum.toString exact}"
                | IncrProp incr -> $"..{incr |> Increment.toString exact}.."
                | ValsProp vs -> vs |> ValueSet.toString exact

    
        let apply unr fMin fMax fMinMax fIncr fMinIncr fIncrMax fValueSet = function
            | Unrestricted -> unr
            | Min min -> min |> fMin
            | Max max -> max |> fMax
            | MinMax (min, max) -> (min, max) |> fMinMax
            | Incr incr -> incr |> fIncr
            | MinIncr (min, incr) -> (min, incr) |> fMinIncr
            | IncrMax (incr, max) -> (incr, max) |> fIncrMax
            | ValSet vs -> vs |> fValueSet


        /// Count the number of values in a `ValueRange`.
        /// Returns 0 if no count is possible.
        let cardinality =
            let zero _ = 0
            apply 0 zero zero zero zero zero zero ValueSet.count


        /// Checks whether a `ValueRange` is `Unrestricted`
        let isUnrestricted =
            let returnFalse = Boolean.returnFalse

            apply
                true
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse


        /// Checks whether a `ValueRange` is `Min`
        let isMin =
            let returnFalse = Boolean.returnFalse

            apply
                false
                Boolean.returnTrue
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse


        /// Checks whether a `ValueRange` is `Max`
        let isMax =
            let returnFalse = Boolean.returnFalse

            apply
                false
                returnFalse
                Boolean.returnTrue
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse


        /// Checks whether a `ValueRange` is `MinMax`
        let isMinMax =
            let returnFalse = Boolean.returnFalse

            apply
                false
                returnFalse
                returnFalse
                Boolean.returnTrue
                returnFalse
                returnFalse
                returnFalse
                returnFalse


        let isIncr =
            let returnFalse = Boolean.returnFalse

            apply
                false
                returnFalse
                returnFalse
                returnFalse
                Boolean.returnTrue
                returnFalse
                returnFalse
                returnFalse


        let isMinIncr =
            let returnFalse = Boolean.returnFalse

            apply
                false
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                Boolean.returnTrue
                returnFalse
                returnFalse


        let isIncrMax =
            let returnFalse = Boolean.returnFalse

            apply
                false
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                Boolean.returnTrue
                returnFalse


        let isValueSet =
            let returnFalse = Boolean.returnFalse

            apply
                false
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                Boolean.returnTrue

        /// Checks whether a `BigRational` is between an optional 
        /// **min** and an optional **max** 
        let isBetweenMinMax min max v =
            let fMin = function
            | None -> true
            | Some(Minimum.MinIncl m) -> v >= m
            | Some(Minimum.MinExcl m) -> v > m

            let fMax  = function
            | None -> true
            | Some(Maximum.MaxIncl m) -> v <= m
            | Some(Maximum.MaxExcl m) -> v < m

            (fMin min) && (fMax max)


        let isMultipleOfIncr incrOpt v =
            let isDiv v i = v |> BigRational.isMultiple i

            match incrOpt with
            | None -> true
            | Some (Increment incr) -> incr |> Seq.exists (isDiv v)

        /// Filter a set of `BigRational` according
        /// to **min** **max** and incr constraints
        let filter minOpt incrOpt maxOpt (ValueSet vs) =
            vs 
            |> Set.filter (fun v ->
                v |> isBetweenMinMax minOpt maxOpt &&
                v |> isMultipleOfIncr incrOpt
            )
            |> ValueSet.create


        let minEQmax max min =
            match min, max with
            | Minimum.MinIncl min, Maximum.MaxIncl max -> min = max
            | _ -> false

        /// Checks whether `Minimum` **min** > `Maximum` **max**.
        /// Note that inclusivity or exclusivity of a minimum and maximum must be
        /// accounted for.
        let minGTmax max min =
            match min, max with
            | Minimum.MinIncl min, Maximum.MaxIncl max -> min > max
            | Minimum.MinExcl min, Maximum.MaxIncl max
            | Minimum.MinExcl min, Maximum.MaxExcl max
            | Minimum.MinIncl min, Maximum.MaxExcl max -> min >= max

        /// Checks whether `Minimum` **min** <= `Maximum` **max**
        let minSTEmax max min = min |> minGTmax max |> not


        let minMultipleOf (Increment incr) min = min |> Minimum.multipleOf incr


        let maxMultipleOf (Increment incr) max = max |> Maximum.multipleOf incr


        /// An `Unrestricted` `ValueRange`.
        let unrestricted = Unrestricted


        /// Create a `Minimum` `Range` that is
        /// either inclusive or exclusive.
        let createMin isIncl m = m |> Minimum.create isIncl |> Min


        /// Create a `Maximum` `Range` that is
        /// either inclusive or exclusive.
        let createMax isIncl m = m |> Maximum.create isIncl |> Max


        /// Create a `MinMax` `ValueRange`. If **min** > **max** raises 
        /// an `MinLargetThanMax` exception. If min equals max, a `ValueSet` with
        /// value min (= max).
        let minMaxToValueRange min max =
            if min |> minGTmax max then 
                Exceptions.raiseMinLargerThanMax min max
    
            elif min |> minEQmax max then
                min 
                |> Minimum.toBigRational
                |> Set.singleton
                |> ValueSet.create
                |> ValSet

            else (min, max) |> MinMax


        let createMinMax min minIncl max maxIncl =
            let min = min |> Minimum.create minIncl
            let max = max |> Maximum.create maxIncl

            minMaxToValueRange min max

        /// Create a `Range` with a `Minimum`, `Increment` and a `Maximum`.
        let createIncr = Increment.create >> Incr


        let createValSet brs = brs |> ValueSet.create |> ValSet


        let minIncrToValueRange min incr =
            (min |> minMultipleOf incr, incr) |> MinIncr

        /// Create a `MinIncr` `ValueRange`.
        let createMinIncr min minIncl incr =
            incr
            |> Increment.create
            |> minIncrToValueRange (Minimum.create min minIncl)


        /// Create an `IncrMax` `ValueRange`.
        let incrMaxToValueRange incr max =
            (incr, max |> maxMultipleOf incr) |> IncrMax


        let createIncrMax incr max maxIncl =
            max
            |> Maximum.create maxIncl
            |> incrMaxToValueRange (incr |> Increment.create)


        /// Create a `MinIncrMax` `ValueRange`. If **min** > **max** raises 
        /// an `MinLargetThanMax` exception. If min equals max, a `ValueSet` with 
        /// value min (=max).
        let minIncrMaxToValueRange min incr max =
            if min |> minGTmax max then 
                Exceptions.raiseMinLargerThanMax min max
            else
                let min = min |> minMultipleOf incr |> Minimum.toBigRational
                let max = max |> maxMultipleOf incr |> Maximum.toBigRational

                let (Increment incr) = incr
                [
                    for i in incr do
                        [ min..i..max]
                ]
                |> List.collect id
                |> Set.ofList
                |> ValueSet.create
                |> ValSet

        /// Create a `ValueRange` using a `ValueSet` **vs**
        /// an optional `Minimum` **min**, **incr** and `Maximum` **max**.
        /// If both **min**, **incr** and **max** are `None` an `Unrestricted` 
        /// `ValueRange` is created. 
        let create min incr max vs =
            match vs with
            | None ->
                match min, incr, max with
                | None,     None,      None     -> unrestricted
                | Some min, None,      None     -> min |> Min 
                | None,     None,      Some max -> max |> Max 
                | Some min, None,      Some max -> minMaxToValueRange min max
                | Some min, Some incr, None     -> minIncrToValueRange min incr
                | None,     Some incr, Some max -> incrMaxToValueRange incr max
                | None,     Some incr, None     -> incr |> Incr
                | Some min, Some incr, Some max -> minIncrMaxToValueRange min incr max

            | Some vs ->
                vs
                |> filter min incr max
                |> ValSet

        /// Get an optional `Minimum` in a `ValueRange`
        let getMin = 
            apply
                None 
                Some
                Option.none
                (fst >> Some)
                Option.none
                (fst >> Some)
                Option.none
                ValueSet.getMin

        /// Get an optional `Maximum` in a `ValueRange`
        let getIncr = 
            apply
                None 
                Option.none
                Option.none
                Option.none
                Some
                (snd >> Some)
                (fst >> Some)
                Option.none

        /// Get an optional `Maximum` in a `ValueRange`
        let getMax = 
            apply
                None 
                Option.none
                Some
                (snd >> Some)
                Option.none
                Option.none
                (snd >> Some)
                ValueSet.getMax

        /// Get an optional `ValueSet` in a `ValueRange`
        let getValSet = 
            apply
                None 
                Option.none
                Option.none
                Option.none
                Option.none
                Option.none
                Option.none
                Some

        /// Check whether a `ValueRange` **vr** contains
        /// a `BigRational` **v**.
        let contains v vr =
            match vr with
            | ValSet vs -> vs |> ValueSet.contains v
            | _ ->
                let min = vr |> getMin
                let max = vr |> getMax

                let incr = vr |> getIncr
                v |> isBetweenMinMax min max &&
                v |> isMultipleOfIncr incr

        /// Apply a `Minimum` **min** to a `ValueRange` **vr**.
        /// If minimum cannot be set the original `Minimum` is returned.
        /// So, it always returns a more restrictive, i.e. larger, or equal `Minimum`.
        let setMin newMin (vr: ValueRange) =
            let restrict = Minimum.restrict newMin

            let fMin = restrict >> Min

            let fMax max = minMaxToValueRange newMin max

            let fMinMax (min, max) = minMaxToValueRange (min |> restrict) max

            let fIncr incr = minIncrToValueRange newMin incr

            let fMinIncr (min, incr) =
                minIncrToValueRange (min |> restrict) incr

            let fIncrMax (incr, max) =
                minIncrMaxToValueRange newMin incr max

            let fValueSet =
                filter (Some newMin) None None >> ValSet

            vr
            |> apply
                (newMin |> Min)
                fMin
                fMax
                fMinMax
                fIncr
                fMinIncr
                fIncrMax
                fValueSet


        /// Apply a **incr** to a `ValueRange` **vr**.
        /// If increment cannot be set the original is returned.
        /// So, the resulting increment is always more restrictive as the previous one
        let setIncr newIncr vr =
            let restrict = Increment.restrict newIncr

            let fMin min = minIncrToValueRange min newIncr

            let fMax max = incrMaxToValueRange newIncr max

            let fMinMax (min, max) = minIncrMaxToValueRange min newIncr max

            let fIncr = restrict >> Incr

            let fMinIncr (min, incr) =
                minIncrToValueRange min (incr |> restrict)

            let fIncrMax (incr, max) =
                incrMaxToValueRange (incr |> restrict) max

            let fValueSet =
                filter None (Some newIncr) None >> ValSet

            vr
            |> apply
                (newIncr |> Incr)
                fMin
                fMax
                fMinMax
                fIncr
                fMinIncr
                fIncrMax
                fValueSet


        let setMax newMax (vr: ValueRange) =
            let restrict = Maximum.restrict newMax

            let fMin min = minMaxToValueRange min newMax

            let fMax max = max |> restrict |> Max

            let fMinMax (min, max) = minMaxToValueRange min (max |> restrict)

            let fIncr incr = incrMaxToValueRange incr newMax

            let fMinIncr (min, incr) =
                minIncrMaxToValueRange min incr newMax

            let fIncrMax (incr, max) =
                incrMaxToValueRange incr (max |> restrict)

            let fValueSet =
                filter None None (Some newMax) >> ValSet

            vr
            |> apply
                (newMax |> Max)
                fMin
                fMax
                fMinMax
                fIncr
                fMinIncr
                fIncrMax
                fValueSet


        let setValueSet newVs (vr: ValueRange) =
            let min, incr, max, oldVs =
                vr |> getMin, vr |> getIncr, vr |> getMax, vr |> getValSet

            match oldVs with
            | None    -> newVs |> filter min incr max
            | Some vs -> newVs |> ValueSet.intersect vs
            |> ValSet



        /// Create a string (to print) representation of a `ValueRange`. 
        /// `Exact` true prints exact bigrationals, when false
        /// print as floating numbers
        let print exact min incr max vs =

            let printRange min incr max =
                let minToStr = Minimum.toString exact
                let maxToStr = Maximum.toString exact
                let incrToStr = Increment.toString exact

                match min, incr, max with
                | None,     None,  None     -> "<..>"                    
                | Some min, None,  None     -> $"{min |> minToStr}..>"
                | Some min, None,  Some max -> $"{min |> minToStr}..{max |> maxToStr}"
                | None,     None,  Some max -> $"<..{max |> maxToStr}"
                | None,     Some incr, None     -> $"<..{incr |> incrToStr}..>"
                | Some min, Some incr, None     -> $"{min |> minToStr}..{incr |> incrToStr}..>"
                | None,     Some incr, Some max -> $"<..{incr |> incrToStr}..{max |> maxToStr}"
                | Some min, Some incr, Some max -> $"{min |> minToStr}..{incr |> incrToStr}..{max |> maxToStr}"

            match vs with
            | Some vs -> $"{vs |> ValueSet.toString exact}"
            | None -> printRange min incr max


        /// Convert a `ValueRange` to a `string`.
        let toString exact vr =
            let fVs vs =
                print exact None None None (Some vs)

            let unr = print exact None None None None

            let print min incr max = print exact min incr max None

            let fMin min = print (Some min) None None

            let fMax max = print None None (Some max)

            let fIncr incr = print None (Some incr) None

            let fMinIncr (min, incr)  = print (Some min) (Some incr) None

            let fIncrMax (incr, max)  = print None (Some incr) (Some max)

            let fMinMax (min, max) = print (Some min) None (Some max)

            vr
            |> apply
                unr
                fMin
                fMax
                fMinMax
                fIncr
                fMinIncr
                fIncrMax
                fVs

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

                let raiseExc m = m |> MinMaxCalculatorException |> raise


            /// Calculate **x1** and **x2** with operator **op**
            /// and use **incl1** and **inc2** to determine whether
            /// the result is inclusive. Use constructor **c** to
            /// create the optional result.
            let calc c op (x1, incl1) (x2, incl2) =
                let opIsMultOrDiv = (op |> BigRational.opIsMult || op |> BigRational.opIsDiv)

                let incl = 
                    match incl1, incl2 with
                    | true, true -> true
                    | _ -> false

                match x1, x2 with
                | Some v, _  when opIsMultOrDiv && v = 0N ->
                    0N |> c incl1 |> Some 
                | Some v, _
                | _, Some v when op |> BigRational.opIsMult && v = 0N ->
                    0N |> c incl |> Some
                | Some _, None when op |> BigRational.opIsDiv ->
                    0N |> c incl |> Some
                | Some (v1), Some (v2) ->
                    if op |> BigRational.opIsDiv && v2 = 0N then None
                    else 
                        v1 |> op <| v2 |> c incl |> Some
                | _ -> None


            /// Calculate an optional `Minimum`
            let calcMin = calc Minimum.create


            /// Calculate an optional `Maximum`
            let calcMax = calc Maximum.create


            let minimize min1 min2 =
                match min1, min2 with
                | None,    None     -> None
                | Some _,  None 
                | None,    Some _   -> None
                | Some m1, Some m2 ->
                    if m1 |> Minimum.minSTmin m2 then m1
                    else m2
                    |> Some


            let maximize max1 max2 =
                match max1, max2 with
                | None,    None     -> None
                | Some _,  None 
                | None,    Some _   -> None
                | Some m1, Some m2 ->
                    if m1 |> Maximum.maxGTmax m2 then m1
                    else m2
                    |> Some


            /// Match a min, max tuple **min**, **max**
            /// to:
            ///
            /// * `PP`: both positive
            /// * `NN`: both negative
            /// * `NP`: one negative, the other positive
            let (|PP|NN|NP|NZ|ZP|) (min, max) =
                match min, max with
                | Some min, _         when min > 0N             -> PP
                | _,        Some max  when max < 0N             -> NN
                | Some min, Some max  when min < 0N && max > 0N -> NP
                | None,     Some max  when max > 0N             -> NP
                | Some min, None      when min < 0N             -> NP
                | None,     None                                -> NP
                | _,        Some max  when max = 0N             -> NZ
                | Some min, _         when min = 0N             -> ZP
                // failing cases
                | Some min, Some max when min = 0N && max = 0N  ->
                    $"{min} = {max} = 0" |> failwith
                | Some min, Some max when min >= 0N && max < 0N ->
                    $"{min} > {max}" |> failwith
                | _ -> $"could not handle {min} {max}" |> failwith


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
                | PP, ZP ->  // min = min1 * min2, max = max1 * max2
                    calcMin (*) min1 min2, calcMax (*) max1 max2
                | PP, NN -> // min = max1 * min2, max = min1 * max2
                    calcMin (*) max1 min2, calcMax (*) min1 max2
                | PP, NZ -> // min = max1 * min2, max = min1 * max2
                    calcMin (*) max1 min2, calcMax (*) min1 max2
                | PP, NP -> // min = min1 * min2, max = max1 * max2
                    calcMin (*) max1 min2, calcMax (*) max1 max2

                | ZP, PP ->  // min = min1 * min2, max = max1 * max2
                    calcMin (*) min1 min2, calcMax (*) max1 max2
                | ZP, ZP ->  // min = min1 * min2, max = max1 * max2
                    calcMin (*) min1 min2, calcMax (*) max1 max2
                | ZP, NN -> // min = max1 * min2, max = min1 * max2
                    calcMin (*) max1 min2, calcMax (*) min1 max2
                | ZP, NZ -> // min = max1 * min2, max = min1 * max2
                    calcMin (*) max1 min2, calcMax (*) min1 max2
                | ZP, NP -> // min = min1 * min2, max = max1 * max2
                    calcMin (*) min1 min2, calcMax (*) max1 max2

                | NN, PP -> // min = min1 * max2, max = max1 * min2
                    calcMin (*) min1 max2, calcMax (*) max1 min2
                | NN, ZP -> // min = min1 * max2, max = max1 * min2
                    calcMin (*) min1 max2, calcMax (*) max1 min2
                | NN, NN -> // min = max1 * max2, max = min1 * min2
                    calcMin (*) max1 max2, calcMax (*) min1 min2
                | NN, NZ -> // min = max1 * max2, max = min1 * min2
                    calcMin (*) max1 max2, calcMax (*) min1 min2
                | NN, NP -> // min = min1 * max2, max = min1 * min2
                    calcMin (*) min1 max2, calcMax (*) min1 min2

                | NZ, PP -> // min = min1 * max2, max = max1 * min2
                    calcMin (*) min1 max2, calcMax (*) max1 min2
                | NZ, ZP -> // min = min1 * max2, max = max1 * min2
                    calcMin (*) min1 max2, calcMax (*) max1 min2
                | NZ, NN -> // min = max1 * max2, max = min1 * min2
                    calcMin (*) max1 max2, calcMax (*) min1 min2
                | NZ, NZ -> // min = max1 * max2, max = min1 * min2
                    calcMin (*) max1 max2, calcMax (*) min1 min2
                | NZ, NP -> // min = min1 * max2, max = min1 * min2
                    calcMin (*) min1 max2, calcMax (*) min1 min2

                | NP, PP -> // min = min1 * max2, max = max1 * max2
                    calcMin (*) min1 max2, calcMax (*) max1 max2
                | NP, ZP -> // min = min1 * max2, max = max1 * max2
                    calcMin (*) min1 max2, calcMax (*) max1 max2
                | NP, NN -> // min = max1 * min2, max = min1 * min2
                    calcMin (*) max1 min2, calcMax (*) min1 min2
                | NP, NZ -> // min = max1 * min2, max = min1 * min2
                    minimize (calcMin (*) min1 max2) (calcMin (*) min2 max1),
                    maximize (calcMax (*) max1 max2) (calcMax (*) min1 min2)
                | NP, NP -> // min = min1 * max2, max = max1 * max2
                    minimize (calcMin (*) min1 max2) (calcMin (*) min2 max1),
                    maximize (calcMax (*) max1 max2) (calcMax (*) min1 min2)


            /// Calculate `Minimum` option and
            /// `Maximum` option for division of
            /// (**min1**, **max1**) and (**min2**, **max2)
            let division min1 max1 min2 max2 =
                match (min1 |> fst, max1 |> fst), (min2 |> fst, max2 |> fst) with
                | PP, PP -> // min = min1 / max2, max =	max1 / min2
                    calcMin (/) min1 max2, calcMax (/) max1 min2
                | PP, NN -> // min = max1 / max2	, max = min1 / min2
                    calcMin (/) max1 max2, calcMax (/) min1 min2
                | PP, ZP ->
                    calcMin (/) min1 max2, calcMax (/) max1 min2

                | ZP, PP -> // min = min1 / max2, max =	max1 / min2
                    calcMin (/) min1 max2, calcMax (/) max1 min2
                | ZP, NN -> // min = max1 / max2	, max = min1 / min2
                    calcMin (/) max1 max2, calcMax (/) min1 min2
                | ZP, ZP ->
                    calcMin (/) min1 max2, calcMax (/) max1 min2

                | NN, PP -> // min = min1 / min2, max = max1 / max2
                    calcMin (/) min1 min2, calcMax (/) max1 max2
                | NN, NN -> // min = max1 / min2	, max = min1 / max2
                    calcMin (/) max1 min2, calcMax (/) min1 max2
                | NN, NZ ->
                    calcMin (/) max1 min2, calcMax (/) min1 max2
                | NN, ZP ->
                    calcMin (/) min1 min2, calcMax (/) max1 max2

                | NZ, PP -> // min = min1 / min2, max = max1 / max2
                    calcMin (/) min1 min2, calcMax (/) max1 max2
                | NZ, NN -> // min = max1 / min2	, max = min1 / max2
                    calcMin (/) max1 min2, calcMax (/) min1 max2
                | NZ, NZ ->
                    calcMin (/) max1 min2, calcMax (/) min2 max2

                | NP, PP -> // min = min1 / min2, max = max1 / min2
                    calcMin (/) min1 min2, calcMax (/) max1 min2
                | NP, NN -> // min = max1 / max2, max = min1 / max2
                    calcMin (/) max1 max2, calcMax (/) min1 max2
                // division by range containing zero
                | NN, NP
                | PP, NP
                | NP, NP 
                | NZ, NP
                | ZP, NP

                | NP, ZP 
                | NZ, ZP

                | PP, NZ
                | NP, NZ
                | ZP, NZ -> None, None


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

            let calcMinMax min1 max1 min2 max2 =
                let getMin m =
                    let incl =
                        match m with
                        | Some v -> v |> Minimum.isIncl
                        | None   -> false
                    m |> Option.bind (Minimum.toBigRational >> Some), incl

                let getMax m =
                    let incl =
                        match m with
                        | Some v -> v |> Maximum.isIncl
                        | None   -> false
                    m |> Option.bind (Maximum.toBigRational >> Some), incl

                MinMaxCalcultor.calcMinMax
                    op
                    (min1 |> getMin)
                    (max1 |> getMax)
                    (min2 |> getMin)
                    (max2 |> getMax)


            match x1, x2 with
            | Unrestricted, Unrestricted -> unrestricted
            | ValSet s1, ValSet s2 -> ValueSet.calc op s1 s2 |> ValSet

            // A set with an increment results in a new set of increment
            // Need to match all scenarios with a valueset and an increment
            | ValSet s, Incr i
            | Incr i, ValSet s

            | ValSet s, MinIncr(_, i)
            | MinIncr(_, i), ValSet s

            | ValSet s, IncrMax(i, _)
            | IncrMax(i, _), ValSet s ->

                let min1, max1 = x1 |> getMin, x1 |> getMax
                let min2, max2 = x2 |> getMin, x2 |> getMax

                let min, max = calcMinMax min1 max1 min2 max2

                // calculate a new increment based upon the valueset and an increment
                let incr1 = i |> Some
                let incr2 =
                    let (ValueSet s) = s
                    s |> Increment.create |> Some
                let incr = Increment.calcOpt op incr1 incr2

                match min, incr, max with
                | None, None, None -> unrestricted
                | _ -> create min incr max None

            // In any other case calculate min, incr and max
            | _ ->
                let min1, incr1, max1 = x1 |> getMin, x1 |> getIncr, x1 |> getMax
                let min2, incr2, max2 = x2 |> getMin, x2 |> getIncr, x2 |> getMax

                let min, max = calcMinMax min1 max1 min2 max2

                // calculate a new increment based upon the incr1 and incr2
                let incr = Increment.calcOpt op incr1 incr2

                match min, incr, max with
                | None, None, None -> unrestricted
                | _ -> create min incr max None


        /// Checks whether a `ValueRange` vr1 is a subset of 
        /// `ValueRange` vr2.
        let isSubSetOf vr2 vr1 =
            match vr1, vr2 with
            | ValSet s1, ValSet s2 ->
                s2 |> ValueSet.isSubset s1
            | _ -> false


        let toProperties vr =
            let unr = set []

            let fMin min = set [ min |> MinProp ]

            let fMax max = set [ max |> MaxProp ]

            let fMinMax (min, max) = set [ min |> MinProp; max |> MaxProp ]

            let fIncr incr = set [ incr |> IncrProp ]

            let fMinIncr (min, incr) = set [ min |> MinProp; incr |> IncrProp ]

            let fIncrMax (incr, max) = set [ incr |> IncrProp; max |> MaxProp ]

            let fVs vs = set [ vs |> ValsProp ]

            vr
            |> apply
                unr
                fMin
                fMax
                fMinMax
                fIncr
                fMinIncr
                fIncrMax
                fVs


        let diffWith vr1 vr2 =
            vr1
            |> toProperties
            |> Set.difference (vr2 |> toProperties)


        /// Set a `ValueRange` expr to a `ValueRange` y.
        /// So, the result is equal to or more restrictive than the original `y`. 
        let applyExpr y expr =
            let set get set vr =
                match expr |> get with
                | Some m -> vr |> set m
                | None   -> vr

            match expr with
            | Unrestricted -> y
            | ValSet vs    -> y |> setValueSet vs
            | _ ->
                y
                |> set getMin setMin
                |> set getMax setMax
                |> set getIncr setIncr

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
            { v with
                Values = (v |> get).Values <== vr
            }

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
            |> ValueRange.setMin (BigRational.zero |> Minimum.create false)

        { v with Values = vr }


    /// Get the number of distinct values
    let count v = v |> getValueRange |> ValueRange.cardinality


    /// Checks whether **v1** and **v2** have the
    /// same `Name`
    let eqName v1 v2 = v1 |> getName = (v2 |> getName)


    let eqValues var1 var2 =
        var1 |> getValueRange = (var2 |> getValueRange)
 
    /// Checks whether a `Variable` **v** is solved,
    /// i.e. there is but one possible value left.
    let isSolved var =
        (var |> getValueRange |> ValueRange.isValueSet) &&
        (var |> count = 1)

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
        (v1 |> getValueRange) |> op <| (v2 |> getValueRange)
        |> createRes

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

        /// Constant 0
        let zero =
            [0N]
            |> ValueRange.createValSet 
            |> createSucc (Name.createExc "zero")

        /// Constant 1
        let one = 
            [1N]
            |> ValueRange.createValSet 
            |> createSucc (Name.createExc "one")

        /// Constant 2
        let two =
            [2N]
            |> ValueRange.createValSet 
            |> createSucc (Name.createExc "two")

        /// Constant 3
        let three =
            [3N]
            |> ValueRange.createValSet 
            |> createSucc (Name.createExc "three")

        /// Check whether the operator is subtraction
        let opIsSubtr op = (three |> op <| two) |> eqValues (three ^- two) // = 1

        /// Check whether the operator is addition
        let opIsAdd op   = (three |> op <| two) |> eqValues (three ^+ two) // = 5

        /// Check whether the operator is multiplication
        let opIsMult op  = (three |> op <| two) |> eqValues (three ^* two) // = 6

        /// Check whether the operator is divsion
        let opIsDiv op   = (three |> op <| two) |> eqValues (three ^/ two) // = 3/2


        let toString op =
            match op with
            | _ when op |> opIsMult  -> "x"
            | _ when op |> opIsDiv   -> "/"
            | _ when op |> opIsAdd   -> "+"
            | _ when op |> opIsSubtr -> "-"
            | _ -> ""


    /// Handle the creation of a `Variable` from a `Dto` and
    /// vice versa.
    module Dto =

        /// The `Dto` representation of a `Variable`
        type Dto =
            {
                Name: string
                Min: BigRational option
                MinIncl: bool
                Incr : BigRational list
                Max: BigRational option
                MaxIncl: bool
                Vals: BigRational list
            }

        let isUnr (dto : Dto) =
            dto.Min = None && dto.Max = None &&
            dto.Incr |> List.isEmpty && dto.Vals |> List.isEmpty

        /// Create a `Dto`
        let createDto n min minincl incr max maxincl vals =
            {
                Name = n
                Vals = vals
                Min = min
                MinIncl = minincl
                Incr = incr
                Max = max
                MaxIncl = maxincl
            }

        /// Create an *empty* *new* `Dto` with only a name **n**
        let createNew n = createDto n None false [] None false []

        /// Apply `f` to an `Dto` `d`
        let apply f (d: Dto) = f d

        /// Apply an array of `vals` to an **dto**
        /// making sure the `Unr` is set to `false`.
        let setVals vals dto = { dto with Vals = vals }


        let setIncr incr dto = { dto with Incr = incr }

        /// Set a `min` to an **dto** that is either inclusive `incl` true or exclusive `false`
        let setMin  min incl dto = { dto with Min = min; MinIncl = incl }

        /// Set a `max` to an **dto** that is either inclusive `incl` true or exclusive `false`
        let setMax  max incl dto = { dto with Max = max; MaxIncl = incl }

        /// Match a string **p** to a field of `Dto`
        let (|Vals|Incr|MinIncl|MinExcl|MaxIncl|MaxExcl|NoProp|) p =
            match p |> String.toLower with
            | "vals"     -> Vals
            | "incr"     -> Incr
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
            | Incr     -> dto |> setIncr vs
            | MinIncl  -> dto |> setMin  (vs |> getVal) true
            | MinExcl  -> dto |> setMin  (vs |> getVal) false
            | MaxIncl  -> dto |> setMax  (vs |> getVal) true
            | MaxExcl  -> dto |> setMax  (vs |> getVal) false
            | _   -> dto


        /// Create a `Variable` from a `Dto` and
        /// raise a `DtoException` if this fails.
        let fromDto (dto: Dto) =
            let succ = id

            let n = dto.Name |> Name.create succ (fun m -> m |> Name.Exceptions.raiseExc)

            let vs =
                match dto.Vals with
                | [] -> None
                | _ ->
                    dto.Vals
                    |> Set.ofList
                    |> ValueRange.ValueSet.create
                    |> Some

            let min = dto.Min |> Option.bind (fun v -> v |> Minimum.create dto.MinIncl |> Some)
            let max = dto.Max |> Option.bind (fun v -> v |> Maximum.create dto.MaxIncl |> Some)
            let incr =
                if dto.Incr |> List.isEmpty then None
                else
                    dto.Incr
                    |> Set.ofList |> ValueRange.Increment.create
                    |> Some

            let vr = ValueRange.create min incr max vs  

            create succ n vr

        /// Return a `string` representation of a `Dto`
        let toString exact = fromDto >> toString exact

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
                    dto.Vals
                    |> Set.ofList
                    |> ValueRange.ValueSet.create
                    |> Some

            let min = dto.Min |> Option.bind (fun v -> v |> Minimum.create dto.MinIncl |> Some)
            let max = dto.Max |> Option.bind (fun v -> v |> Maximum.create dto.MaxIncl |> Some)
            let incr =
                if dto.Incr |> List.isEmpty then None
                else
                    dto.Incr
                    |> Set.ofList |> ValueRange.Increment.create
                    |> Some

            try
                let vr = ValueRange.create min incr max vs

                match n with
                | Some n' -> create succ n' vr
                | _ -> None
            with _ -> None


        /// Create a `Dto` from a `Variable`.
        let toDto (v: Variable) =

            let dto = createNew (let (Name.Name n) = v.Name in n)

            let minincl =
                match v.Values |> ValueRange.getMin with
                | Some m -> m |> Minimum.isExcl |> not | None -> false

            let maxincl =
                match v.Values |> ValueRange.getMax with
                | Some m -> m |> Maximum.isExcl |> not | None -> false

            let min  =
                v.Values
                |> ValueRange.getMin
                |> Option.bind (Minimum.toBigRational >> Some)

            let max  =
                v.Values
                |> ValueRange.getMax
                |> Option.bind (Maximum.toBigRational >> Some)

            let vals =
                v.Values
                |> ValueRange.getValSet
                |> function
                | Some (ValueSet vs) -> vs |> Set.toList
                | None -> []

            { dto with
                Vals = vals
                Min = min
                MinIncl = minincl
                Max = max
                MaxIncl = maxincl }
