
#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: FParsec"

#r "../../Informedica.Utils.Lib/bin/Debug/net5.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.GenUnits.Lib/bin/Debug/net5.0/Informedica.GenUnits.Lib.dll"


#time



/// Helper functions for `Option`
module Option = 

    let none _ = None



module Boolean =

    let returnFalse _ = false

    let returnTrue _ = true



/// Helper functions for `List`
module List =

    /// Replace an element in a list
    /// when the `pred` function returns `true`.
    let replace pred x xs =
        match xs |> List.tryFindIndex pred with
        | Some(ind) ->
            (xs |> Seq.take ind |> Seq.toList) @ [x] @ 
            (xs |> Seq.skip (ind + 1) |> Seq.toList)
        | None -> xs

    let distinct xs = xs |> Seq.ofList |> Seq.distinct |> Seq.toList


    let replaceOrAdd pred x xs =
        if xs |> List.exists pred then
            xs 
            |> List.map (fun x' ->
                if x' |> pred then x else x'
            )
        else x::xs



module Types =

    open System
    open MathNet.Numerics

    /// Represents a non empty/null string identifying a `Variable`.
    /// `Name` can be no longer than 1000 characters.
    type Name = Name of string


    /// The minimal value in
    /// a `Range`. Can be inclusive
    /// or exclusive.
    type Minimum =
        | MinIncl of BigRational
        | MinExcl of BigRational


    /// The maximum value in
    /// a `Range`. Can be inclusive
    /// or exclusive.
    type Maximum =
        | MaxIncl of BigRational
        | MaxExcl of BigRational


    type Range =
        {   
            Multiples : bigint Set
            Delta : BigRational
        }


    /// `ValueRange` represents a discrete set of
    /// rational numbers.
    type ValueRange =
        | Unrestricted
        | Min of Minimum
        | Max of Maximum
        | MinMax  of Minimum * Maximum
        | Range of Range
        | ValueSet of Set<BigRational>


    /// Represents a variable in an
    /// `Equation`. The variable is
    /// identified by `Name` and has
    /// a `Values` that are either
    /// `Unrestricted` or restricted by
    /// a `ValueSet` or a `Range`.
    type Variable =
        {
            Name: Name
            Values: ValueRange
        }


    /// An equation is either a `ProductEquation`
    /// or a `Sumequation`, the first variable is the
    /// dependent variable, i.e. the result of the 
    /// equation, the second part are the independent
    /// variables in the equation
    type Equation = 
        | ProductEquation of Variable * Variable list
        | SumEquation     of Variable * Variable list


    /// The `Result` of solving an `Equation`
    /// is that either the `Equation` is the 
    /// same or has `Changed`.
    type Result =
        | UnChanged
        | Changed   of Variable list


    /// Represents a property of a `Variable`.
    ///
    /// * `Vals`: A set of distinct values
    /// * `MinIncl`: An inclusive minimum
    /// * `MinExcl`: An exclusive minimum
    /// * `MaxIncl`: An inclusive maximum
    /// * `MaxExcl`: An exclusive maximum
    type Property =
        | ValsProp of BigRational Set
        | DeltaProp of BigRational
        | MinInclProp of BigRational
        | MinExclProp of BigRational
        | MaxInclProp of BigRational
        | MaxExclProp of BigRational


    /// A limitation of the maximum number
    /// of values to use as a constraint
    type Limit = 
        | MinLim of int
        | MaxLim of int
        | MinMaxLim of (int * int)
        | NoLimit


    /// Represents a constraint on a `Variable`.
    /// I.e. either a set of values, or an increment
    /// or a minimum of maximum.
    type Constraint =
        {
            Name : Name
            Property : Property
            Limit : Limit
        }


    module Events =

        type Event =
        | EquationCouldNotBeSolved of Equation
        | EquationStartedCalculation of Variable list
        | EquationStartedSolving of Equation
        | EquationFinishedCalculation of Variable list * Variable list
        | EquationVariableChanged of Variable
        | EquationFinishedSolving of Variable list
        | EquationLoopedSolving of bool * Variable * Variable list * Variable list
        | SolverLoopedQue of Equation list
        | ConstraintSortOrder of (int * Constraint) list
        | ConstraintVariableNotFound of Constraint * Equation list
        | ConstraintLimitSetToVariable of Limit * Variable
        | ConstraintVariableApplied of Constraint * Variable
        | ConstrainedEquationsSolved of Constraint * Equation list
        | ApiSetVariable of Variable * Equation list
        | ApiEquationsSolved of Equation list
        | ApiAppliedConstraints of Constraint list * Equation list

    
    module Exceptions =

        type Message =
        | NameNullOrWhiteSpaceException
        | NameLongerThan1000 of string
        | ValueRangeMinLargerThanMax of Minimum * Maximum
        | ValueRangeNotAValidOperator
        | ValueRangeEmptyValueSet
        | VariableCannotSetValueRange of (Variable * ValueRange)
        | EquationDuplicateVariables of Variable list
        | EquationEmptyVariableList 
        | SolverInvalidEquations of Equation list


    module Logging =


        type IMessage = interface end
        type TimeStamp = DateTime


        type Level =
            | Informative
            | Debug
            | Warning
            | Error


        type SolverMessage =
            | ExceptionMessage of Exceptions.Message
            | SolverMessage of Events.Event
            interface IMessage


        type Message =
            {
                TimeStamp : TimeStamp
                Level : Level
                Message : IMessage
            }

    
        type Logger =   
            {
                Log : Message -> unit
            }



module Logging =

    
    open System
    open Types.Logging


    let private create l e = 
        {
            TimeStamp = DateTime.Now
            Level = l
            Message = e
        } 


    let logMessage level (logger : Logger) evt =
        evt 
        |> SolverMessage
        |> create level
        |> logger.Log


    let logInfo logger msg = logMessage Informative logger msg


    let logWarning logger msg = logMessage Warning logger msg


    let logError (logger : Logger) msg = 
        msg
        |> ExceptionMessage
        |> create Error
        |> logger.Log




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


        module Exceptions =


            exception ValueRangeException of Exceptions.Message


            let raiseExc m = m |> ValueRangeException |> raise


            let raiseMinLargerThanMax min max =
                (min, max)
                |> Exceptions.ValueRangeMinLargerThanMax
                |> raiseExc


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
        /// where **unr** is used for an `Unrestricted`
        /// `ValueRange`, **fv** is used for `ValueSet` and
        /// **fr** is used for `Range`
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


        /// Checks whether a `ValueRange` is `Unrestricted`
        let isMin =
            let returnFalse = Boolean.returnFalse
            apply false Boolean.returnTrue returnFalse returnFalse returnFalse returnFalse


        /// Checks whether a `ValueRange` is `Unrestricted`
        let isMax =
            let returnFalse = Boolean.returnFalse
            apply true returnFalse Boolean.returnTrue returnFalse returnFalse returnFalse


        /// Checks whether a `ValueRange` is `Unrestricted`
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
        /// When min and max are `None` then this will
        let isBetween min max v =
            let fTrue = fun _ -> true

            let fMin  = function
            | None -> fTrue
            | Some(Minimum.MinIncl m) -> (<=) m
            | Some(Minimum.MinExcl m) -> (<) m

            let fMax  = function
            | None -> fTrue
            | Some(Maximum.MaxIncl m) -> (>=) m
            | Some(Maximum.MaxExcl m) -> (>) m

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
        /// an optional `Minimum` **min**, **incr** and `Maximum` **max**.
        /// If both **min**, **incr** and **max** are `None` an `Unrestricted` 
        /// `ValueRange` is created. 
        let create vs min max =
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

            let fMinMax = fst >> checkMin >> Min

            let fValueSet =
                let max = vr  |> getMax
                filter (Some min) max >> ValueSet.create

            let returnVr _ = vr

            vr
            |> apply (min |> Min) fMin returnVr fMinMax returnVr fValueSet


        /// Apply a `Maximum` **max** to a `ValueRange` **vr**.
        /// If maximum cannot be set the original is returned.
        /// So, it always returns a more restrictive, i.e. smaller, or equal `Maximum`.
        let setMax max vr =
            // Check whether the new min is more restrictive than the old min
            let checkMax max' = 
                if max |> Maximum.maxLTmax max' then max' else max

            let fMax = checkMax >> Max

            let fMinMax = snd >> checkMax >> Max

            let fValueSet =
                let min = vr  |> getMin
                filter min (Some max) >> ValueSet.create

            let returnVr _ = vr

            vr
            |> apply (max |> Max) returnVr fMax fMinMax returnVr fValueSet



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

            create (Some vs2) min max


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




        /// Applies an infix operator **op**
        /// to `ValueRange` **x1** and **x2**.
        /// Calculates `Minimum`, increment or `Maximum`
        /// if either **x1** or **x2** is not a `ValueSet`.
        /// Doesn't perform any calculation when both
        /// **x1** and **x2** are `Unrestricted`.
        let calc op (x1, x2) =
            let calcOpt = calcOpt op

            // Note: doing this can have serious performance issues!!
            // let toVS vr =
            //     match vr with
            //     | Range(MinIncrMax (min, inr, max)) -> minIncrMaxToValueSet min inr max
            //     | _ -> vr
            // // first check if range can be changed to valueset
            // let x1 = x1 |> toVS
            // let x2 = x2 |> toVS

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
                | _ -> create None min max


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
                |> set getMin  setMin
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

            let vr = ValueRange.create vs min max 

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
                let vr = ValueRange.create vs min max

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



/// Functions that handle the `Equation` type that
/// either represents a `ProductEquation` </br>
/// y = x1 \* x2 * ... \* xn </br>
/// or a `SumEquations` </br>
/// y = x1 \* x2 * ... \* xn
module Equation =

    open Types
    open Variable.Operators

    module ValueRange = Variable.ValueRange
    
    module Exception =

        /// Equation exception
        exception EquationException of Exceptions.Message

        /// Raise an `EquationException` with `Message` `m`.
        let raiseExc m = m |> EquationException |> raise

    /// Create an `Equation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    /// The type of Equation product or sum
    /// is determined by the constructor **c**.
    let create c succ fail (y, xs) = 
        let vars = y::xs
        match vars |> List.filter (fun v -> vars |> List.filter ((=) v) |> List.length > 1) with
        | [] -> (y, xs) |> c |> succ
        | duplicates -> 
            duplicates 
            |> Exceptions.EquationDuplicateVariables 
            |> fail

    /// Create an `ProductEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    let createProductEq = create ProductEquation

    /// Create an `SumEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    let createSumEq = create SumEquation

    /// Create an `ProductEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time raising an exception.
    let createProductEqExc = createProductEq id Exception.raiseExc 

    /// Create an `SumEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time raising an exception.
    let createSumEqExc = createSumEq id Exception.raiseExc

    /// Apply **fp** to a `ProductEquation` and
    /// **fs** to a `SumEquation`.
    let apply fp fs = function
        | ProductEquation (y,xs) -> fp y xs
        | SumEquation (y, xs)    -> fs y xs

    /// Check whether an `Equation` is a product equation
    let isProduct = apply (fun _ _ -> true) (fun _ _ -> false)

    /// Check whether an `Equation` is a sum equation
    let isSum = apply (fun _ _ -> true) (fun _ _ -> false)

    /// Turn an `Equation` into a list of `Variable`
    let toVars = 
        let f y xs = y::xs
        apply f f

    let count e = 
        e
        |> toVars
        |> List.fold (fun (acc : int) v ->
            (+) (v |> Variable.count) acc
        ) 0

    let countProduct e = 
        e
        |> toVars
        |> List.fold (fun acc v ->
            let c = v |> Variable.count
            (if c = 0 then 1 else c) * acc
        ) 1


    let toString exact eq = 
        let op = if eq |> isProduct then "*" else "+"
        let varToString = Variable.toString exact

        match eq |> toVars with
        | [] -> ""
        | _::[] -> ""
        | y::xs -> 
            let s = 
                sprintf "%s = " (y |> varToString) + 
                (xs |> List.fold (fun s v -> s + (v |> varToString) + " " + op + " ") "")
            s.Substring(0, s.Length - 2)


    /// Make sure that the `Variables` in the
    /// `Equation` can only contain positive 
    /// non zero values.
    let nonZeroOrNegative e =
        let set c y xs =
            let y' = y |> Variable.setNonZeroOrNegative
            let xs' = xs |> List.map Variable.setNonZeroOrNegative
            (y', xs') |> c 
        let fp = set ProductEquation
        let fs = set SumEquation
        e |> apply fp fs

    /// Check whether an `Equation` contains
    /// a `Variable` **v**
    let contains v = toVars >> (List.exists (Variable.eqName v))

    /// Check whether `Equation`s 
    /// **eq1** and **eq2** are equal
    let equals eq1 eq2 = 
        let vrs1 = eq1 |> toVars
        let vrs2 = eq2 |> toVars
        vrs1 |> List.forall (fun vr -> 
            vrs2 |> List.exists (Variable.eqName vr)) &&
        ((eq1 |> isProduct) && (eq2 |> isProduct) ||
         (eq1 |> isSum)     && (eq2 |> isSum))

    /// Find a `Variable` **vr** in
    /// an `Equation` **eq** and return
    /// the result in a list
    let find vr eq =
        eq
        |> toVars
        |> List.filter (fun vr' -> vr' |> Variable.getName = (vr |> Variable.getName))

    /// Find a `Variable` with `Name`
    /// **n** in an `Equation` **eq**
    /// and return the result as a list
    let findName n eq =
        eq
        |> toVars
        |> List.filter (fun vr -> vr |> Variable.getName = n)

    /// Replace a `Variable` **v** in the 
    /// `Equation` **e**.
    let replace v e =
        let r c v vs =
            let vs = vs |> List.replace ((Variable.eqName) v) v
            c id (fun _ -> e) ((vs |> List.head), (vs|> List.tail))
        let fp y xs = r createProductEq v (y::xs)
        let fs y xs = r createSumEq v (y::xs)
        e |> apply fp fs


    // Check whether an equation is solved
    let isSolved = function
        | ProductEquation (y, xs) 
        | SumEquation (y, xs) ->
            [y] @ xs |> List.forall Variable.isSolved


    // Check whether an equation will change by calc
    // This is not the same as `isSolved`!! If all 
    // the variables are unrestricted than the equation
    // is not solvable but is also not solved.
    // ToDo when > 1 variable is <0..> --> also not solvable!! 
    let isSolvable = function 
        | ProductEquation (y, xs)
        | SumEquation (y, xs) ->
            ([y] @ xs |> List.exists Variable.isSolvable) &&
            // maybe only when > 1 is unrestricted instead of all
            ([y] @ xs |> List.forall Variable.isUnrestricted |> not)


    let check e = 
        let issub op (y : Variable) (xs : Variable list) =
            xs
            |> function
            | [] -> true
            | _  ->
                if y.Values |> ValueRange.isValueSet &&
                   xs |> List.map Variable.getValueRange
                      |> List.forall ValueRange.isValueSet then
                
                    y.Values
                    |> ValueRange.isSubSetOf (xs |> List.reduce (op)).Values

                else true

        if e |> isSolvable then
            e
            |> function
            | ProductEquation (y, xs) ->
                xs 
                |> issub (^*) y
            | SumEquation (y, xs) ->
                xs 
                |> issub (^+) y

        else true


    /// Solve an equation **e**, return a list of
    /// changed `Variable`s. 
    /// ToDo change this to be more consistent with mutable values
    let solve log eq =
        eq
        |> Events.EquationStartedSolving
        |> Logging.logInfo log

        let runOnce y xs =
            let c1 =
                y::xs 
                |> List.filter (Variable.getValueRange >> ValueRange.isValueSet)
                |> List.length
            let c2 =  (y::xs |> List.length)
            
            (c2 - c1 <= 1) 

        if eq |> isSolved then 
            []

        else
            let rec calc changed op1 op2 y xs rest =
                (y::xs)
                |> Events.EquationStartedCalculation
                |> Logging.logInfo log

                match rest with 
                | []  -> 
                    (changed, xs)
                    |> Events.EquationFinishedCalculation
                    |> Logging.logInfo log

                    changed, xs
                | x::tail ->
                    let xs'  = xs |> List.filter ((<>) x)

                    let x' =
                        match xs' with
                        | [] -> x <== y 
                        | _  -> x <== (y |> op2 <| (xs' |> List.reduce op1))

                    let changed = 
                        if x = x' then changed 
                        else
                            x'
                            |> Events.EquationVariableChanged
                            |> Logging.logInfo log
                            
                            changed 
                            |> List.replaceOrAdd (Variable.eqName x') x'

                    tail |> calc changed op1 op2 y (x'::xs')

            let rec loop b op1 op2 y xs changed =
                let x   = xs |> List.head
                let xs' = xs |> List.filter ((<>) x)
            
                // op1 = (*) or (+) and op2 = (/) or (-)
                // Calculate y = x1 op1 x2 op1 .. op1 xn
                let ychanged, y' = calc [] op1 op1 x xs' [y]
            
                // Replace y with the new y with is in a list
                let y = y' |> List.head
            
                // Calculate x1 = y op2 (x2 op1 x3 .. op1 xn)
                //       and x2 = y op2 (x1 op1 x3 .. op1 xn)
                //       etc..
                let xchanged, xs = calc [] op1 op2 y xs xs

                // If something has changed restart until nothing changes anymore
                // or only has to run once
                match ychanged @ xchanged with
                | [] ->
                    changed
                    |> Events.EquationFinishedSolving
                    |> Logging.logInfo  log

                    changed
                | _  ->
                    ychanged @ xchanged
                    |> List.fold (fun acc v ->  
                        acc |> List.replaceOrAdd (Variable.eqName v) v
                    ) changed
                    |> fun changed ->
                        // only run once so now is ready
                        if b then changed
                        else
                            (b, y, xs, changed)
                            |> Events.EquationLoopedSolving
                            |> Logging.logInfo log

                            let b = runOnce y xs
                            loop b op1 op2 y xs changed
            
            let b, y, xs, op1, op2 =
                match eq with
                | ProductEquation (y, xs) -> y, xs, (^*), (^/)
                | SumEquation     (y, xs) -> y, xs, (^+), (^-)
                |> fun (y, xs, op1, op2) ->
                    // run only once when all but one is a value set
                    runOnce y xs, y, xs, op1, op2
                        
            match xs with 
            | [] -> []
            | _  ->
                try 
                    loop b op1 op2 y xs  []
                with
                | Variable.Exceptions.VariableException m -> 
                    m 
                    |> Logging.logError log

                    eq
                    |> Events.EquationCouldNotBeSolved
                    |> Logging.logWarning log

                    m 
                    |> Variable.Exceptions.raiseExc


    module Dto =

        type VariableDto = Variable.Dto.Dto

        /// `Dto` for an `Equation`
        type Dto = { Vars: VariableDto[]; IsProdEq: bool }

        /// Create a `Dto` with `vars` (variable dto array)
        /// that is either a `ProductEquation` or a `SumEquation`
        let create isProd vars  = { Vars = vars; IsProdEq = isProd }

        /// Create a `ProductEquation` `Dto`
        let createProd = create true

        /// Create a `SumEquation` `Dto`
        let createSum  = create false

        /// Return the `string` representation of a `Dto`
        let toString exact (dto: Dto) = 
            let op = if dto.IsProdEq then "*" else "+"
            let varToString = Variable.Dto.toString exact

            match dto.Vars |> Array.toList with
            | [] -> ""
            | _::[] -> ""
            | y::xs -> 
                let s = 
                    sprintf "%s = " (y |> varToString) + 
                    (xs |> List.fold (fun s v -> s + (v |> varToString) + " " + op + " ") "")
                s.Substring(0, s.Length - 2)


        /// Create a `Dto` and raise an exception if it fails
        let fromDto dto =
            let succ = id
            let fail = Exception.raiseExc

            match dto.Vars |> Array.toList with
            | [] -> Exceptions.EquationEmptyVariableList |> fail
            | y::xs ->
                let y = y |> Variable.Dto.fromDto
                let e = (y, xs |> List.map Variable.Dto.fromDto)
            
                if dto.IsProdEq then 
                    e 
                    |> createProductEq succ fail
                else 
                    e 
                    |> createSumEq succ fail

        /// Create a `Dto` from an `Equation` **e**
        let toDto e =
            let c isProd y xs =
                { Vars = y::xs |> List.map Variable.Dto.toDto |> List.toArray; IsProdEq = isProd }
            
            let fp = c true 
            let fs = c false 
            
            e |> apply fp fs



/// Implementations of solvers for product equations
/// sum equations and a set of product and/or sum
/// equations
module Solver =
    
    module EQD = Equation.Dto

    open Types
    
    module Exception =

        /// Equation exception
        exception SolverException of Exceptions.Message

        /// Raise an `EquationException` with `Message` `m`.
        let raiseExc m = m |> SolverException |> raise


    /// Format a set of equations to print.
    /// Using **f** to allow additional processing
    /// of the string.
    let printEqs exact pf eqs = 
        let eqs = 
            eqs 
            |> List.sortBy (fun e ->
                e 
                |> Equation.toVars
                |> List.head
                |> Variable.getName)

        "equations result:\n" |> pf
        eqs
        |> List.map (Equation.toString exact)
        |> List.iteri (fun i s ->
            s
            |> sprintf "%i.\t%s" i
            |> pf
        )
//        sprintf "raw: \n%A" eqs |> pf
        "-----" |> pf 

        eqs    
    

    /// Checks whether a list of `Equation` **eqs**
    /// contains an `Equation` **eq**
    let contains eq eqs = eqs |> List.exists ((=) eq)

    /// The `Result` of solving an `Equation`
    /// is that either the `Equation` is the 
    /// same or has `Changed`.
    type Result =
        | UnChanged
        | Changed   of Variable list


    /// Replace a list of `Variable` **vs**
    /// in a list of `Equation` **es**, return
    /// a list of replaced `Equation` and a list
    /// of unchanged `Equation`
    let replace vars es =
        let rpl, rst = 
            es 
            |> List.partition (fun e -> 
                vars 
                |> List.exists (fun v -> 
                    e 
                    |> Equation.contains v
                )
            )

        vars 
        |> List.fold (fun acc v -> 
            acc 
            |> List.map (Equation.replace v)
        ) rpl
        , rst

    /// Solve the equation `e` and return 
    /// the set of equations `es` it belongs 
    /// to either as `Changed` or `Unchanged`
    let solveEquation log e = 

        let changed = 
            e 
            |> Equation.solve log

        if changed |> List.length > 0 then 
            changed |> Changed 
        else UnChanged


    let memSolve f =
        let cache = ref Map.empty
        fun e ->
            match (!cache).TryFind(e) with
            | Some r -> r
            | None ->
                let r = f e
                cache := (!cache).Add(e, r)
                r

    let sortQue que =
        if que |> List.length = 0 then que
        else
            que 
            |> List.sortBy Equation.countProduct

        
    /// Create the equation solver using a 
    /// product equation and a sum equation solver
    /// and function to determine whether an 
    /// equation is solved
    let solve log sortQue vr eqs =

        let solveE = solveEquation log

        let rec loop n que acc =
            que
            |> Events.SolverLoopedQue
            |> Logging.logInfo log

            match que with
            | [] -> 
                match acc |> List.filter (Equation.check >> not) with
                | []   -> acc
                | invalid -> 
                    let msg =
                        invalid
                        |> Exceptions.SolverInvalidEquations
                    
                    msg |> Logging.logError log
                    msg |> Exception.raiseExc
                
            | eq::tail ->
                // If the equation is already solved, or not solvable 
                // just put it to  the accumulated equations and go on with the rest
                if eq |> Equation.isSolvable |> not then
                    [ eq ] 
                    |> List.append acc
                    |> loop (n + 1) tail

                // Else go solve the equation
                else
                    match eq |> solveE with
                    // Equation is changed, so every other equation can 
                    // be changed as well (if changed vars are in the other
                    // equations) so start new
                    | Changed vars ->

                        let eq = [ eq ] |> replace vars |> fst

                        // find all eqs with vars in acc and put these back on que
                        acc
                        |> replace vars
                        |> function
                        | (rpl, rst) ->
                            // replace vars in tail
                            let que = 
                                tail
                                |> replace vars
                                |> function 
                                | (es1, es2) ->
                                    es1
                                    |> List.append es2
                                    |> List.append rpl

                            rst
                            |> List.append eq
                            |> loop (n + 1) que 

                    // Equation did not in fact change, so put it to
                    // the accumulated equations and go on with the rest
                    | UnChanged ->
                        [eq] 
                        |> List.append acc
                        |> loop (n + 1) tail

        eqs 
        |> replace [vr]
        |> function 
        | (rpl, rst) -> loop 0 (rpl |> sortQue) rst
            
            
    


module Props =

    open Types

    module ValueRange = Variable.ValueRange

    let matchProp p =

        match p with
        | ValsProp vs -> 
            vs
            |> ValueRange.ValueSet
        | _ ->
            match p with
            | ValsProp _ -> "all ready matched" |> failwith
//            | IncrProp vs -> vs |> ValueRange.createMinIncrRange
            | MinInclProp v -> v |> ValueRange.createMinRange true 
            | MinExclProp v -> v |> ValueRange.createMinRange false 
            | MaxInclProp v -> v |> ValueRange.createMaxRange true 
            | MaxExclProp v -> v |> ValueRange.createMaxRange false 


    let getMin = function
    | MinInclProp v | MinExclProp v -> v |> Some
    | _ -> None


    let getMax = function
    | MaxInclProp v | MaxExclProp v -> v |> Some
    | _ -> None

    // let getIncr = function
    // | IncrProp vs -> vs |> Some
    // | _ -> None



module Constraint =

    open Types

    module ValueRange = Variable.ValueRange
    module Name = Variable.Name

    let eqsName (c1 : Constraint) (c2 : Constraint) = c1.Name = c2.Name  


    let toString { Name = n; Property = p; Limit = l} =
        sprintf "%s: %A %A" (n |> Name.toString) p l


    let scoreConstraint c =
            match c.Property with
            | ValsProp vs -> 
                let n = vs |> Set.count
                if n = 1 then    -3, c
                else              n, c
            | MinInclProp _
            | MinExclProp _   -> -5, c
//            | IncrProp _      -> -4, c
            | _               -> -2, c


    let orderConstraints log cs =
        cs
        // calc min and max from valsprop constraints
        |> List.fold (fun acc c ->
            match c.Property with
            | ValsProp vs ->
                if vs |> Set.count <= 1 then [c] |> List.append acc
                else
                    let min = vs |> Set.minElement |> MinInclProp
                    let max = vs |> Set.maxElement |> MaxInclProp
                    [
                        c
                        { c with Property = min ; Limit = NoLimit }
                        { c with Property = max ; Limit = NoLimit }
                    ]
                    |> List.append acc
            | _ -> [c] |> List.append acc
        ) []
        // clean up list of constraints
        // |> List.fold (fun acc c ->
        //     if acc |> List.exists ((=) c) then acc
        //     else c::acc
        //         // match acc |> List.tryFind (fun x -> c.Name = x.Name) with
        //         // | None    -> c::acc
        //         // | Some c' ->
        //         //     match c'.Property with
        //         //     | ValsProp n when n |> Set.count = 1 ->
        //         //         acc
        //         //         |> List.fold (fun acc x ->
        //         //             if x.Name = c'.Name then 
        //         //                 if acc |> List.exists ((=) c') then acc
        //         //                 else c'::acc
        //         //             else x::acc
        //         //         ) []
        //         //     | _ -> c::acc
        // ) []
        |> List.fold (fun acc c ->
            if acc |> List.exists ((=) c) then acc
            else
                acc @ [c]
        ) []
        |> fun cs -> cs |> List.map scoreConstraint
        |> List.sortBy fst
        |> fun cs ->
            cs
            |> Events.ConstraintSortOrder
            |> Logging.logInfo log

            cs
            |> List.map snd 


    let apply log sortQue (c : Constraint) eqs =

        let lim l b vr =
            if vr |> Variable.count <= l then vr
            else
                vr
                |> Variable.getValueRange
                |> ValueRange.getValueSet
                |> function
                | Some vs ->
                    vs
                    |> Set.toList
                    |> fun xs -> 
                        if b then xs |> List.sort 
                        else xs |> List.sortDescending
                    |> List.take l
                    |> Set.ofList
                    |> ValueRange.ValueSet.create
                    |> Variable.setValueRange vr
                | None -> vr

        eqs 
        |> List.collect (Equation.findName c.Name)
        |> function
        | [] -> 
            (c, eqs)
            |> Events.ConstraintVariableNotFound
            |> Logging.logWarning log

            None

        | vr::_ ->

            c.Property
            |> Props.matchProp
            |> Variable.setValueRange vr
            |> fun vr ->
                match c.Limit with
                | NoLimit -> vr
                | MaxLim l -> 
                    (c.Limit, vr)
                    |> Events.ConstraintLimitSetToVariable
                    |> Logging.logInfo log

                    vr |> lim l false  
                | MinLim l -> 
                    (c.Limit, vr)
                    |> Events.ConstraintLimitSetToVariable
                    |> Logging.logInfo log

                    vr |> lim l true

                // ToDo implement min max limit
                | _ -> vr
            |> Some
        |> function
        | None -> eqs
        | Some vr ->
            (c, vr)
            |> Events.ConstraintVariableApplied
            |> Logging.logInfo log

            eqs 
            |> Solver.solve log sortQue vr
            |> fun eqs ->
                (c, eqs)
                |> Events.ConstrainedEquationsSolved
                |> Logging.logInfo log

                eqs




/// Public funtions to use the library
module Api =

    open System

    open Informedica.Utils.Lib.BCL

    open Types

    module VRD = Variable.Dto
    module EQD = Equation.Dto
    
    module ValueRange = Variable.ValueRange 
    module Name = Variable.Name


    /// Initialize the solver returning a set of equations
    let init eqs = 
        let notempty = String.IsNullOrWhiteSpace >> not
        let prodEqs, sumEqs = eqs |> List.partition (String.contains "*")
        let createProdEqs = List.map (EQD.createProd >> EQD.fromDto)
        let createSumEqs  = List.map (EQD.createSum  >> EQD.fromDto)

        let parse eqs op = 
            eqs 
            |> List.map (String.splitAt '=')
            |> List.map (Array.collect (String.splitAt op))
            |> List.map (Array.map String.trim)
            |> List.map (Array.filter notempty)
            |> List.map (Array.map VRD.createNew)
        
        (parse prodEqs '*' |> createProdEqs) @ (parse sumEqs '+' |> createSumEqs)


    let setVariableValues lim n p eqs =

        eqs 
        |> List.collect (Equation.findName n)
        |> function
        | [] -> None

        | vr::_ ->

            p
            |> Props.matchProp
            |> Variable.setValueRange vr
            |> fun vr ->
                match lim with
                | Some l ->
                    if vr |> Variable.count > l then
                        vr
                        |> Variable.getValueRange
                        |> ValueRange.getValueSet
                        |> function
                        | Some vs -> 
                            vs 
                            |> Seq.sort 
                            |> Seq.take l 
                            |> ValueRange.ValueSet.create
                            |> Variable.setValueRange vr
                        | None -> vr

                    else vr
                | None -> vr
                |> Some




    /// Solve an `Equations` list with
    ///
    /// * f: function used to process string message
    /// * n: the name of the variable to be updated
    /// * p: the property of the variable to be updated
    /// * vs: the values to update the property of the variable
    /// * eqs: the list of equations to solve
    let solve sortQue log lim n p eqs =

        eqs 
        |> setVariableValues lim n p
        |> function
        | None -> eqs
        | Some vr -> 
            (vr, eqs)
            |> Events.ApiSetVariable
            |> Logging.logInfo log
                        
            eqs 
            |> Solver.solve log sortQue vr
            |> fun eqs ->
                eqs
                |> Events.ApiEquationsSolved
                |> Logging.logInfo log

                eqs


    /// Make a list of `EQD`
    /// to contain only positive
    /// values as solutions
    let nonZeroNegative eqs =
        eqs 
        |> List.map Equation.nonZeroOrNegative


    let solveConstraints log cs eqs = 
        let apply = 
            fun c eqs ->
                try
                    Constraint.apply log Solver.sortQue c eqs
                with
                | Variable.Exceptions.VariableException m -> 
                    m
                    |> Logging.logError log

                    m 
                    |> Variable.Exceptions.raiseExc
                | e -> 
                    e |> raise

        cs
        |> Constraint.orderConstraints log
        |> List.fold (fun acc c ->
            acc
            |> apply c
        ) eqs
        |> fun eqs ->
            (cs, eqs)
            |> Events.ApiAppliedConstraints
            |> Logging.logInfo log

            eqs
        


module SolverLogging =
    
    open Informedica.Utils.Lib.BCL

    open Types
    open Types.Logging
    open Types.Events

    module Name = Variable.Name
    module ValueRange = Variable.ValueRange

    let printException = function
    | Exceptions.ValueRangeEmptyValueSet -> 
        "ValueRange cannot have an empty value set"
    | Exceptions.EquationEmptyVariableList -> 
        "An equation should at least contain one variable"
    | Exceptions.SolverInvalidEquations eqs ->
        eqs
        |> List.map (Equation.toString true)
        |> String.concat "\n"
        |> sprintf "The following equations are invalid\n%s"
    | Exceptions.ValueRangeMinLargerThanMax (min, max) ->
        sprintf "%A is larger than %A" min max
    | Exceptions.ValueRangeNotAValidOperator ->
        sprintf "The value range operator was invalid or unknown"
    | Exceptions.EquationDuplicateVariables vrs ->
        vrs
        |> List.map (Variable.getName >> Name.toString)
        |> String.concat (", ")
        |> sprintf "The list of variables for the equation contains duplicates:\n%s"
    | Exceptions.NameLongerThan1000 s ->
        sprintf "This name contains more than 1000 chars: %s" s
    | Exceptions.NameNullOrWhiteSpaceException ->
        sprintf "A name cannot be a blank string"
    | Exceptions.VariableCannotSetValueRange (var, vlr) ->
        sprintf "This variable:\n%s\ncannot be set with this range:%s\n"
            (var |> Variable.toString true)
            (vlr |> ValueRange.toString true)


    let printMsg = function
    | ExceptionMessage m ->
        m 
        |> printException
    | SolverMessage m ->
        match m with
        | EquationCouldNotBeSolved eq -> 
            eq
            |> Equation.toString true
            |> sprintf "=== Cannot solve Equation ===\n%s" 
        | EquationStartedCalculation vars -> ""
        | EquationStartedSolving eq -> 
            eq
            |> Equation.toString true
            |> sprintf "=== Start solving Equation ===\n%s"
        | EquationFinishedCalculation (changed, vars) -> 
            changed
            |> List.map (Variable.getName >> Name.toString)
            |> String.concat ", "
            |> fun s -> 
                if s |> String.isNullOrWhiteSpace then "No changed vars"
                else s
            |> sprintf "=== Equation finished calculation ===\n%s"
        | EquationVariableChanged var -> 
            var
            |> Variable.toString true
            |> sprintf "=== Equation Variable changed ===\n%s"
        | EquationFinishedSolving vars -> ""
        | EquationLoopedSolving (b, var, changed, vars) -> 
            "=== Equation loop solving"
        | SolverLoopedQue eqs -> ""
        | ConstraintSortOrder cs -> 
            cs
            |> List.map (fun (i, c) ->
                c
                |> Constraint.toString
                |> sprintf "%i: %s" i
            )
            |> String.concat "\n"
            |> sprintf "=== Constraint sort order ===\n%s"
        | ConstraintVariableNotFound (c, eqs) -> 
            c
            |> sprintf "Constraint %A cannot be set"
            |> (fun s -> 
                eqs
                |> List.map (Equation.toString true)
                |> String.concat "\n"
                |> sprintf "%s\In equations:\%s" s
            )
            |> sprintf "=== Constraint Variable not found ===\n%s"
        | ConstraintLimitSetToVariable (l, var) -> ""
        | ConstraintVariableApplied (c, var) -> 
            c
            |> Constraint.toString
            |> fun s -> 
                var
                |> Variable.getName
                |> Name.toString
                |> sprintf "%s apply to %s" s
            |> sprintf "=== Constraint apply Variable ===\n%s"
        | ConstrainedEquationsSolved (c, eqs) -> 
            c 
            |> Constraint.toString
            |> fun s ->
                eqs
                |> List.sort
                |> List.map (Equation.toString true)
                |> String.concat "\n"
                |> sprintf "Constraint: %s applied to\n%s" s
            |> sprintf "=== Equations solved ===\n%s"
        | ApiSetVariable (var, eqs) -> ""
        | ApiEquationsSolved eqs -> ""
        | ApiAppliedConstraints (cs, eqs) -> ""


    let logger f =
        {
            Log = 
                fun { TimeStamp = _; Level = _; Message = msg } ->
                    match msg with
                    | :? Logging.SolverMessage as m ->
                        m 
                        |> printMsg
                        |> f
                    | _ -> 
                        msg
                        |> sprintf "cannot print msg: %A" 
                        |> f
                        
        }
