
#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "nuget: Unquote"


#r "../../Informedica.Utils.Lib/bin/Debug/net5.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.GenUnits.Lib/bin/Debug/net5.0/Informedica.GenUnits.Lib.dll"


#time



/// Create the necessary test generators
module Generators =

    open FsCheck
    open MathNet.Numerics
    open Expecto


    let bigRGen (n, d) =
    let bigRGen (n, d) =
    let bigRGen (n, d) =
        let d = if d = 0 then 1 else d
        let n' = abs (n) |> BigRational.FromInt
        let d' = abs (d) |> BigRational.FromInt
        n' / d'
    let bigRGenerator =
        gen {
            let! n = Arb.generate<int>
            let! d = Arb.generate<int>
            return bigRGen (n, d)
            return bigRGen (n, d)
            return bigRGen (n, d)

    type BigRGenerator() =
    type BigRGenerator() =
    type BigRGenerator() =
        static member BigRational() =
            }


    let config =
    let config =
    let config =
        { FsCheckConfig.defaultConfig with
            arbitrary = [ typeof<BigRGenerator> ]


    let testProp testName prop =
        prop |> testPropertyWithConfig config testName
        prop |> testPropertyWithConfig config testName
        prop |> testPropertyWithConfig config testName
    let run =
        runTestsWithCLIArgs [] [| "--summary" |]
    let run =
        runTestsWithCLIArgs [] [| "--summary" |]

[<AutoOpen>]
module Utils =

    /// Helper functions for `BigRational`
    module BigRational =
    module BigRational =
    module BigRational =




        /// ToDo: doesn't return `NoOp` but fails,
        ///
        ///
        /// or subtraction, returns `NoOp` when
        /// the operation is neither.
        let (|Mult|Div|Add|Subtr|) op =
            match op with
            | _ when op |> BigRational.opIsMult -> Mult
            | _ when op |> BigRational.opIsMult -> Mult
            | _ when op |> BigRational.opIsMult -> Mult
            | _ when op |> BigRational.opIsDiv -> Div
            | _ when op |> BigRational.opIsAdd -> Add


        let private toMultipleOf b d n =
        let private toMultipleOf b d n =
        let private toMultipleOf b d n =
            if d = 0N then
                n
            else
                let m =
                    (n / d)
                    |> BigRational.ToBigInt
                    |> BigRational.FromBigInt

                    (n / d)
                    if m * d < n then
                        (m + 1N) * d
                    else
                        m * d
                else if m * d > n then
                    (m - 1N) * d
                else
                    m * d
                    (n / d)
                    if m * d < n then
                        (m + 1N) * d
                    else
                        m * d
                else if m * d > n then
                    (m - 1N) * d

                    m * d
                if b then
            |> Set.fold
                (fun (b, acc) i ->
                    let ec = if isMax then (>=) else (<=)
                    let nc = if isMax then (>) else (<)
                    let ad = if isMax then (-) else (+)

                    let mult =
                        if isMax then
                            m |> toMaxMultipleOf i
                        else
                            m |> toMinMultipleOf i

                    let mult =
                        if (isIncl |> not) && (mult |> ec <| m) then
                            (mult |> ad <| i)
                        else
                            mult
                else if m * d > n then
                    match acc with
                    | Some a ->
                        // need to preserve isIncl, i.e. `b` if nothing happened
                        if (mult |> nc <| a) then
                            (true, Some mult)
                        else
                            (b, Some a)
                    | None -> (true, Some mult)
                )
                (isIncl, None)

                    let mult =
                        if (isIncl |> not) && (mult |> ec <| m) then
        let maxInclMultipleOf =
            calcMinOrMaxToMultiple true true
                        else
                            mult
        let maxExclMultipleOf =
            calcMinOrMaxToMultiple true false
                    match acc with
                    | Some a ->
        let minInclMultipleOf =
            calcMinOrMaxToMultiple false true
                        if (mult |> nc <| a) then
                            (true, Some mult)
        let minExclMultipleOf =
            calcMinOrMaxToMultiple false false
                            (b, Some a)
                    | None -> (true, Some mult)
                )
                (isIncl, None)

                    let mult =
                        if (isIncl |> not) && (mult |> ec <| m) then
        let maxInclMultipleOf =
            calcMinOrMaxToMultiple true true
                        else
            let tests =
                testList
                    "bigrational"
                    [


                        testList
                            "multiples"
                            [
                                fun incr min ->
                                    let mult = min |> toMinMultipleOf incr

                                    if mult >= min then
                                        true
                                    else
                                        printfn $"||{mult} >= {min}?||"
                                        false
                                |> Generators.testProp
                                    $"multiple of incr should be >= min"

                                fun incr min ->
                                    let mult =
                                        min |> minInclMultipleOf incr |> snd

                                    if mult >= min then
                                        true
                                    else
                                        printfn $"||{mult} >= {min}?||"
                                        false
                                |> Generators.testProp
                                    "multiple incrs should be >= min incl"
        let maxExclMultipleOf =
                                fun incr min ->
                                    let mult =
                                        min |> minExclMultipleOf incr |> snd
            calcMinOrMaxToMultiple true false
                                    if incr |> Set.count = 0 || mult > min then
                                        true
                                    else
                                        printfn $"||{mult} > {min}?||"
                                        false
                                |> Generators.testProp
                                    "multiple incrs should be > min excl"

                                fun incr max ->
                                    let mult = max |> toMaxMultipleOf incr
                            (b, Some a)
                                    if mult <= max then
                                        true
                                    else
                                        printfn $"||{mult} <= {max}?||"
                                        false
                                |> Generators.testProp
                                    $"multiple of incr should be <= max"
            calcMinOrMaxToMultiple true true
                                fun incr max ->
                                    let mult =
                                        max |> maxInclMultipleOf incr |> snd
                        testList
                                    if mult <= max then
                                        true
                                    else
                                        printfn $"||{mult} <= {max}?||"
                                        false
                                |> Generators.testProp
                                    "multiple incrs should be <= max incl"
                                    else
                                fun incr max ->
                                    let mult =
                                        max |> maxExclMultipleOf incr |> snd
                                        min |> minInclMultipleOf incr |> snd
                                    if incr |> Set.count = 0 || mult < max then
                                        true
                                    else
                                        printfn $"||{mult} < {max}?||"
                                        false
                                |> Generators.testProp
                                    "multiple incrs should be < max excl"
                                    "multiple incrs should be >= min incl"
                            ]
                    ]
                                    let mult =
            let run () = tests |> Generators.run
                                        true
                                    else
                                        printfn $"||{mult} > {min}?||"
                                        false
                                |> Generators.testProp
                                    "multiple incrs should be > min excl"

                                fun incr max ->
                                    let mult = max |> toMaxMultipleOf incr

                                    if mult <= max then
                                        true
                                    else
                                        printfn $"||{mult} <= {max}?||"
                                        false
                                |> Generators.testProp
                                    $"multiple of incr should be <= max"

                                fun incr max ->
                                    let mult =
                                        max |> maxInclMultipleOf incr |> snd
                        testList
                                    if mult <= max then
            xs |> List.map (fun a -> if pred a then x else a)
                                        printfn $"||{mult} <= {max}?||"
                                        false
                                |> Generators.testProp
                                    "multiple incrs should be <= max incl"
                                    else
                                fun incr max ->
                                    let mult =
                                        max |> maxExclMultipleOf incr |> snd
                                        min |> minInclMultipleOf incr |> snd
                                    if incr |> Set.count = 0 || mult < max then
                                        true
                                    else
                                        printfn $"||{mult} < {max}?||"
                                        false
                                |> Generators.testProp
                                    "multiple incrs should be < max excl"

                            ]
                    ]
                                    let mult =
            let run () = tests |> Generators.run
                                        true
                                    else
                                        printfn $"||{mult} > {min}?||"
                                        false
                                |> Generators.testProp
                                    "multiple incrs should be > min excl"
            let tests =
                testList
                    "list"
                    [
                        testList
                            "replace"
                            [
                                fun xs ->
                                    let isEven x = x % 2 = 0
                                    let even = xs |> replace isEven 2

                                    (xs |> List.filter isEven |> List.length) = (even
                                                                                 |> List
                                                                                     .filter (
                                                                                         (=)
                                                                                             2
                                                                                     )
                                                                                 |> List.length)
                                |> testProperty
                                    "replace count should equel predicate count"
                            ]
                                |> Generators.testProp

            let run () = tests |> Generators.run

                                    if mult <= max then
            xs |> List.map (fun a -> if pred a then x else a)
                                        printfn $"||{mult} <= {max}?||"
                                        false
                                |> Generators.testProp
                                    "multiple incrs should be <= max incl"

                                fun incr max ->
                                    let mult =
                                        max |> maxExclMultipleOf incr |> snd

                                    if incr |> Set.count = 0 || mult < max then
                                        true
                                    else
                                        printfn $"||{mult} < {max}?||"
                                        false
                                |> Generators.testProp
                                    "multiple incrs should be < max excl"

                            ]
                    ]

            let run () = tests |> Generators.run


    /// Helper functions for `Option`
    module Option =

        let none _ = None
            let tests =
                testList
                    "list"
                    [
                        testList
                            "replace"
                            [
                                fun xs ->
                                    let isEven x = x % 2 = 0
                                    let even = xs |> replace isEven 2

                                    (xs |> List.filter isEven |> List.length) = (even
                                                                                 |> List
                                                                                     .filter (
                                                                                         (=)
                                                                                             2
                                                                                     )
                                                                                 |> List.length)
                                |> testProperty
                                    "replace count should equel predicate count"
                            ]

    /// Helper functions for `List`
            let run () = tests |> Generators.run
        /// when the `pred` function returns `true`.
        let replace pred x xs =
            xs |> List.map (fun a -> if pred a then x else a)


        let distinct xs =
            xs |> Seq.ofList |> Seq.distinct |> Seq.toList

        /// Replace an element using a predicate
        /// If element doesn't exist, add the element
        let replaceOrAdd pred x xs =
            if xs |> List.exists pred then
                xs |> replace pred x
            else
                x :: xs



        module Tests =

            open Expecto
            open Expecto.Flip


            let testReplace () =
                [ 1..10 ] |> replace (fun x -> x % 2 = 0) 0

            let testReplaceOrAdd () = [ 1..10 ] |> replaceOrAdd ((=) 11) 0

            [<Tests>]
            let tests =
                testList
                    "list"
                    [
                        testList
                            "replace"
                            [
                                fun xs ->
                                    let isEven x = x % 2 = 0
                                    let even = xs |> replace isEven 2

                                    (xs |> List.filter isEven |> List.length) = (even
                                                                                 |> List
                                                                                     .filter (
                                                                                         (=)

    and Change = Change of change: Property * variable: Variable
                                                                                     )
                                                                                 |> List.length)
                                |> testProperty
                                    "replace count should equel predicate count"
                            ]
                    ]

            let run () = tests |> Generators.run



module Types =

    open System
    open MathNet.Numerics

    /// Represents a non empty/null string identifying a `Variable`.
    /// `Name` can be no longer than 1000 characters and cannot be
    /// a null string
    type Name = Name of string


    /// The minimal value in
    /// a `ValueRange`. Can be inclusive
    /// or exclusive.
    type Minimum =
        | MinIncl of BigRational
        | MinExcl of BigRational


    /// The maximum value in
    /// a `ValueRange`. Can be inclusive
    /// or exclusive.
    type Maximum =
        | MaxIncl of BigRational
        | MaxExcl of BigRational


    type ValueSet = BigRational Set


    type Increment = BigRational Set


    /// `ValueRange` represents a domain of
    /// rational numbers. A `ValueRange` can be either
    /// - `Unrestricted`: any rational number
    /// - `Increment`: any number that is a multiple of an increment
    /// - `Min`: have a minimum
    /// - `MinIncrement`: a minimum with the domain consisting of multiples of one increment
    /// - `Max`: have a maximum
    /// - `IncrementMax`: a domain of multiples of an increment with a maximum
    /// - `MinMax`: have both a minimum and maximum

    and Change = Change of change: Property * variable: Variable
    type ValueRange =
        | Unrestricted
        | Min of Minimum
        | Max of Maximum
        | MinMax of Minimum * Maximum
        | Incr of Increment
        | MinIncr of min: Minimum * incr: Increment
        | IncrMax of incr: Increment * max: Maximum
        | ValueSet of ValueSet // Set<BigRational>


    /// Represents a variable in an
    /// `Equation`. The variable is
    /// identified by `Name` and has
    /// a `Values` described by the
    /// `ValueRange`.
    type Variable = { Name: Name; Values: ValueRange }


    /// An equation is either a `ProductEquation`
    /// or a `Sumequation`, the first variable is the
    /// dependent variable, i.e. the result of the
    /// equation, the second part are the independent
    /// variables in the equation
    type Equation =
        | ProductEquation of Variable * Variable list
        | SumEquation of Variable * Variable list



    /// Represents a property of a `Variable`.
    ///
    /// - `MinIncl`: An inclusive minimum
    /// - `MinExcl`: An exclusive minimum
    /// - `MaxIncl`: An inclusive maximum
    /// - `MaxExcl`: An exclusive maximum
    /// - `RangeProp`: A delta with multiples
    /// - `Vals`: A set of distinct values
    type Property =
        | IncrementProp of Increment
        | MinInclProp of BigRational
        | MinExclProp of BigRational
        | MaxInclProp of BigRational
        | MaxExclProp of BigRational
        | ValsProp of BigRational Set


    /// The `Result` of solving an `Equation`
    /// is that either the `Equation` is the
    /// same or has `Changed`.
    type Result =
        | UnChanged
        | Changed of Change list

    and Change = Change of change: Property * variable: Variable


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
            Name: Name
            Property: Property
            Limit: Limit
        }


    module Events =

        type Event =
            | EquationCouldNotBeSolved of Equation
            | EquationStartedCalculation of Variable list
            | EquationStartedSolving of Equation
            | EquationFinishedCalculation of Variable list * Variable list
            | EquationVariableChanged of Variable
            | EquationFinishedSolving of Variable list
            | EquationLoopedSolving of
                bool *
                Variable *
                Variable list *
                Variable list
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
            | SolverLooped of Equation list

    module Logging =


        type IMessage =
            interface
            end

        type TimeStamp = DateTime


        type Level =
            | Informative
            | Debug
        let createExc =
            create id Exceptions.raiseExc
            | Error


        type SolverMessage =
            | ExceptionMessage of Exceptions.Message
            | SolverMessage of Events.Event
            interface IMessage


        type Message =
            {
                TimeStamp: TimeStamp
                Level: Level
                Message: IMessage
            }


        type Logger = { Log: Message -> unit }



module Logging =


    open System
    open Types.Logging


    let private create l e =
        {
            TimeStamp = DateTime.Now
            Level = l
            Message = e
        }


    let logMessage level (logger: Logger) evt =
        evt |> SolverMessage |> create level |> logger.Log


    let logInfo logger msg = logMessage Informative logger msg


    let logWarning logger msg = logMessage Warning logger msg


    let logError (logger: Logger) msg =
        msg
            let minGTEmin min1 min2 = min1 = min2 || minGTmin min1 min2
        |> logger.Log



            let minSTmin min1 min2 = min2 |> minGTEmin min1 |> not
//// The notation for this is:
/// - <..>   : meaning a variable can be any rational number
/// - [0N..> : meaning that the variable can be any number larger than or equal to 0N
/// - <0N..> : meaning that the variable can be any number larger than but excluding 0N
/// - [0N..10N> : meaning that the variable must be between 0N up to but excluding 10N
/// - [0N..10N] : meaning that the variable can be 0N up to and including 10N
/// - [1N,3N,4N,5N] : meaning that the variable can only be one of the numbers
///
/// - `Name` : A `Variable` is identified by its `Name`
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
            if s |> String.IsNullOrWhiteSpace then
                Exceptions.NullOrWhiteSpaceException |> fail
            else
                match s |> String.trim with
                let tests =
                    testList
                        "minimum"
                        [

                            fun b m1 m2 ->
                                let min1 = createMin b m1
                                let min2 = createMin b m2
                                m1 > m2 = (min1 |> minGTmin min2)
                            |> Generators.testProp "min1 > min2"
                | n -> n |> Exceptions.LongerThan1000 |> fail
                            fun b m1 m2 ->
                                let min1 = createMin b m1
                                let min2 = createMin b m2
                                m1 < m2 = (min1 |> minSTmin min2)
                            |> Generators.testProp "min1 < min2"
        /// an `NameException` when it fails.
                            fun m1 m2 ->
                                let min1 = createMin true m1
                                let min2 = createMin false m2
                                (m1 = m2 || m1 < m2) = (min1 |> minSTmin min2)
                            |> Generators.testProp
                                "min1 incl < min2 excl, also when min1 = min2"

                            fun m1 m2 ->
                                let min1 = createMin false m1
                                let min2 = createMin true m2
                                m1 < m2 = (min1 |> minSTmin min2)
                            |> Generators.testProp "min1 excl < min2 incl"

                            fun b m1 m2 ->
                                let min1 = createMin b m1
                                let min2 = createMin b m2
                                m1 >= m2 = (min1 |> minGTEmin min2)
                            |> Generators.testProp "min1 >= min2"
            exception ValueRangeException of Exceptions.Message
                            fun b m1 m2 ->
                                let min1 = createMin b m1
                                let min2 = createMin b m2
                                m1 <= m2 = (min1 |> minSTEmin min2)
                            |> Generators.testProp "min1 <= min2"
            let raiseMinLargerThanMax min max =
                            fun m1 m2 ->
                                let min1 = createMin true m1
                                let min2 = createMin false m2
                                m1 > m2 = (min1 |> minGTmin min2)
                            |> Generators.testProp "min1 incl > min2 excl"

                            fun m1 m2 ->
                                let min1 = createMin false m1
                                let min2 = createMin true m2
                                (m1 = m2 || m1 > m2) = (min1 |> minGTmin min2)
                            |> Generators.testProp
                                "min1 excl > min2 incl, also when min1 = min2"
                function
                            fun b m ->
                                let min = createMin b m
            /// must be taken into account.
                                min
                                |> minToBoolBigRational
                                |> fun (b, m) -> createMin b m = min
                            |> Generators.testProp
                                "construct and deconstruct min there and back again"
                        ]

                            fun b m1 m2 ->
                let run () = tests |> Generators.run
                            |> Generators.testProp "min1 > min2"
                | MinExcl m2, MinIncl m1 -> m2 >= m1
                            fun b m1 m2 ->
                                let min1 = createMin b m1
                                let min2 = createMin b m2
                                m1 < m2 = (min1 |> minSTmin min2)
                            |> Generators.testProp "min1 < min2"

                            fun m1 m2 ->
                                let min1 = createMin true m1
                                let min2 = createMin false m2
                                (m1 = m2 || m1 < m2) = (min1 |> minSTmin min2)
                            |> Generators.testProp
                                "min1 incl < min2 excl, also when min1 = min2"
                | MinIncl _ -> false
                            fun m1 m2 ->
                                let min1 = createMin false m1
                                let min2 = createMin true m2
                                m1 < m2 = (min1 |> minSTmin min2)
                            |> Generators.testProp "min1 excl < min2 incl"
            /// Creates a `Minimum` from a `BigRational` set.
                            fun b m1 m2 ->
                                let min1 = createMin b m1
            let maxGTEmax max1 max2 = max1 = max2 || maxGTmax max1 max2
                    s |> Set.minElement |> MinIncl |> Some
                            fun b m1 m2 ->
                                let min1 = createMin b m1
                                let min2 = createMin b m2
            let maxSTmax max1 max2 = max2 |> maxGTEmax max1 |> not
                | MinExcl v -> v
                            fun m1 m2 ->
                                let min1 = createMin true m1
                                let min2 = createMin false m2
                                m1 > m2 = (min1 |> minGTmin min2)
                            |> Generators.testProp "min1 incl > min2 excl"

                            fun m1 m2 ->
                                let min1 = createMin false m1
                                let min2 = createMin true m2
                                (m1 = m2 || m1 > m2) = (min1 |> minGTmin min2)
                            |> Generators.testProp
                                "min1 excl > min2 incl, also when min1 = min2"
                else
                            fun b m ->
                                let min = createMin b m
                open Expecto
                                min
                                |> minToBoolBigRational
                                |> fun (b, m) -> createMin b m = min
                            |> Generators.testProp
                                "construct and deconstruct min there and back again"
                        ]

                            fun b m1 m2 ->
                let run () = tests |> Generators.run
                            |> Generators.testProp "min1 > min2"

                            fun b m1 m2 ->
                                let min1 = createMin b m1
                                let min2 = createMin b m2
                                m1 < m2 = (min1 |> minSTmin min2)
                            |> Generators.testProp "min1 < min2"

                            fun m1 m2 ->
                                let min1 = createMin true m1
                                let min2 = createMin false m2
                                (m1 = m2 || m1 < m2) = (min1 |> minSTmin min2)
                            |> Generators.testProp
                                "min1 incl < min2 excl, also when min1 = min2"

                            fun m1 m2 ->
                                let min1 = createMin false m1
                                let min2 = createMin true m2
                                m1 < m2 = (min1 |> minSTmin min2)
                            |> Generators.testProp "min1 excl < min2 incl"
                let tests =
                    testList
                        "maximum"
                        [
                            fun b m1 m2 ->
                            fun b m1 m2 ->
                                let max1 = createMax b m1
                                let max2 = createMax b m2
                                m1 > m2 = (max1 |> maxGTmax max2)
                            |> Generators.testProp "max1 > max2"

                            fun b m1 m2 ->
                                let max1 = createMax b m1
                                let max2 = createMax b m2
                                m1 < m2 = (max1 |> maxSTmax max2)
                            |> Generators.testProp "max1 < max2"
                                let min2 = createMin b m2
                            fun m1 m2 ->
                                let max1 = createMax false m1
                                let max2 = createMax true m2
                                (m1 = m2 || m1 < m2) = (max1 |> maxSTmax max2)
                            |> Generators.testProp
                                "max1 excl < max2 incl, also when max1 = max2"
                                m1 > m2 = (min1 |> minGTmin min2)
                            fun m1 m2 ->
                                let max1 = createMax true m1
                                let max2 = createMax false m2
                                m1 < m2 = (max1 |> maxSTmax max2)
                            |> Generators.testProp "max1 incl < max2 excl"
                                (m1 = m2 || m1 > m2) = (min1 |> minGTmin min2)
                            fun b m1 m2 ->
                                let max1 = createMax b m1
                                let max2 = createMax b m2
                                m1 >= m2 = (max1 |> maxGTEmax max2)
                            |> Generators.testProp "max1 >= max2"

                            fun b m1 m2 ->
                                let max1 = createMax b m1
                                let max2 = createMax b m2
                                m1 <= m2 = (max1 |> maxSTEmax max2)
                            |> Generators.testProp "max1 <= max2"
                        ]
                            fun m1 m2 ->
                                let max1 = createMax false m1
                                let max2 = createMax true m2
                                m1 > m2 = (max1 |> maxGTmax max2)
                            |> Generators.testProp "max1 excl > max2 incl"

                            fun m1 m2 ->
                                let max1 = createMax true m1
                                let max2 = createMax false m2
                                (m1 = m2 || m1 > m2) = (max1 |> maxGTmax max2)
                            |> Generators.testProp
                                "max1 incl > max2 excl, also when max1 = max2"
                function
                            fun b m ->
                                let max = createMax b m
            /// must be taken into account.
                                max
                                |> maxToBoolBigRational
                                |> fun (b, m) -> createMax b m = max
                            |> Generators.testProp
                                "construct and deconstruct max there and back again"
                        "maximum"
                        ]

                            fun b m1 m2 ->
                let run () = tests |> Generators.run
                            |> Generators.testProp "max1 > max2"

                            fun b m1 m2 ->
                                let max1 = createMax b m1
                                let max2 = createMax b m2
                                m1 < m2 = (max1 |> maxSTmax max2)
                            |> Generators.testProp "max1 < max2"

                            fun m1 m2 ->
                                let max1 = createMax false m1
                                let max2 = createMax true m2
                                (m1 = m2 || m1 < m2) = (max1 |> maxSTmax max2)
                            |> Generators.testProp
                                "max1 excl < max2 incl, also when max1 = max2"
                if s |> Set.isEmpty then
                            fun m1 m2 ->
                                let max1 = createMax true m1
                                let max2 = createMax false m2
                                m1 < m2 = (max1 |> maxSTmax max2)
                            |> Generators.testProp "max1 incl < max2 excl"
            let maxToBigRational =
                            fun b m1 m2 ->
                                let max1 = createMax b m1
                                let max2 = createMax b m2
                                m1 >= m2 = (max1 |> maxGTEmax max2)
                            |> Generators.testProp "max1 >= max2"
            let isMaxExcl =
                            fun b m1 m2 ->
                                let max1 = createMax b m1
                                let max2 = createMax b m2
                                m1 <= m2 = (max1 |> maxSTEmax max2)
                            |> Generators.testProp "max1 <= max2"
            let isMaxIncl = isMaxExcl >> not
                            fun m1 m2 ->
                                let max1 = createMax false m1
                                let max2 = createMax true m2
                                m1 > m2 = (max1 |> maxGTmax max2)
                            |> Generators.testProp "max1 excl > max2 incl"

                            fun m1 m2 ->
                                let max1 = createMax true m1
            apply 0 zero zero zero zero zero zero Set.count
                                |> maxToBoolBigRational
                                |> fun (b, m) -> createMax b m = max
                            |> Generators.testProp
                                "construct and deconstruct max there and back again"
                        "maximum"

                        ]

                            fun b m1 m2 ->
                let run () = tests |> Generators.run
                            |> Generators.testProp "max1 > max2"

                            fun b m1 m2 ->
                                let max1 = createMax b m1
                                let max2 = createMax b m2
                                m1 < m2 = (max1 |> maxSTmax max2)
                            |> Generators.testProp "max1 < max2"

                            fun m1 m2 ->
                                let max1 = createMax false m1

                                let max2 = createMax true m2
                                (m1 = m2 || m1 < m2) = (max1 |> maxSTmax max2)
                            |> Generators.testProp
                                "max1 excl < max2 incl, also when max1 = max2"

                            fun m1 m2 ->
                                let max1 = createMax true m1
                                let max2 = createMax false m2
                                m1 < m2 = (max1 |> maxSTmax max2)
                            |> Generators.testProp "max1 incl < max2 excl"

                            fun b m1 m2 ->
                                let max1 = createMax b m1
                                let max2 = createMax b m2

                                m1 >= m2 = (max1 |> maxGTEmax max2)
                            |> Generators.testProp "max1 >= max2"

                            fun b m1 m2 ->
                                let max1 = createMax b m1
                                let max2 = createMax b m2
                                m1 <= m2 = (max1 |> maxSTEmax max2)
                            |> Generators.testProp "max1 <= max2"

                            fun m1 m2 ->
                                let max1 = createMax false m1
                                let max2 = createMax true m2
                                m1 > m2 = (max1 |> maxGTmax max2)
                            |> Generators.testProp "max1 excl > max2 incl"


                            fun m1 m2 ->
                                let max1 = createMax true m1
            apply 0 zero zero zero zero zero zero Set.count
                                |> maxToBoolBigRational
                                |> fun (b, m) -> createMax b m = max
                            |> Generators.testProp
                                "construct and deconstruct max there and back again"


                        ]


                let run () = tests |> Generators.run



        module ValueSet =


            /// Create a `ValueSet` from a set of `BigRational`.
            let create s =
                if s |> Seq.isEmpty then
                    Exceptions.ValueRangeEmptyValueSet
                    |> Exceptions.raiseExc


                else
                    s |> Set.ofSeq |> ValueSet



            let getMin vs =
                vs |> Set.minElement |> Minimum.createMin true


            let getMax vs =
                vs |> Set.maxElement |> Maximum.createMax true




        /// Aply the give functions to `Values`
        let apply unr fMin fMax fMinMax fIncr fMinIncr fIncrMax fValueSet =

            function
            | Unrestricted -> unr
            | Min min -> min |> fMin
            | Max max -> max |> fMax
            | MinMax (min, max) -> (min, max) |> fMinMax
            | Incr incr -> incr |> fIncr
            | MinIncr (min, incr) -> (min, incr) |> fMinIncr
            | IncrMax (incr, max) -> (incr, max) |> fIncrMax
            | ValueSet vs -> vs |> fValueSet


        /// Count the number of values in a `ValueRange`.

        /// Returns 0 if no count is possible.

        let cardinality =
            let zero _ = 0
            apply 0 zero zero zero zero zero zero Set.count


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
        let filter minOpt incrOpt maxOpt =
                returnFalse
                v |> isBetweenMinMax minOpt maxOpt
                && v |> isMultipleOfIncr incrOpt
            |> Set.filter


        /// Checks whether a `ValueRange` is `Incr`
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



        /// Checks whether a `ValueRange` is `MinIncr`
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

                    let incrToStr incr =
                        incr |> Seq.map brToStr |> String.concat ", "
        /// Checks whether a `ValueRange` is `MinIncr`
        let isIncrMax =
                    | Some min, None, None -> $"{left}{min |> brToStr}..>"

            apply
                    | None, None, Some max -> $"<..{max |> brToStr}{right}"
                returnFalse
                returnFalse
                        $"{left}{min |> brToStr}..[{incr |> incrToStr}]..>"
                returnFalse
                Boolean.returnTrue
                returnFalse
                        $"<..[{incr |> incrToStr}]..{max |> brToStr}{right}"

        /// Checks whether a `ValueRange` is a `ValueSet`
        let isValueSet =
            let returnFalse = Boolean.returnFalse

            apply
                false
                returnFalse
        let filter minOpt incrOpt maxOpt =
                returnFalse
                v |> isBetweenMinMax minOpt maxOpt
                && v |> isMultipleOfIncr incrOpt
            |> Set.filter
                Boolean.returnTrue


        /// Checks whether a `BigRational` is between an optional
        /// **min** and an optional **max**
                let incl, min =
                    min |> Minimum.minToBoolBigRational

            let fMin =
                function
                | None -> true
                let incl, max =
                    max |> Maximum.maxToBoolBigRational

                | Some (Minimum.MinExcl m) -> v > m

            let fMax =
                let minincl, min =
                    min |> Minimum.minToBoolBigRational

                let maxincl, max =
                    max |> Maximum.maxToBoolBigRational

                | Some (Maximum.MaxIncl m) -> v <= m
                | Some (Maximum.MaxExcl m) -> v < m


            (fMin minOpt) && (fMax maxOpt)
            let fMinIncr (min, incr) =
                let incl, min =
                    min |> Minimum.minToBoolBigRational

        let isMultipleOfIncr incrOpt v =
            let isDiv v i = v |> BigRational.isMultiple i

                let incl, max =
                    max |> Maximum.maxToBoolBigRational

            | None -> true

        /// Checks whether `Minimum` **min** > `Maximum` **max**.
            let unr =
                printRange None false None false None

            vr
            |> apply unr fMin fMax fMinMax fIncr fMinIncr fIncrMax fVs
        /// accounted for.
                    let incrToStr incr =
                        incr |> Seq.map brToStr |> String.concat ", "
            match min, max with
            | Minimum.MinIncl mn, Maximum.MaxIncl mx -> mn > mx
                    | Some min, None, None -> $"{left}{min |> brToStr}..>"
            | Minimum.MinExcl mn, Maximum.MaxExcl mx
            | Minimum.MinIncl mn, Maximum.MaxExcl mx -> mn >= mx
                    | None, None, Some max -> $"<..{max |> brToStr}{right}"
        /// Checks whether `Minimum` **min** <= `Maximum` **max**
        let minSTEmax max min = min |> minLTmax max |> not
                        $"{left}{min |> brToStr}..[{incr |> incrToStr}]..>"

        /// Checks whether `Minimum` **min** = `Maximum` **max**.
        /// Note that when one or both minimum and maximum are exclusive, they
                        $"<..[{incr |> incrToStr}]..{max |> brToStr}{right}"
        let minEQmax max min =
            match min, max with
            | Minimum.MinIncl mn, Maximum.MaxIncl mx -> mn = mx
            | _ -> false


        /// Filter a set of `BigRational` according
        /// to **min** **max** and incr constraints
        let filter minOpt incrOpt maxOpt =
            fun v ->
                v |> isBetweenMinMax minOpt maxOpt
                && v |> isMultipleOfIncr incrOpt
            |> Set.filter


        /// Create a string (to print) representation of a `ValueRange`.
        /// `Exact` true prints exact bigrationals, when false
        /// print as floating numbers
        /// This assumes that the smallest increment will calculate the smallest
                    min |> Minimum.minToBoolBigRational


            let printVals vals =
                let vals =
                let incl, max =
                    max |> Maximum.maxToBoolBigRational

                    |> List.sort
                    |> List.map (
                        if exact then
                let minincl, min =
                    min |> Minimum.minToBoolBigRational

                let maxincl, max =
                    max |> Maximum.maxToBoolBigRational

                            BigRational.toFloat >> sprintf "%A"
                    )

                $"""[{vals |> String.concat ", "}]"""

            let fMinIncr (min, incr) =
                let incl, min =
                    min |> Minimum.minToBoolBigRational
            let min =
                min
                |> minMultipleOf incr
                |> Minimum.minToBigRational

            let max =
                max
                |> maxMultipleOf incr
                |> Maximum.maxToBigRational
                else
                    let left = if minincl then "[" else "<"
                let incl, max =
                    max |> Maximum.maxToBoolBigRational


                        if exact then
                        else
            let unr =
                printRange None false None false None

            vr
            |> apply unr fMin fMax fMinMax fIncr fMinIncr fIncrMax fVs

                    let incrToStr incr =
                        incr |> Seq.map brToStr |> String.concat ", "

                    match min, incr, max with
                    | Some min, None, None -> $"{left}{min |> brToStr}..>"
                    | Some min, None, Some max ->
                        $"{left}{min |> brToStr}..{max |> brToStr}{right}"
                    | None, None, Some max -> $"<..{max |> brToStr}{right}"
                    | None, Some incr, None -> $"<..[{incr |> incrToStr}]..>"
                    | Some min, Some incr, None ->
                    (min |> minMultipleOf incr, incr) |> MinIncr
                    | Some min, Some incr, Some max ->
                        $"{left}{min |> brToStr}..[{incr |> incrToStr}]..{max |> brToStr}{right}"
                    | None, Some incr, Some max ->
                        $"<..[{incr |> incrToStr}]..{max |> brToStr}{right}"
                    | None, None, None -> "[]"

            if vals |> List.isEmpty |> not then
                vals |> printVals
            else
                printRange min max incr



        /// Convert a `ValueRange` to a `string`.
        let toString exact vr =
            let fVs vs =
                print exact false None false None false None (vs |> Set.toList)

            let printRange min minincl max maxincl incr =
                print exact false min minincl max maxincl incr []

            let fMin min =
        /// This assumes that the smallest increment will calculate the smallest
                    min |> Minimum.minToBoolBigRational

                printRange (Some min) incl None false None

            let fMax max =
                let incl, max =
                    max |> Maximum.maxToBoolBigRational

                printRange None false (Some max) incl None

            let fMinMax (min, max) =
                let minincl, min =
                    min |> Minimum.minToBoolBigRational

                let maxincl, max =
                    max |> Maximum.maxToBoolBigRational

                printRange (Some min) minincl (Some max) maxincl None

            let fIncr incr =
                printRange None false None false (Some incr)

            let fMinIncr (min, incr) =
                let incl, min =
                    min |> Minimum.minToBoolBigRational
            let min =
                min
                |> minMultipleOf incr
                |> Minimum.minToBigRational

            let max =
                max
                |> maxMultipleOf incr
                |> Maximum.maxToBigRational

            let fIncrMax (incr, max) =
                let incl, max =
                    max |> Maximum.maxToBoolBigRational

                printRange None false (Some max) incl (Some incr)


            let unr =
                printRange None false None false None

            vr
            |> apply unr fMin fMax fMinMax fIncr fMinIncr fIncrMax fVs

                v |> isBetweenMinMax min max
                && v |> isMultipleOfIncr incr
        let unrestricted = Unrestricted


        /// Create a `Minimum` `Range` that is
        /// either inclusive or exclusive.
        let createMinValueRange isIncl m = m |> Minimum.createMin isIncl |> Min


                    (min |> minMultipleOf incr, incr) |> MinIncr
        /// either inclusive or exclusive.
        let createMaxValueRange isIncl m = m |> Maximum.createMax isIncl |> Max


        /// Create a `MinMax` `ValueRange`. If **min** > **max** raises
        /// an `MinLargetThanMax` exception. If min equals max, a `ValueSet` with
        /// value min (= max).
        let minMaxToValueRange min max =
            if min |> minLTmax max then
                Exceptions.raiseMinLargerThanMax min max

            elif min |> minEQmax max then
                minIncrToValueRange (newMin |> minMultipleOf incr) incr
                |> Minimum.minToBigRational
                |> Set.singleton
                |> ValueSet.create

            else
                (min, max) |> MinMax


        /// Calculate `Minimum` as a multiple of `Increment` **incr**.
        /// This assumes that the smallest increment will calculate the smallest
        /// minimum as a multiple of that increment.
        let minMultipleOf incr min =
            match min |> Minimum.minToBoolBigRational with
            | true, br -> br |> BigRational.minInclMultipleOf incr
            | false, br -> br |> BigRational.minExclMultipleOf incr
            |> fun (b, br) -> Minimum.createMin b br

        // Calculate `Maximum` **max** as a multiple of **incr**.
        let maxMultipleOf incr max =
            match max |> Maximum.maxToBoolBigRational with
            | true, br -> br |> BigRational.maxInclMultipleOf incr
            | false, br -> br |> BigRational.maxExclMultipleOf incr
            |> fun (b, br) -> Maximum.createMax b br


        let minIncrToValueRange min incr =
            (min |> minMultipleOf incr, incr) |> MinIncr


        let incrMaxToValueRange incr max =
            (incr, max |> maxMultipleOf incr) |> IncrMax


        /// Create a set of `BigRational` using **min**, **incr** and a **max**.
        let minIncrMaxToValueRange min incr max =
            let min =
                min
                |> minMultipleOf incr
                |> Minimum.minToBigRational

            let max =
                max
                |> maxMultipleOf incr
                |> Maximum.maxToBigRational
            let fMinIncr (min, incr) = minIncrMaxToValueRange min incr newMax
                for i in incr do
                    [ min..i..max ]
            ]
            |> List.collect id
            |> Set.ofList
            |> ValueSet


        /// Create a `ValueRange` using a `ValueSet` **vs**
        /// an optional `Minimum` **min** and `Maximum` **max**.
        /// If both **min** and **max** are `None` an `Unrestricted`
        /// `ValueRange` is created.
                v |> isBetweenMinMax min max
                && v |> isMultipleOfIncr incr
            | None ->
                match minOpt, incrOpt, maxOpt with
                | None, None, None -> unrestricted
                | Some min, None, None -> min |> Min
                | None, None, Some max -> max |> Max
                | Some min, None, Some max -> minMaxToValueRange min max
                | None, Some incr, None -> incr |> Incr
                | Some min, Some incr, None ->
                    (min |> minMultipleOf incr, incr) |> MinIncr
                | None, Some incr, Some max ->
                    (incr, max |> maxMultipleOf incr) |> IncrMax
                | Some min, Some incr, Some max ->
                    minIncrMaxToValueRange min incr max
            | Some vs -> vs |> ValueSet.create


        /// Get an optional `Minimum` in a `ValueRange`
                       incr
                       |> Set.exists (fun i' -> i |> BigRational.isMultiple i')
                   ) then
                    newIncr
                minIncrToValueRange (newMin |> minMultipleOf incr) incr
                else
                    incr
                Option.none
                (fst >> Some)
                Option.none
                (ValueSet.getMin >> Some)

            let fMax max =
                (newIncr, max |> maxMultipleOf newIncr) |> IncrMax
        /// Get an optional `Maximum` in a `ValueRange`
            let fMinMax (min, max) = minIncrMaxToValueRange min newIncr max
                None
                Option.none
                Some
                (snd >> Some)
                Option.none
                Option.none
                (snd >> Some)
                (ValueSet.getMax >> Some)


        /// Get an optional `Incr` in a `ValueRange`
        let getIncr =
            apply
                None
                Option.none
                Option.none
                Option.none
            |> apply unr fMin fMax fMinMax fIncr fMinIncr fIncrMax fValueSet
        let getValueSet =
            apply
                None
                Option.none
                Option.none
                Option.none
                Option.none
                Option.none
                vr |> getMin, vr |> getIncr, vr |> getMax, vr |> getValueSet
        /// a `BigRational` **v**.
        let contains v vr =
            match vr with
            | ValueSet vs -> vs |> Set.contains v
            | _ ->
                let min = vr |> getMin
                let max = vr |> getMax
                let incr = vr |> getIncr

                v |> isBetweenMinMax min max
                && v |> isMultipleOfIncr incr


        /// Apply a `Minimum` **min** to a `ValueRange` **vr**.
        /// If minimum cannot be set the original `Minimum` is returned.
        /// So, it always returns a more restrictive, i.e. larger, or equal `Minimum`.
        let setMin newMin (vr: ValueRange) =
            // Check whether the new min is more restrictive than the old min
            let checkMin min =
                if newMin |> Minimum.minGTmin min then
                    newMin
                else
                    min
                let raiseExc =
                    MinMaxCalculatorException >> raise
            let fMin = checkMin >> Min

            let fMax max = minMaxToValueRange newMin max

                       incr
                       |> Set.exists (fun i' -> i |> BigRational.isMultiple i')
                   ) then
                    newIncr
                minIncrToValueRange (newMin |> minMultipleOf incr) incr
                else
                    incr
            let fMinIncr (min, incr) =
                minIncrToValueRange (min |> checkMin) incr

            let fIncrMax (incr, max) =
                minIncrMaxToValueRange (newMin |> checkMin) incr max
            let fMax max =
                (newIncr, max |> maxMultipleOf newIncr) |> IncrMax
            let fValueSet =
            let fMinMax (min, max) = minIncrMaxToValueRange min newIncr max
                filter (Some newMin) incr max >> ValueSet.create

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


        /// Apply a `Maximum` **max** to a `ValueRange` **vr**.
        /// If maximum cannot be set the original is returned.
        /// So, it always returns a more restrictive, i.e. smaller, or equal `Maximum`.
            |> apply unr fMin fMax fMinMax fIncr fMinIncr fIncrMax fValueSet

            let fMax = checkMax >> Max

            let fMinMax (min, max) =
                minMaxToValueRange min (max |> checkMax)

            let fIncr incr = incrMaxToValueRange incr newMax

                vr |> getMin, vr |> getIncr, vr |> getMax, vr |> getValueSet

            let fValueSet =
                let min = vr |> getMin
                let incr = vr |> getIncr
                filter min incr (Some newMax) >> ValueSet.create

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


        /// Apply a **incr** to a `ValueRange` **vr**.
        /// If increment cannot be set the original is returned.
        /// So, the resulting increment is always more restrictive as the previous one
        let setIncr newIncr vr =

                let raiseExc =
                    MinMaxCalculatorException >> raise
            // ToDo needs testing!!
            let checkIncr incr =
                if newIncr
                   |> Set.forall (fun i ->
                       incr
                       |> Set.exists (fun i' -> i |> BigRational.isMultiple i')
                   ) then
                    newIncr

                else
                    incr

            let unr = newIncr |> Incr

            let fMin min = minIncrToValueRange min newIncr

            let fMax max =
                (newIncr, max |> maxMultipleOf newIncr) |> IncrMax

            let fMinMax (min, max) = minIncrMaxToValueRange min newIncr max

            let fIncr incr = incr |> checkIncr |> Incr

            let fMinIncr (min, incr) =
                let incr = incr |> checkIncr
                (min |> minMultipleOf incr, incr) |> MinIncr

            let fIncrMax (incr, max) =
                let incr = incr |> checkIncr
                (incr, max |> maxMultipleOf incr) |> IncrMax

            let fValueSet =
                let min = vr |> getMin
                let max = vr |> getMax
                filter min (Some newIncr) max >> ValueSet.create

            vr
            |> apply unr fMin fMax fMinMax fIncr fMinIncr fIncrMax fValueSet


        /// Appy a set of `BigRational` to a `ValueRange` **vr**.
        /// the result is a filtered or the intersect of
        /// the set of `BigRational` and **vr**.
        let setValues newVs vr =

            let min, incr, max, oldVs =
                vr |> getMin, vr |> getIncr, vr |> getMax, vr |> getValueSet

            let vs =
                match oldVs with
                | None -> newVs |> filter min incr max
                | Some vs -> newVs |> filter min incr max |> Set.intersect vs

            create min incr max (Some vs)


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

                let raiseExc =
                    MinMaxCalculatorException >> raise

                | BigRational.Add
                | BigRational.Subtr ->
            /// Calculate **x1** and **x2** with operator **op**
            /// and use **incl1** and **inc2** to determine whether
            /// the result is inclusive. Use constructor **c** to
            /// create the optional result.
            let calc c op (x1, incl1) (x2, incl2) =
                let incl =
                    match incl1, incl2 with
                // incr cannot be calculated based on division
                    | _ -> false

                match x1, x2 with
                | Some (v1), Some (v2) ->
                    if op |> BigRational.opIsDiv && v2 = 0N then
                        None
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
                | Some (min), _ when min >= 0N -> PP
                | _, Some (max) when max < 0N -> NN
            | ValueSet s, MinIncr (_, i)
            | MinIncr (_, i), ValueSet s

            | ValueSet s, IncrMax (i, _)
            | IncrMax (i, _), ValueSet s
            /// `Maximum` option for addition of
            | ValueSet s, MinIncr (_, i)
            | IncrMax (i, _), ValueSet s
                let min = calcMin (+) min1 min2
            | ValueSet s, MinIncr (_, i)
            | IncrMax (i, _), ValueSet s ->


            /// Calculate `Minimum` option and
            /// `Maximum` option for subtraction of
            /// (**min1**, **max1**) and (**min2**, **max2)
            let subtraction min1 max1 min2 max2 =
                let min = calcMin (-) min1 max2
                let max = calcMax (-) max1 min2
                min, max
                            | None -> false

                        m
                        |> Option.bind (Minimum.minToBigRational >> Some),
                        incl
            /// Calculate `Minimum` option and
            /// `Maximum` option for multiplication of
            /// (**min1**, **max1**) and (**min2**, **max2)
            let multiplication min1 max1 min2 max2 =
                match ((min1 |> fst), (max1 |> fst)),
                            | None -> false

                        m
                        |> Option.bind (Maximum.maxToBigRational >> Some),
                        incl
                | PP, PP -> // min = min1 * min2, max = max1 * max2
                    calcMin (*) min1 min2, calcMax (*) max1 max2
                | PP, NN -> // min = max1 * min2, max = min1 * max2
                    calcMin (*) max1 min2, calcMax (*) min1 max2
                | PP, NP -> // min = min1 * min2, max = max1 * max2
                | BigRational.Add
                | BigRational.Subtr ->
                | NN, PP -> // min = min1 * max2, max = max1 * min2
                    calcMin (*) min1 max2, calcMax (*) max1 min2
                | NN, NN -> // min = max1 * max2, max = min1 * min2
                    calcMin (*) max1 max2, calcMax (*) min1 min2
                | NN, NP -> // min = min1 * max2, max = min1 * min2
                    calcMin (*) min1 max2, calcMax (*) min1 min2
                | NP, PP -> // min = min1 * max2, max = max1 * max2
                // incr cannot be calculated based on division
                | NP, NN -> // min = max1 * min2, max = min1 * min2
                    calcMin (*) max1 min2, calcMax (*) min1 min2
                | NP, NP -> // min = min1 * max2, max = max1 * max2
                    calcMin (*) min1 max2, calcMax (*) max1 max2
                let min1, incr1, max1 =
                    x1 |> getMin, x1 |> getIncr, x1 |> getMax

                let min2, incr2, max2 =
                    x2 |> getMin, x2 |> getIncr, x2 |> getMax
            /// Calculate `Minimum` option and
            /// `Maximum` option for division of
            /// (**min1**, **max1**) and (**min2**, **max2)
            let division min1 max1 min2 max2 =
                match (min1 |> fst, max1 |> fst), (min2 |> fst, max2 |> fst)
                    with
                            | None -> false

                        m
                        |> Option.bind (Minimum.minToBigRational >> Some),
                        incl
                | PP, NN -> // min = max1 / max2	, max = min1 / min2
                    calcMin (/) max1 max2, calcMax (/) min1 min2
                | NN, PP -> // min = min1 / min2, max = max1 / max2
                    calcMin (/) min1 min2, calcMax (/) max1 max2
                | NN, NN -> // min = max1 / min2	, max = min1 / max2
                            | None -> false

                        m
                        |> Option.bind (Maximum.maxToBigRational >> Some),
                        incl
                    calcMin (/) min1 min2, calcMax (/) max1 min2
                | NP, NN -> // min = max1 / max2, max = min1 / max2
                    calcMin (/) max1 max2, calcMax (/) min1 max2
                // division by range containing zero
                | NN, NP
                | PP, NP
                | NP, NP -> None, None

            | ValueSet s, MinIncr (_, i)
            | MinIncr (_, i), ValueSet s
            /// according to the operand
            | ValueSet s, IncrMax (i, _)
            | IncrMax (i, _), ValueSet s
                | BigRational.Mult -> multiplication
            | ValueSet s, MinIncr (_, i)
            | IncrMax (i, _), ValueSet s
                | BigRational.Subtr -> subtraction
            | ValueSet s, MinIncr (_, i)
            | IncrMax (i, _), ValueSet s ->
                    |> Exceptions.raiseExc



        /// Calculate an increment with
        /// **incr1** of x1 and **incr2** of x2
        /// in an equation: y = x1 **op** x2
        let calcIncr op incr1 incr2 =
            match incr1, incr2 with
                            | None -> false

                        m
                        |> Option.bind (Minimum.minToBigRational >> Some),
                        incl
                // y.incr = x1.incr * x2.incr
                | BigRational.Mult ->
                    [
                        for x in i1 do
                            for y in i2 do
                            | None -> false

                        m
                        |> Option.bind (Maximum.maxToBigRational >> Some),
                        incl
                    ]
                    |> Set.ofList
                    |> Some

                // when y = x1 + x2 then y.incr = gcd of x1.incr and x2.incr
                | BigRational.Add
                | BigRational.Subtr ->
                    [
                        for x in i1 do
                            for y in i2 do
                                BigRational.gcd x y
                    ]
                    |> Set.ofList
                    |> Some
                // incr cannot be calculated based on division
                | _ -> None

            | _ -> None

                let min1, incr1, max1 =
                    x1 |> getMin, x1 |> getIncr, x1 |> getMax

                let min2, incr2, max2 =
                    x2 |> getMin, x2 |> getIncr, x2 |> getMax
        /// to `ValueRange` **x1** and **x2**.
        /// Calculates `Minimum`, increment or `Maximum`
        /// if either **x1** or **x2** is not a `ValueSet`.
        /// Doesn't perform any calculation when both
        /// **x1** and **x2** are `Unrestricted`.
        let calc op (x1, x2) =
                            | None -> false

                        m
                        |> Option.bind (Minimum.minToBigRational >> Some),
                        incl
            | Unrestricted, Unrestricted -> unrestricted
            | ValueSet s1, ValueSet s2 ->
                // When one of the sets does not contain any value then the result of
                // of the calculation cannot contain any value either
                if s1 |> Set.isEmpty || s2 |> Set.isEmpty then
                            | None -> false

                        m
                        |> Option.bind (Maximum.maxToBigRational >> Some),
                        incl

                else
                    Seq.allPairs s1 s2
                    |> Seq.map (fun (x1, x2) -> x1 |> op <| x2)
                    |> ValueSet.create

            // A set with an increment results in a new set of increment
            // Need to match all scenarios with a valueset and an increment
            | ValueSet s, MinIncr (_, i)
            | MinIncr (_, i), ValueSet s

            | ValueSet s, IncrMax (i, _)
            | IncrMax (i, _), ValueSet s

            | ValueSet s, MinIncr (_, i)
            | IncrMax (i, _), ValueSet s

            | ValueSet s, MinIncr (_, i)
            | IncrMax (i, _), ValueSet s ->
    let createRes =
        createSucc ("Result" |> Name.createExc)
                let min1, max1 = x1 |> getMin, x1 |> getMax
                let min2, max2 = x2 |> getMin, x2 |> getMax

                let min, max =
                    let getMin m =
                        let incl =
                            match m with
                            | Some v -> v |> Minimum.isMinIncl
                            | None -> false

                        m
                        |> Option.bind (Minimum.minToBigRational >> Some),
                        incl

                    let getMax m =
                        let incl =
                            match m with
                            | Some v -> v |> Maximum.isMaxIncl
                            | None -> false

                        m
                        |> Option.bind (Maximum.maxToBigRational >> Some),
                        incl

                    MinMaxCalcultor.calcMinMax
                        op
                        (min1 |> getMin)
                        (max1 |> getMax)
                        (min2 |> getMin)
                        (max2 |> getMax)

                // calculate a new increment based upon the valueset and an increment
                let incr1 = i |> Some
                let incr2 = s |> Some
                let incr = calcIncr op incr1 incr2

                match min, incr, max with
                | None, None, None -> unrestricted
                | _ -> create min incr max None

            // In any other case calculate min, incr and max
            | _ ->
                let min1, incr1, max1 =
                    x1 |> getMin, x1 |> getIncr, x1 |> getMax

                let min2, incr2, max2 =
                    x2 |> getMin, x2 |> getIncr, x2 |> getMax

                let min, max =
                    let getMin m =
                        let incl =
                            match m with
                            | Some v -> v |> Minimum.isMinIncl
                            | None -> false

                        m
                        |> Option.bind (Minimum.minToBigRational >> Some),
                        incl
    let count v =
        v |> getValueRange |> ValueRange.cardinality
                    let getMax m =
                        let incl =
                            match m with
                            | Some v -> v |> Maximum.isMaxIncl
                            | None -> false

                        m
                        |> Option.bind (Maximum.maxToBigRational >> Some),
                        incl

                    MinMaxCalcultor.calcMinMax
                        op
                        (min1 |> getMin)
                        (max1 |> getMax)
                        (min2 |> getMin)
                        (max2 |> getMax)

                // calculate a new increment based upon the incr1 and incr2
                let incr = calcIncr op incr1 incr2

                match min, incr, max with
                | None, None, None -> unrestricted
    let isUnrestricted =
        getValueRange >> ValueRange.isUnrestricted




        /// Checks whether a `ValueRange` vr1 is a subset of
    let createRes =
        createSucc ("Result" |> Name.createExc)
        let isSubSetOf vr2 vr1 =
            match vr1, vr2 with
            | ValueSet s1, ValueSet s2 -> s2 |> Set.isSubset s1
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
            | ValueSet vs ->
                if vs |> Set.isEmpty then
                    Exceptions.ValueRangeEmptyValueSet
                    |> Exceptions.raiseExc

                else
                    y |> setValues vs
            | _ -> y |> set getMin setMin |> set getMax setMax


        // Extend type with basic arrhythmic operations.
        type ValueRangeCalc =
            | Mult
            | Div
            | Add
            | Subtr
            | Expr

            static member inline (?<-)(op, vr1, vr2) =
                match op with
                | Mult -> calc (*) (vr1, vr2)
                | Div -> calc (/) (vr1, vr2)
                | Add -> calc (+) (vr1, vr2)
                | Subtr -> calc (-) (vr1, vr2)
                | Expr -> applyExpr vr1 vr2


        module Operators =

            let inline (^*) vr1 vr2 = (?<-) Mult vr1 vr2

            let inline (^/) vr1 vr2 = (?<-) Div vr1 vr2

            let inline (^+) vr1 vr2 = (?<-) Add vr1 vr2

            let inline (^-) vr1 vr2 = (?<-) Subtr vr1 vr2

            let inline (<==) vr1 vr2 = (?<-) Expr vr1 vr2



    open Informedica.Utils.Lib.BCL
    let count v =
        let createDto n unr vals min minincl incr max maxincl =
            {
                Name = n
                Unr = unr
                Vals = vals
                Min = min
                MinIncl = minincl
                Incr = incr
                Max = max
                MaxIncl = maxincl
            }

    module Minimum = ValueRange.Minimum
        let createNew n =
            createDto n true [] None false [] None false

    type ValueRangeException = ValueRange.Exceptions.ValueRangeException


    module Exceptions =

        exception VariableException of Exceptions.Message

        let raiseExc m = m |> VariableException |> raise
        let setMin min incl dto =
            { dto with
                Unr = false
                Min = min
                MinIncl = incl
            }

    /// Create a `Variable` and passes
        let setMax max incl dto =
            { dto with
                Unr = false
                Max = max
                MaxIncl = incl
            }
    let create succ n vs = { Name = n; Values = vs } |> succ


    /// Create a `Variable` and directly
    /// return the result.
    let createSucc = create id
    let isUnrestricted =
            | "vals" -> Vals
            | "minincl" -> MinIncl
            | "minexcl" -> MinExcl
            | "incr" -> Incr
            | "maxincl" -> MaxIncl
            | "maxexcl" -> MaxExcl
            | _ -> NoProp
        createSucc ("Result" |> Name.createExc)


    /// Apply **f** to `Variable` **var**.
    let apply f (var: Variable) = var |> f


                | [ v ] -> v |> Some
                | _ -> None


            | Vals -> dto |> setVals vs
            | MinIncl -> dto |> setMin (vs |> getVal) true
            | MinExcl -> dto |> setMin (vs |> getVal) false
            | Incr -> dto |> setIncr vs
            | MaxIncl -> dto |> setMax (vs |> getVal) true
            | MaxExcl -> dto |> setMax (vs |> getVal) false
            | NoProp -> dto
    let getName v = (v |> get).Name


            exact
            {
                Name = name
                Unr = unr
                Vals = vals
                Min = min
                MinIncl = minincl
                Incr = incr
                Max = max
                MaxIncl = maxincl
            }
            =

            let vals =
                ValueRange.print
    /// Get the `ValueRange of a `Variable`.
                    unr
                    min
                    minincl
                    max
                    maxincl
                    (Some incr)
                    vals
    let setName n v : Variable = { v with Name = n }

    /// Apply a `ValueRange` **vr** to
    /// `Variable` **v**.
    let setValueRange v vr =
        try
            let vr' = (v |> get).Values <== vr

            { v with Values = vr' }
            let n =
                dto.Name
                |> Name.create succ (fun m -> m |> Name.Exceptions.raiseExc)
        with
        | :? ValueRangeException ->
            (v, vr)
            |> Exceptions.VariableCannotSetValueRange
                | _ -> dto.Vals |> Set.ofList |> Some

            let min =
                dto.Min
                |> Option.bind (fun v ->
                    v |> Minimum.createMin dto.MinIncl |> Some
                )

            let max =
                dto.Max
                |> Option.bind (fun v ->
                    v |> Maximum.createMax dto.MaxIncl |> Some
                )

    let setNonZeroOrNegative v =
        let vr =
            (v |> get).Values
                    | [] -> None
                    | _ -> dto.Incr |> Set.ofList |> Some
    /// Get the number of distinct values
            let vr = ValueRange.create min incr max vs
        let createDto n unr vals min minincl incr max maxincl =
            {
                Name = n
                Unr = unr
                Vals = vals
                Min = min
                MinIncl = minincl
                Incr = incr
                Max = max
                MaxIncl = maxincl
            let n =
                dto.Name |> Name.create succ (fun m -> m |> fail)


        let createNew n =
            createDto n true [] None false [] None false
                | _ -> dto.Vals |> Set.ofList |> Some

            let min =
                dto.Min
                |> Option.bind (fun v ->
                    v |> Minimum.createMin dto.MinIncl |> Some
                )

            let max =
                dto.Max
                |> Option.bind (fun v ->
                    v |> Maximum.createMax dto.MaxIncl |> Some
                )
    /// i.e. there is but one possible value left.
    let isSolved v =
                match dto.Incr with
        && (v |> getValueRange |> ValueRange.isValueSet)
                | _ -> dto.Incr |> Set.ofList |> Some
                MinIncl = incl
            }

    /// Checks whether a `Variable` is *solvable*
        let setMax max incl dto =
            { dto with
                Unr = false
            with
            | _ -> None
                MaxIncl = incl
            }
    /// (or no values at all)
    let isSolvable = isSolved >> not

            let dto =
                createNew (let (Name.Name n) = v.Name in n)
    /// Checks whether there are no restrictions to
            let unr =
                v.Values |> ValueRange.isUnrestricted
    let isUnrestricted =
            | "vals" -> Vals
            | "minincl" -> MinIncl
                | Some m -> m |> Minimum.isMinExcl |> not
                | None -> false
            | "incr" -> Incr
            | "maxincl" -> MaxIncl
            | "maxexcl" -> MaxExcl
                | Some m -> m |> Maximum.isMaxExcl |> not
                | None -> false
        (v1 |> getValueRange) |> op
            let min =
        |> createRes


    /// Extend type with basic arrhythmic operations.
            let max =
                | [ v ] -> v |> Some
                | _ -> None
        | Add
        | Subtr
            | Vals -> dto |> setVals vs
            | MinIncl -> dto |> setMin (vs |> getVal) true
            | MinExcl -> dto |> setMin (vs |> getVal) false
            | Incr -> dto |> setIncr vs
                    | Some i -> i |> Set.toList
                    | None -> []
            | NoProp -> dto
            | Div -> calc (^/) (v1, v2)
            | Add -> calc (^+) (v1, v2)
            | Subtr -> calc (^-) (v1, v2)
            exact
                    | Some vs -> vs |> Set.toList
                    | None -> []
                Unr = unr
                Vals = vals
                Min = min
                MinIncl = minincl
                Incr = incr
                Max = max
                MaxIncl = maxincl
            }
                MaxIncl = maxincl
            }

            let vals =
                ValueRange.print
            | Expr ->
                    unr
                    min
                    minincl
                    max
                    maxincl
                    (Some incr)
                    vals
        let inline (^*) v1 v2 = (?<-) Mult v1 v2
        let inline (^/) v1 v2 = (?<-) Div v1 v2

        let inline (^+) v1 v2 = (?<-) Add v1 v2

        let inline (^-) v1 v2 = (?<-) Subtr v1 v2

        let inline (<==) v1 v2 = (?<-) Expr v1 v2

            let n =
                dto.Name
                |> Name.create succ (fun m -> m |> Name.Exceptions.raiseExc)

    /// Handle the creation of a `Variable` from a `Dto` and
    /// vice versa.
    module Dto =
                | _ -> dto.Vals |> Set.ofList |> Some

            let min =
                dto.Min
                |> Option.bind (fun v ->
                  vars |> List.filter ((=) v) |> List.length > 1
              )
                )

            let max =
                dto.Max
                |> Option.bind (fun v ->
                    v |> Maximum.createMax dto.MaxIncl |> Some
                )
        type Dto =
                Unr: bool
                Min: BigRational option
                MinIncl: bool
                    | [] -> None
                    | _ -> dto.Incr |> Set.ofList |> Some

            let vr = ValueRange.create min incr max vs
        let createDto n unr vals min minincl incr max maxincl =
            {
                Name = n
                Unr = unr
                Vals = vals
    let createProductEqExc =
        createProductEq id Exception.raiseExc
                MinIncl = minincl
                Incr = incr
                Max = max
                MaxIncl = maxincl
    let createSumEqExc =
        createSumEq id Exception.raiseExc
                dto.Name |> Name.create succ (fun m -> m |> fail)

        /// Create an *empty* *new* `Dto` with only a name **n**
        let createNew n =
            createDto n true [] None false [] None false
                | _ -> dto.Vals |> Set.ofList |> Some

            let min =
                dto.Min
    let isProduct =
        apply (fun _ _ -> true) (fun _ _ -> false)
                    v |> Minimum.createMin dto.MinIncl |> Some
                )
    let isSum =
        apply (fun _ _ -> true) (fun _ _ -> false)
            let max =
                dto.Max
                |> Option.bind (fun v ->
                    v |> Maximum.createMax dto.MaxIncl |> Some
                )
        /// making sure the `Unr` is set to `false`.
        let setVals vals dto = { dto with Unr = false; Vals = vals }
                match dto.Incr with
        /// Set a `min` to an **dto** that is either inclusive `incl` true or exclusive `false`
                | _ -> dto.Incr |> Set.ofList |> Some
                MinIncl = incl
            }

        /// Set a `max` to an **dto** that is either inclusive `incl` true or exclusive `false`
        let setMax max incl dto =
            { dto with
                Unr = false
            with
            | _ -> None
                MaxIncl = incl
                    (if c = 0 then 1 else c) * acc
                )

        /// Set an `incr` to a **dto**
        let setIncr incr dto = { dto with Unr = false; Incr = incr }
            let dto =
        let op =
            if eq |> isProduct then "*" else "+"

        /// Match a string **p** to a field of `Dto`
            let unr =
                v.Values |> ValueRange.isUnrestricted
            match p |> String.toLower with
            | "vals" -> Vals
            | "minincl" -> MinIncl
                | Some m -> m |> Minimum.isMinExcl |> not
                | None -> false
            | "incr" -> Incr
            | "maxincl" -> MaxIncl
            | "maxexcl" -> MaxExcl
                | Some m -> m |> Maximum.isMaxExcl |> not
                | None -> false

            let min =
        /// Set a `Dto` member **p** with a value `v` to a `Dto` **dto**.
        /// If no field can be matched the **dto** is returned unchanged.
        let setProp p vs dto =
            let getVal vs =
            let max =
                | [ v ] -> v |> Some
                | _ -> None

            let xs' =
                xs |> List.map Variable.setNonZeroOrNegative

            match p with
            | Vals -> dto |> setVals vs
            | MinIncl -> dto |> setMin (vs |> getVal) true
            | MinExcl -> dto |> setMin (vs |> getVal) false
            | Incr -> dto |> setIncr vs
                    | Some i -> i |> Set.toList
                    | None -> []
            | NoProp -> dto

        /// Return a `string` representation of a `Dto`
        let toString
            exact
                    | Some vs -> vs |> Set.toList
                    | None -> []
                Unr = unr
                Vals = vals
                Min = min
                MinIncl = minincl
                Incr = incr
                Max = max
                MaxIncl = maxincl
            }
                MaxIncl = maxincl
            }

            let vals =
                ValueRange.print
                    exact
                    unr
            vr' |> Variable.getName = (vr |> Variable.getName)
        )
                    minincl
                    max
                    maxincl
                    (Some incr)
                    vals

            sprintf "%s%s" name vals


        /// Create a `Variable` from a `Dto` and
        /// raise a `DtoException` if this fails.
        let fromDto (dto: Dto) =
            let succ = id
            let vs =
                vs |> List.replace ((Variable.eqName) v) v

            let n =
                dto.Name
                |> Name.create succ (fun m -> m |> Name.Exceptions.raiseExc)

            let vs =
                match dto.Vals with
                | [] -> None
                | _ -> dto.Vals |> Set.ofList |> Some

            let min =
                dto.Min
                |> Option.bind (fun v ->
                  vars |> List.filter ((=) v) |> List.length > 1
              )
                )

            let max =
                dto.Max
                |> Option.bind (fun v ->
                    v |> Maximum.createMax dto.MaxIncl |> Some
                )

            let incr =
                dto.Incr
                |> function
                    | [] -> None
                    | _ -> dto.Incr |> Set.ofList |> Some

            let vr = ValueRange.create min incr max vs

            create succ n vr


        /// Create a `Variable` option from a `Dto` and
    let createProductEqExc =
        createProductEq id Exception.raiseExc
        let fromDtoOpt (dto: Dto) =
            let succ = Some
            let fail = Option.none

    let createSumEqExc =
        createSumEq id Exception.raiseExc
                dto.Name |> Name.create succ (fun m -> m |> fail)

            let vs =
                match dto.Vals with
                | [] -> None
                | _ -> dto.Vals |> Set.ofList |> Some

            let min =
                dto.Min
    let isProduct =
        apply (fun _ _ -> true) (fun _ _ -> false)
                    v |> Minimum.createMin dto.MinIncl |> Some
                )
    let isSum =
        apply (fun _ _ -> true) (fun _ _ -> false)
            let max =
                dto.Max
                |> Option.bind (fun v ->
                    v |> Maximum.createMax dto.MaxIncl |> Some
                )

            let incr =
                match dto.Incr with
                | [] -> None
                | _ -> dto.Incr |> Set.ofList |> Some

            try
                let vr = ValueRange.create min incr max vs

                match n with
                | Some n' -> create succ n' vr
                | _ -> None
            with
            | _ -> None

                    (if c = 0 then 1 else c) * acc
                )
        /// Create a `Dto` from a `Variable`.
        let toDto (v: Variable) =

            let dto =
        let op =
            if eq |> isProduct then "*" else "+"


            let unr =
                v.Values |> ValueRange.isUnrestricted

            let minincl =
                match v.Values |> ValueRange.getMin with
                | Some m -> m |> Minimum.isMinExcl |> not
                | None -> false

            let maxincl =
                match v.Values |> ValueRange.getMax with
                | Some m -> m |> Maximum.isMaxExcl |> not
                | None -> false

            let min =
                v.Values
                |> ValueRange.getMin
                |> Option.bind (Minimum.minToBigRational >> Some)

            let max =
                v.Values
                |> ValueRange.getMax

            let xs' =
                xs |> List.map Variable.setNonZeroOrNegative


            let incr =
                v.Values
                |> ValueRange.getIncr
                |> function
                let ychanged, y' =
                    calc [] op1 op1 x xs' [ y ]
                    | None -> []

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
                MaxIncl = maxincl
            }


/// Functions that handle the `Equation` type that
/// either represents a `ProductEquation` </br>
                            acc |> List.replaceOrAdd (Variable.eqName v) v
                        )
            vr' |> Variable.getName = (vr |> Variable.getName)
        )
/// y = x1 + x2 + ... + xn
module Equation =

    open Types
    open Variable.Operators

    module ValueRange = Variable.ValueRange

    module Exception =

        /// Equation exception
        exception EquationException of Exceptions.Message

            let vs =
                vs |> List.replace ((Variable.eqName) v) v

        let raiseExc m = m |> EquationException |> raise

    /// Create an `Equation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    /// The type of Equation product or sum
    /// is determined by the constructor **c**.
    let create c succ fail (y, xs) =
        let vars = y :: xs

        match vars
              |> List.filter (fun v ->
                  vars |> List.filter ((=) v) |> List.length > 1
              )
            with
        | [] -> (y, xs) |> c |> succ
        | duplicates ->
            duplicates
            |> Exceptions.EquationDuplicateVariables
            |> fail

    /// Create an `ProductEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    let createProductEq = create ProductEquation
            {
                Vars: VariableDto []
                IsProdEq: bool
            }
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    let createSumEq = create SumEquation

    /// Create an `ProductEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time raising an exception.
    let createProductEqExc =
        createProductEq id Exception.raiseExc

    /// Create an `SumEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time raising an exception.
    let createSumEqExc =

            let varToString =
                Variable.Dto.toString exact

    /// Apply **fp** to a `ProductEquation` and
    /// **fs** to a `SumEquation`.
    let apply fp fs =
        function
        | ProductEquation (y, xs) -> fp y xs
        | SumEquation (y, xs) -> fs y xs

    /// Check whether an `Equation` is a product equation
    let isProduct =
        apply (fun _ _ -> true) (fun _ _ -> false)

    /// Check whether an `Equation` is a sum equation
    let isSum =
        apply (fun _ _ -> true) (fun _ _ -> false)

    /// Turn an `Equation` into a list of `Variable`
    let toVars =
        let f y xs = y :: xs
        apply f f

    let count e =
        e
        |> toVars

                let e =
                    (y, xs |> List.map Variable.Dto.fromDto)

    let countProduct e =
        match e with
        | SumEquation _ -> -1
        | _ ->
            e
            |> toVars
            |> List.fold
                (fun acc v ->
                {
                    Vars =
                        y :: xs
                        |> List.map Variable.Dto.toDto
                        |> List.toArray
                    IsProdEq = isProd
                }

    let toString exact eq =
        let op =
            if eq |> isProduct then "*" else "+"

        let varToString = Variable.toString exact

        match eq |> toVars with
        | [] -> ""
        | _ :: [] -> ""
        | y :: xs ->
            let s =
                sprintf "%s = " (y |> varToString)
                + (xs
                   |> List.fold
                       (fun s v -> s + (v |> varToString) + " " + op + " ")
                       "")

            s.Substring(0, s.Length - 2)


    /// Make sure that the `Variables` in the
    /// `Equation` can only contain positive
    /// non zero values.
    let nonZeroOrNegative e =
        let set c y xs =
            let y' = y |> Variable.setNonZeroOrNegative

            let xs' =
                xs |> List.map Variable.setNonZeroOrNegative

            |> Variable.getName
        )

        let fp = set ProductEquation
        let fs = set SumEquation
        e |> apply fp fs
                let ychanged, y' =
                    calc [] op1 op1 x xs' [ y ]
    /// Check whether an `Equation` contains
    /// a `Variable` **v**
    let contains v =
        toVars >> (List.exists (Variable.eqName v))

    /// Check whether `Equation`s
    /// **eq1** and **eq2** are equal
    let equals eq1 eq2 =
        let vrs1 = eq1 |> toVars
        let vrs2 = eq2 |> toVars

        vrs1
        |> List.forall (fun vr -> vrs2 |> List.exists (Variable.eqName vr))
        && ((eq1 |> isProduct) && (eq2 |> isProduct)
            || (eq1 |> isSum) && (eq2 |> isSum))

    /// Find a `Variable` **vr** in
    /// an `Equation` **eq** and return
    /// the result in a list
    let find vr eq =
        eq
        |> toVars
                            acc |> List.replaceOrAdd (Variable.eqName v) v
                        )
            vr' |> Variable.getName = (vr |> Variable.getName)
        )

    /// Find a `Variable` with `Name`
    /// **n** in an `Equation` **eq**
    /// and return the result as a list
    let findName n eq =
        eq
        |> toVars
        |> List.filter (fun vr -> vr |> Variable.getName = n)
                |> List.exists (fun v -> e |> Equation.contains v)
            )
    /// Replace a `Variable` **v** in the
    /// `Equation` **e**.
    let replace v e =
        let r c v vs =
            let vs =
                vs |> List.replace ((Variable.eqName) v) v

            c id (fun _ -> e) ((vs |> List.head), (vs |> List.tail))

        let fp y xs = r createProductEq v (y :: xs)
        let fs y xs = r createSumEq v (y :: xs)
        e |> apply fp fs


    // Check whether an equation is solved
    let isSolved =
        function
        | ProductEquation (y, xs)
        | SumEquation (y, xs) -> y :: xs |> List.forall Variable.isSolved


    // Check whether an equation will change by calc
    // This is not the same as `isSolved`!! If all
    // the variables are unrestricted than the equation
    // is not solvable but is also not solved.
    let isSolvable =
        function
        | ProductEquation (y, xs)
        | SumEquation (y, xs) ->
            let es = y :: xs

            es |> List.exists Variable.isSolvable
            {
                Vars: VariableDto []
                IsProdEq: bool
            }
               |> List.length > 1
               |> not


    let check e =
        let issub op (y: Variable) (xs: Variable list) =
            xs
            |> function
                | [] -> true
                | _ ->
                    if y.Values |> ValueRange.isValueSet
                       && xs
                          |> List.map Variable.getValueRange
                          |> List.forall ValueRange.isValueSet then
                        y.Values
            let varToString =
                Variable.Dto.toString exact

                        |> ValueRange.isSubSetOf (xs |> List.reduce (op)).Values

                    else
                        true

        if e |> isSolvable then
            e
            |> function
                | ProductEquation (y, xs) -> xs |> issub (^*) y
                | SumEquation (y, xs) -> xs |> issub (^+) y

                    let msg =
                        invalid |> Exceptions.SolverInvalidEquations
            true


    /// Solve an equation **e**, return a list of
    /// changed `Variable`s.
    let solve log eq =
        eq
        |> Events.EquationStartedSolving
        |> Logging.logInfo log

        // let runOnce y xs =

                let e =
                    (y, xs |> List.map Variable.Dto.fromDto)
        //         y::xs
        //         |> List.filter (Variable.getValueRange >> ValueRange.isValueSet)
        //         |> List.length
        //     let c2 =  (y::xs |> List.length)

        //     (c2 - c1 <= 1)

        if eq |> isSolved then
            []
                {
                    Vars =
                        y :: xs
                        |> List.map Variable.Dto.toDto
                        |> List.toArray
                    IsProdEq = isProd
                }
                |> Logging.logInfo log

                match rest with
                | [] ->
                    (changed, xs)
                    |> Events.EquationFinishedCalculation
                    |> Logging.logInfo log

                    changed, xs
                | x :: tail ->
                    let xs' = xs |> List.filter ((<>) x)

                    let x' =
                        match xs' with
                        | [] -> x <== y
                        | _ -> x <== (y |> op2 <| (xs' |> List.reduce op1))

                    let changed =
                        if x = x' then
                            changed
                        else
                            x'
                            |> Events.EquationVariableChanged
                            |> Logging.logInfo log

                            changed
                            |> List.replaceOrAdd (Variable.eqName x') x'

                    tail |> calc changed op1 op2 y (x' :: xs')

            // op1 = (*) or (+) and op2 = (/) or (-)
            |> Variable.getName
        )
                let x = xs |> List.head
                let xs' = xs |> List.filter ((<>) x)

                // Calculate y = x1 op1 x2 op1 .. op1 xn
                let ychanged, y' =
                    calc [] op1 op1 x xs' [ y ]

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
                    |> Logging.logInfo log

                    changed
                | _ ->
                    ychanged @ xchanged
                    |> List.fold
                        (fun acc v ->
                            acc |> List.replaceOrAdd (Variable.eqName v) v
                        )
                        changed
                    |> fun changed ->
                        // only run once so now is ready
                        if b then
                            changed
                        else
                            (b, y, xs, changed)
                            |> Events.EquationLoopedSolving
                            |> Logging.logInfo log

                |> List.exists (fun v -> e |> Equation.contains v)
            )
                            loop b op1 op2 y xs changed

            let b, y, xs, op1, op2 =
                match eq with
                | ProductEquation (y, xs) -> y, xs, (^*), (^/)
                | SumEquation (y, xs) -> y, xs, (^+), (^-)
                |> fun (y, xs, op1, op2) ->
                    // run only once when all but one is a value set
                    false, y, xs, op1, op2 // runOnce y xs, y, xs, op1, op2

            match xs with
            | [] -> []
            | _ ->
                try
                    loop b op1 op2 y xs []
                with
                | Variable.Exceptions.VariableException m ->
                    m |> Logging.logError log

                    eq
                    |> Events.EquationCouldNotBeSolved
                    |> Logging.logWarning log

                    m |> Variable.Exceptions.raiseExc


    module Dto =

        type VariableDto = Variable.Dto.Dto

        /// `Dto` for an `Equation`
        type Dto =
            {
                Vars: VariableDto []
                IsProdEq: bool
            }

        /// Create a `Dto` with `vars` (variable dto array)
        /// that is either a `ProductEquation` or a `SumEquation`
        let create isProd vars = { Vars = vars; IsProdEq = isProd }

        /// Create a `ProductEquation` `Dto`
        let createProd = create true

        /// Create a `SumEquation` `Dto`
        let createSum = create false

        /// Return the `string` representation of a `Dto`
        let toString exact (dto: Dto) =
            let op = if dto.IsProdEq then "*" else "+"

            let varToString =
                Variable.Dto.toString exact

            match dto.Vars |> Array.toList with
            | [] -> ""
            | _ :: [] -> ""
            | y :: xs ->
                let s =
                    sprintf "%s = " (y |> varToString)
                    + (xs
                       |> List.fold
                           (fun s v -> s + (v |> varToString) + " " + op + " ")
                           "")

                    let msg =
                        invalid |> Exceptions.SolverInvalidEquations


        /// Create a `Dto` and raise an exception if it fails
        let fromDto dto =
            let succ = id
            let fail = Exception.raiseExc

            match dto.Vars |> Array.toList with
            | [] -> Exceptions.EquationEmptyVariableList |> fail
            | y :: xs ->
                let y = y |> Variable.Dto.fromDto

                let e =
                    (y, xs |> List.map Variable.Dto.fromDto)

                if dto.IsProdEq then
                    e |> createProductEq succ fail
                else
                    e |> createSumEq succ fail

        /// Create a `Dto` from an `Equation` **e**
        let toDto e =
            let c isProd y xs =
                {
                    Vars =
                        y :: xs
                        |> List.map Variable.Dto.toDto
                        |> List.toArray
                    IsProdEq = isProd
                }

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


    let sortByName eqs =
        eqs
        |> List.sortBy (fun e ->
            e
            |> Equation.toVars
            |> List.head
            |> Variable.getName
        )


    /// Format a set of equations to print.
    /// Using **f** to allow additional processing
    /// of the string.
    let printEqs exact pf eqs =

        "equations result:\n" |> pf

        eqs
        |> sortByName
        |> List.map (Equation.toString exact)
        |> List.iteri (fun i s -> s |> sprintf "%i.\t%s" i |> pf)

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
        | Changed of Variable list


    /// Replace a list of `Variable` **vs**
    /// in a list of `Equation` **es**, return
    /// a list of replaced `Equation` and a list
    /// of unchanged `Equation`
    let replace vars es =
        let rpl, rst =
            es
            |> List.partition (fun e ->
                vars
                |> List.exists (fun v -> e |> Equation.contains v)
            )

        vars
        |> List.fold (fun acc v -> acc |> List.map (Equation.replace v)) rpl,
        rst

    /// Solve the equation `e` and return
    /// the set of equations `es` it belongs
    /// to either as `Changed` or `Unchanged`
    let solveEquation log e =

        let changed = e |> Equation.solve log

        if changed |> List.length > 0 then
            changed |> Changed
        else
            UnChanged


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
        if que |> List.length = 0 then
            que
        else
            que |> List.sortBy Equation.countProduct


    /// Create the equation solver using a
    /// product equation and a sum equation solver
    /// and function to determine whether an
    /// equation is solved
    let solve log sortQue vr eqs =

        let solveE = solveEquation log

        let rec loop n que acc =
            if n > ((que @ acc |> List.length) * 10) then
                (que @ acc)
                |> Exceptions.SolverLooped
                |> Logging.logError log

                (que @ acc)
                |> Exceptions.SolverLooped
                |> Exception.raiseExc

            let que = que |> sortQue

            que
            |> Events.SolverLoopedQue
            |> Logging.logInfo log

            match que with
            | [] ->
                match acc |> List.filter (Equation.check >> not) with
                | [] -> acc
                | invalid ->
                    let msg =
                        invalid |> Exceptions.SolverInvalidEquations

                    msg |> Logging.logError log
                    msg |> Exception.raiseExc

            | eq :: tail ->
                // If the equation is already solved, or not solvable
                // just put it to  the accumulated equations and go on with the rest
                if eq |> Equation.isSolvable |> not then
                    [ eq ] |> List.append acc |> loop (n + 1) tail

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

                                rst |> List.append eq |> loop (n + 1) que

                    // Equation did not in fact change, so put it to
                    // the accumulated equations and go on with the rest
                    | UnChanged ->
                        [ eq ] |> List.append acc |> loop (n + 1) tail

        eqs
        |> replace [ vr ]
        |> function
            | (rpl, rst) -> loop 0 rpl rst