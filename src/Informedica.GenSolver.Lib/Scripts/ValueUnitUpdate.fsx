

#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: FParsec"

#r "../../Informedica.Utils.Lib/bin/Debug/net6.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.GenUnits.Lib/bin/Debug/net6.0/Informedica.GenUnits.Lib.dll"



[<AutoOpen>]
module rec Types =

    open System
    open MathNet.Numerics

    open Informedica.GenUnits.Lib

    /// Represents a non empty/null string identifying a `Variable`.
    /// `Name` can be no longer than 1000 characters and cannot be
    /// a null string
    type Name = Name of string


    /// The minimal value in
    /// a `ValueRange`. Can be inclusive
    /// or exclusive.
    type Minimum =
        | MinIncl of ValueUnit
        | MinExcl of ValueUnit


    /// The maximum value in
    /// a `ValueRange`. Can be inclusive
    /// or exclusive.
    type Maximum =
        | MaxIncl of ValueUnit
        | MaxExcl of ValueUnit


    type ValueSet = ValueSet of ValueUnit


    type Increment = Increment of ValueUnit


    /// `ValueRange` represents a domain of
    /// rational numbers. A `ValueRange` can be either
    /// - `Unrestricted`: any rational number
    /// - `Increment`: any number that is a multiple of an increment
    /// - `Min`: have a minimum
    /// - `MinIncrement`: a minimum with the domain consisting of multiples of one increment
    /// - `Max`: have a maximum
    /// - `IncrementMax`: a domain of multiples of an increment with a maximum
    /// - `MinMax`: have both a minimum and maximum
    type ValueRange =
        | Unrestricted
        | NonZeroNoneNegative
        | Min of Minimum
        | Max of Maximum
        | MinMax of Minimum * Maximum
        | Incr of Increment
        | MinIncr of min: Minimum * incr: Increment
        | IncrMax of incr: Increment * max: Maximum
        | MinIncrMax of min: Minimum * incr: Increment * max: Maximum
        | ValSet of ValueSet // Set<BigRational>

    /// Represents a variable in an
    /// `Equation`. The variable is
    /// identified by `Name` and has
    /// a `Values` described by the
    /// `ValueRange`.
    type Variable = { Name: Name; Values: ValueRange }

    /// Represents a property of a `Variable`.
    type Property =
        | MinProp of Minimum
        | MaxProp of Maximum
        | IncrProp of Increment
        | ValsProp of ValueSet

    /// An equation is either a `ProductEquation`
    /// or a `Sumequation`, the first variable is the
    /// dependent variable, i.e. the result of the
    /// equation, the second part are the independent
    /// variables in the equation
    type Equation =
        | ProductEquation of Variable * Variable list
        | SumEquation of Variable * Variable list

    /// The `Result` of solving an `Equation`
    /// is that either the `Equation` is the
    /// same or has `Changed`.
    type SolveResult =
        | Unchanged
        | Changed of List<Variable * Property Set>
        | Errored of Exceptions.Message list


    /// Represents a constraint on a `Variable`.
    /// I.e. either a set of values, or an increment
    /// or a minimum of maximum.
    type Constraint =
        {
            Name: Name
            Property: Property
        }


    module Exceptions =

        type Message =
            | NameNullOrWhiteSpaceException
            | NameLongerThan1000 of name: string
            | ValueRangeMinLargerThanMax of Minimum * Maximum
            | ValueRangeNotAValidOperator
            | ValueRangeEmptyValueSet
            | ValueRangeTooManyValues of valueCount: int
            | ValueRangeEmptyIncrement
            | ValueRangeMinOverFlow of Minimum
            | ValueRangeMaxOverFlow of Maximum
            | ValueRangeMinMaxException of string
            | VariableCannotSetValueRange of Variable * ValueRange
            | VariableCannotCalcVariables of
                v1: Variable *
                op: (ValueRange -> ValueRange -> ValueRange) *
                v2: Variable
            | EquationDuplicateVariables of duplicateVars: Variable list
            | EquationEmptyVariableList
            | ConstraintVariableNotFound of Constraint * Equation list
            | SolverInvalidEquations of Equation list
            | SolverTooManyLoops of loopCount : int * Equation list
            | SolverErrored of loopCount: int * Message list * Equation list


    module Events =

        type Event =
            | EquationStartedSolving of Equation
            | EquationStartCalculation of
                op1: (Variable -> Variable -> Variable) *
                op2: (Variable -> Variable -> Variable) *
                x: Variable *
                y: Variable *
                xs: Variable List
            | EquationFinishedCalculation of Variable list * changed : bool
            | EquationCouldNotBeSolved of Equation
            | EquationFinishedSolving of Equation * SolveResult
            | SolverStartSolving of Equation list
            | SolverLoopedQue of loopCount: int * Equation list
            | SolverFinishedSolving of Equation list
            | ConstraintSortOrder of (int * Constraint) list
            | ConstraintApplied of Constraint
            | ConstrainedSolved of Constraint


    module Logging =

        type IMessage =
            interface
            end


        type TimeStamp = DateTime


        type Level =
            | Informative
            | Warning
            | Debug
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




[<AutoOpen>]
module Utils =

    module Constants =


        let MAX_LOOP_COUNT = 10


        let MAX_CALC_COUNT = 5000


        let MAX_BIGINT = 999999999999999999999999999999999999999999999999I



    module BigRational =

        open MathNet.Numerics


        let denominator (br : BigRational) = br.Denominator

        let numerator (br : BigRational) = br.Numerator



    module Array =

        open Informedica.Utils.Lib.BCL

        let removeBigRationalMultiples xs =
            if xs |> Array.isEmpty then xs
            else
                xs
                |> Array.fold (fun acc x1 ->
                    acc
                    |> Array.filter (fun x2 ->
                        x1 = x2 ||
                        x2 |> BigRational.isMultiple x1 |> not
                    )
                ) xs



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


    let ignore = { Log = ignore }



module Exceptions =

    /// Equation exception
    exception SolverException of Exceptions.Message list


    /// Raise an `EquationException` with `Message` `m`.
    let raiseExc log errs m =
        match log with
        | Some log ->
            // printfn $"adding error {m} to {errs |> List.length}"
            m |> Logging.logError log
        | None -> ()

        m::errs |> SolverException |> raise



module ValueUnit =

    open MathNet.Numerics

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL

    open Informedica.GenUnits.Lib
    open ValueUnit



    let getBaseValue = toBase >> getValue


    let isEmpty = getValue >> Array.isEmpty


    let isZero = getValue >> Array.forall ((=) 0N)

    let gtZero = getValue >> Array.forall ((<) 0N)

    let gteZero = getValue >> Array.forall ((<=) 0N)

    let stZero = getValue >> Array.forall ((>) 0N)

    let steZero = getValue >> Array.forall ((>=) 0N)


    let minElement = applyToValue (Array.min >> Array.singleton)


    let maxElement = applyToValue (Array.max >> Array.singleton)


    let multipleOf f incr vu =
        vu
        |> toBase
        |> applyToValue (fun vs ->
            let incr = incr |> getBaseValue |> Set.ofArray
            vs
            |> Array.map (f incr)
            |> Array.map snd
        )
        |> toUnit


    let minInclMultipleOf = multipleOf BigRational.minInclMultipleOf

    let minExclMultipleOf = multipleOf BigRational.minExclMultipleOf


    let maxInclMultipleOf = multipleOf BigRational.maxInclMultipleOf

    let maxExclMultipleOf = multipleOf BigRational.maxExclMultipleOf


    let denominator = getValue >> (Array.map BigRational.denominator)

    let numerator = getValue >> (Array.map BigRational.numerator)


    let filter pred = toBase >> applyToValue (Array.filter pred) >> toUnit

    let removeBigRationalMultiples =
        toBase >> applyToValue (Array.removeBigRationalMultiples) >> toUnit


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
        |> Array.forall (fun v ->
            vu1
            |> toBase
            |> getValue
            |> Array.exists ((=) v)
        )

    let takeFirst n = applyToValue (Array.take n)

    let takeLast n = applyToValue (Array.rev >> Array.take n >> Array.rev)


    let setSingleValue v vu =
        vu
        |> getUnit
        |> withValue [|v|]


    let setValue v vu =
        vu
        |> getUnit
        |> withValue v


    // ToDo replace with this
    let valueCount = getValue >> Array.length


    module Operators =

        /// Constant 0
        let zero = [|0N|] |> create Units.Count.times

        /// Constant 1
        let one = [|1N|] |> create Units.Count.times

        /// Constant 2
        let two = [|2N|] |> create Units.Count.times

        /// Constant 3
        let three = [|3N|] |> create Units.Count.times

        /// Check whether the operator is subtraction
        let opIsSubtr op = (three |> op <| two) = three - two // = 1

        /// Check whether the operator is addition
        let opIsAdd op   = (three |> op <| two) = three + two // = 5

        /// Check whether the operator is multiplication
        let opIsMult op  = (three |> op <| two) = three * two // = 6

        /// Check whether the operator is divsion
        let opIsDiv op   = (three |> op <| two) = three / two // = 3/2



        /// Match an operator `op` to either
        /// multiplication, division, addition
        /// or subtraction, returns `NoOp` when
        /// the operation is neither.
        let (|Mult|Div|Add|Subtr|) op =
            match op with
            | _ when op |> opIsMult  -> Mult
            | _ when op |> opIsDiv   -> Div
            | _ when op |> opIsAdd   -> Add
            | _ when op |> opIsSubtr -> Subtr
            | _ -> failwith "Operator is not supported"



module Variable =

    open System
    open MathNet.Numerics

    open Informedica.GenUnits.Lib
    open Types

    let raiseExc errs m = m |> Exceptions.raiseExc None errs


    module Name =

        open Informedica.Utils.Lib
        open Informedica.Utils.Lib.BCL

        /// Create with continuation with **succ** function
        /// when success and **fail** function when failure.
        /// Creates a `Name` from a`string`.
        let create succ fail s =
            let s = s |> String.trim
            if s |> String.IsNullOrWhiteSpace then
                Exceptions.NameNullOrWhiteSpaceException
                |> fail
            else
                if s |> String.length <= 1000 then s |> Name |> succ
                else
                    s
                    |> Exceptions.NameLongerThan1000
                    |> fail

        /// Returns a `Name` option if creation
        /// succeeds else `None`.
        let createOpt = create Some Option.none

        /// Create a `Name` that, raises
        /// an `NameException` when it fails.
        let createExc = create id (raiseExc [])

        /// Return the `string` value of a `Name`.
        let toString (Name s) = s



    /// Functions and types to create and handle `ValueRange`.
    module ValueRange =

        open Informedica.Utils.Lib
        open Informedica.Utils.Lib.BCL



        module Increment =


            let create vu =
                vu
                |> ValueUnit.filter ((<) 0N)
                |> ValueUnit.removeBigRationalMultiples
                |> fun vu ->
                    if vu |> ValueUnit.isEmpty |> not then vu |> Increment
                    else
                        Exceptions.ValueRangeEmptyIncrement
                        |> raiseExc []


            let map f (Increment incr) = incr |> f |> create


            let toValueUnit (Increment vu) = vu


            let intersect (Increment incr1) (Increment incr2) =
                incr1
                |> ValueUnit.intersect incr2
                |> create


            let calc op incr1 incr2 =
                let incr1 = incr1 |> ValueUnit.toBase
                let incr2 = incr2 |> ValueUnit.toBase

                match op with
                // y.incr = x1.incr * x2.incr
                | ValueUnit.Operators.Mult ->
                    incr1 |> op <| incr2
                    |> create
                    |> Some

                // when y = x1 + x2 then y.incr = x1.incr and x2.incr
                | ValueUnit.Operators.Add -> //| BigRational.Subtr ->
                    let vs1 = incr1 |> ValueUnit.getValue
                    let vs2 = incr2 |> ValueUnit.getValue

                    incr1
                    |> ValueUnit.setValue
                        (Array.append vs1 vs2)
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


            let toList (Increment incr) = incr |> ValueUnit.getValue |> Array.toList


            let isEmpty (Increment incr) = incr |> ValueUnit.isEmpty


            let count (Increment incr) = incr |> ValueUnit.getValue |> Array.length


            let restrict (Increment newIncr) (Increment oldIncr) =
                newIncr
                |> ValueUnit.toBase
                |> ValueUnit.filter (fun i1 ->
                    oldIncr
                    |> ValueUnit.getBaseValue
                    |> Array.exists (fun i2 -> i1 |> BigRational.isMultiple i2)
                )
                |> ValueUnit.toUnit
                |> create


            let toString exact (Increment incr) =
                // ToDo extract
                let toStr =
                    if exact then ValueUnit.toStringEngShort
                    else
                        ValueUnit.toStringPrec 3
                $"{incr |> toStr}"


        module Minimum =

            /// Create a `Minimum` that is
            /// either inclusive or exclusive.
            let create isIncl vu =
                if vu |> ValueUnit.isSingleValue then
                    if isIncl then vu |> MinIncl else vu |> MinExcl
                else
                    failwith "a minimum can only be a single value"


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
                | MinIncl m2, MinExcl m1 -> m2 >? m1
                | MinExcl m2, MinIncl m1 -> m2 >=? m1

            /// Checks whether `Minimum` **m2** <= **m1**
            let minSTEmin m1 m2 = m2 |> minGTmin m1 |> not


            let minGTEmin min1 min2 = min1 = min2 || minGTmin min1 min2


            let minSTmin min1 min2 = min2 |> minGTEmin min1 |> not


            /// Checks whether `Minimum` is exclusive.
            let isExcl = function | MinIncl _ -> false | MinExcl _ -> true


            /// Checks whether `Minimum` is inclusive.
            let isIncl = isExcl >> not


            /// Creates a `Minimum` from a `ValueUnit`.
            /// Returns `None` if an empty set.
            let getSetMin s =
                if s |> ValueUnit.isEmpty then None
                else
                    s
                    |> ValueUnit.minElement
                    |> MinIncl
                    |> Some


            /// Convert a `Minimum` to a `ValueUnit`.
            let toValueUnit = function | MinIncl v | MinExcl v -> v


            /// Convert a `Minimum` to a `ValueUnit` and a `bool`
            /// that signifies inclusive or exclusive
            let toBoolValueUnit =
                apply (fun vu -> true, vu) (fun vu -> false, vu)


            let multipleOf incr min =
                let incr = incr |> Increment.toValueUnit
                match min |> toBoolValueUnit with
                | true, vu  -> vu |> ValueUnit.minInclMultipleOf incr
                | false, vu -> vu |> ValueUnit.minExclMultipleOf incr
                |> create true


            let checkTooSmall min =
                if min
                    |> toValueUnit
                    |> ValueUnit.denominator
                    |> Array.exists (fun x -> x > Constants.MAX_BIGINT) then
                    min
                    |> Exceptions.ValueRangeMinOverFlow
                    |> raiseExc []


            let nonZeroNonNeg =
                let fIncl vu =
                    let vu = vu |> ValueUnit.filter (fun br -> br > 0N)
                    if vu |> ValueUnit.isEmpty |> not then vu |> create true
                    else
                        vu
                        |> ValueUnit.setValue [|0N|]
                        |> create false

                let fExcl vu =
                    let vu = vu |> ValueUnit.filter (fun br -> br > 0N)
                    if vu |> ValueUnit.isEmpty |> not then vu |> create false
                    else
                        vu
                        |> ValueUnit.setValue [|0N|]
                        |> create false

                apply fIncl fExcl



            let restrict newMin oldMin =
                newMin |> checkTooSmall

                if newMin |> minGTmin oldMin then
                    newMin
                else
                    oldMin


            let toString exact min =
                let toStr =
                    if exact then ValueUnit.toStringEngShort
                    else
                        ValueUnit.toStringEngShort
                let b, vu =
                    min |> toBoolValueUnit

                $"""{if b then "[" else "<"}{vu |> toStr}"""


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
                | MaxExcl m2, MaxIncl m1 -> m2 >? m1
                | MaxIncl m2, MaxExcl m1 -> m2 >=? m1

            /// Checks whether `Maximum` **m2** <= **m1**
            let maxSTEmax m1 m2 = m2 |> maxGTmax m1 |> not


            let maxGTEmax max1 max2 = max1 = max2 || maxGTmax max1 max2


            let maxSTmax max1 max2 = max2 |> maxGTEmax max1 |> not

            /// Get the maximum value in a `BigRational` set.
            /// Returns `None` if an empty set.
            let getSetMax s =
                if s |> ValueUnit.isEmpty then None
                else
                    s
                    |> ValueUnit.maxElement
                    |> MaxIncl
                    |> Some

            /// Convert a `Maximum` to a `BigRational`.
            let toValueUnit = function | MaxIncl v | MaxExcl v -> v

            /// Checks whether `Maximum` is exclusive.
            let isExcl = function | MaxIncl _ -> false | MaxExcl _ -> true

            /// Checks whether `Maximum` is inclusive.
            let isIncl = isExcl >> not

            /// Turn a `Maximum` into a `BigRational` and a `bool` to indicate
            /// inclusive or exclusive.
            let toBoolValueUnit =
                apply (fun m -> true, m) (fun m -> false, m)


            let multipleOf incr max =
                let incr = incr |> Increment.toValueUnit
                match max |> toBoolValueUnit with
                | true, vu  -> vu |> ValueUnit.maxInclMultipleOf incr
                | false, vu -> vu |> ValueUnit.maxExclMultipleOf incr
                |> create true


            let checkTooLarge max =
                if max
                    |> toValueUnit
                    |> ValueUnit.numerator
                    |> Array.exists (fun x -> x > Constants.MAX_BIGINT) then
                    max
                    |> Exceptions.ValueRangeMaxOverFlow
                    |> raiseExc []


            let nonZeroNonNeg m =
                m
                |> toValueUnit
                |> ValueUnit.setSingleValue 0N
                |> Minimum.create false,
                m
                |> map
                    (ValueUnit.filter (fun br -> br > 0N))
                    (ValueUnit.filter (fun br -> br > 0N))


            let restrict newMax oldMax =
                newMax |> checkTooLarge

                if newMax |> maxSTmax oldMax then
                    newMax
                else
                    oldMax


            let toString exact max =
                let toStr =
                    if exact then ValueUnit.toStringEngShort
                    else
                        ValueUnit.toStringEngShort
                let b, vu =
                    max |> toBoolValueUnit

                $"""{vu |> toStr}{if b then "]" else ">"}"""



        module ValueSet =


            /// Create a `ValueSet` from a set of `BigRational`.
            let create vu =
                if vu |> ValueUnit.isEmpty then
                    Exceptions.ValueRangeEmptyValueSet
                    |> raiseExc []

                else
                    vu
                    |> ValueSet


            let toSet (ValueSet vs) = vs


            let map f (ValueSet vs) = vs |> f |> create

            // ToDo refactor to just ValueUnit.getMin
            let getMin (ValueSet vs) = vs |> Minimum.getSetMin

            // ToDo idem
            let getMax (ValueSet vs) = vs |> Maximum.getSetMax


            let count (ValueSet vs) = vs |> ValueUnit.getValue |> Array.length


            let isEmpty (ValueSet vs) = vs |> ValueUnit.isEmpty


            let contains v (ValueSet vs) = vs |> ValueUnit.containsValue v


            let intersect (ValueSet vs1) (ValueSet vs2) =
                vs1 |> ValueUnit.intersect vs2
                |> create


            let isSubset (ValueSet s1) (ValueSet s2) = ValueUnit.isSubset s1 s2


            // ToDo refactor
            let calc op (ValueSet s1) (ValueSet s2) =
                let count = ValueUnit.getValue >> Array.length
                // When one of the sets does not contain any value then the result of
                // of the calculation cannot contain any value either
                // if s1 |> Set.isEmpty || s2 |> Set.isEmpty then
                //     Exceptions.ValueRangeEmptyValueSet
                //     |> raiseExc
                // make sure the calculation doesn't take too long
                if (s1 |> count) + (s2 |> count) > Constants.MAX_CALC_COUNT then
                    (s1 |> count) + (s2 |> count)
                    |> Exceptions.ValueRangeTooManyValues
                    |> raiseExc []

                else
                    s1 |> op <| s2
                    |> create


            // ToDo refactor
            let toString exact (ValueSet vs) =
                let count = ValueUnit.getValue >> Array.length

                let toStr =
                    if exact then ValueUnit.toStringEngShort
                    else
                        ValueUnit.toStringPrec 3

                if vs |> count <= 10 then
                    $"""[{vs |> toStr}]"""
                else
                    let first3 = vs |> ValueUnit.takeFirst 3
                    let last3 = vs |> ValueUnit.takeLast 3
                    $"[{first3 |> toStr} .. {last3 |> toStr}]"


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


        open Informedica.Utils.Lib

        let map fMin fMax fMinMax fIncr fMinIncr fIncrMax fMinIncrMax fValueSet vr =
            match vr with
            | Unrestricted -> vr
            | NonZeroNoneNegative -> vr
            | Min min -> min |> fMin |> Min
            | Max max -> max |> fMax |> Max
            | MinMax (min, max) -> (min, max) |> fMinMax |> MinMax
            | Incr incr -> incr |> fIncr |> Incr
            | MinIncr (min, incr) -> (min, incr) |> fMinIncr |> MinIncr
            | IncrMax (incr, max) -> (incr, max) |> fIncrMax |> IncrMax
            | MinIncrMax (min, incr, max) -> (min, incr, max) |> fMinIncrMax |> MinIncrMax
            | ValSet vs -> vs |> fValueSet |> ValSet


        let apply unr nonz fMin fMax fMinMax fIncr fMinIncr fIncrMax fMinIncrMax fValueSet = function
            | Unrestricted -> unr
            | NonZeroNoneNegative -> nonz
            | Min min -> min |> fMin
            | Max max -> max |> fMax
            | MinMax (min, max) -> (min, max) |> fMinMax
            | Incr incr -> incr |> fIncr
            | MinIncr (min, incr) -> (min, incr) |> fMinIncr
            | IncrMax (incr, max) -> (incr, max) |> fIncrMax
            | MinIncrMax (min, incr, max) -> (min, incr, max) |> fMinIncrMax
            | ValSet vs -> vs |> fValueSet


        /// Count the number of values in a `ValueRange`.
        /// Returns 0 if no count is possible.
        let cardinality =
            let zero _ = 0
            apply 0 0 zero zero zero zero zero zero zero ValueSet.count


        /// Checks whether a `ValueRange` is `Unrestricted`
        let isUnrestricted =
            let returnFalse = Boolean.returnFalse

            apply
                true
                false
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse

        /// Checks whether a `ValueRange` is `Unrestricted`
        let isNonZeroOrNegative =
            let returnFalse = Boolean.returnFalse

            apply
                false
                true
                returnFalse
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
                false
                Boolean.returnTrue
                returnFalse
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
                false
                returnFalse
                Boolean.returnTrue
                returnFalse
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
                false
                returnFalse
                returnFalse
                Boolean.returnTrue
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse


        let isIncr =
            let returnFalse = Boolean.returnFalse

            apply
                false
                false
                returnFalse
                returnFalse
                returnFalse
                Boolean.returnTrue
                returnFalse
                returnFalse
                returnFalse
                returnFalse


        let isMinIncr =
            let returnFalse = Boolean.returnFalse

            apply
                false
                false
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                Boolean.returnTrue
                returnFalse
                returnFalse
                returnFalse


        let isIncrMax =
            let returnFalse = Boolean.returnFalse

            apply
                false
                false
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                Boolean.returnTrue
                returnFalse
                returnFalse


        let isMinIncrMax =
            let returnFalse = Boolean.returnFalse

            apply
                false
                false
                returnFalse
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
                false
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                returnFalse
                Boolean.returnTrue


        /// Checks whether a `BigRational` is between an optional
        /// **min** and an optional **max**
        let isBetweenMinMax min max br =
            let fMin = function
            | None -> true
            | Some(Minimum.MinIncl m) ->
                m
                |> ValueUnit.getBaseValue
                |> Array.forall (fun v -> br >= v)
            | Some(Minimum.MinExcl m) ->
                m
                |> ValueUnit.getBaseValue
                |> Array.forall (fun v -> br > v)

            let fMax  = function
            | None -> true
            | Some(Maximum.MaxIncl m) ->
                m
                |> ValueUnit.getBaseValue
                |> Array.forall (fun v -> br <= v)

            | Some(Maximum.MaxExcl m) ->
                m
                |> ValueUnit.getBaseValue
                |> Array.forall (fun v -> br < v)

            (fMin min) && (fMax max)


        let isMultipleOfIncr incrOpt br =
            let isDiv i = br |> BigRational.isMultiple i

            match incrOpt with
            | None -> true
            | Some (Increment incr) ->
                incr
                |> ValueUnit.getBaseValue
                |>  Seq.exists isDiv

        /// Filter a set of `BigRational` according
        /// to **min** **max** and incr constraints
        let filter minOpt incrOpt maxOpt (ValueSet vs) =
            vs
            |> ValueUnit.filter (fun v ->
                v |> isBetweenMinMax minOpt maxOpt &&
                v |> isMultipleOfIncr incrOpt
            )
            |> ValueSet.create


        let minEQmax max min =
            match min, max with
            | Minimum.MinIncl min, Maximum.MaxIncl max -> min =? max
            | _ -> false

        /// Checks whether `Minimum` **min** > `Maximum` **max**.
        /// Note that inclusivity or exclusivity of a minimum and maximum must be
        /// accounted for.
        let minGTmax max min =
            match min, max with
            | Minimum.MinIncl min, Maximum.MaxIncl max -> min >? max
            | Minimum.MinExcl min, Maximum.MaxIncl max
            | Minimum.MinExcl min, Maximum.MaxExcl max
            | Minimum.MinIncl min, Maximum.MaxExcl max -> min >=? max

        /// Checks whether `Minimum` **min** <= `Maximum` **max**
        let minSTEmax max min = min |> minGTmax max |> not


        let minMultipleOf incr min = min |> Minimum.multipleOf incr


        let maxMultipleOf incr max = max |> Maximum.multipleOf incr


        /// An `Unrestricted` `ValueRange`.
        let unrestricted = Unrestricted


        let nonZeroOrNegative = NonZeroNoneNegative


        /// Create a `MinMax` `ValueRange`. If **min** > **max** raises
        /// an `MinLargetThanMax` exception. If min equals max, a `ValueSet` with
        /// value min (= max).
        let minMaxToValueRange min max =
            if min |> minGTmax max then
                (min, max)
                |> Exceptions.Message.ValueRangeMinLargerThanMax
                |> raiseExc []

            elif min |> minEQmax max then
                min
                |> Minimum.toValueUnit
                |> ValueSet.create
                |> ValSet

            else (min, max) |> MinMax


        let minIncrToValueRange min incr =
            (min |> minMultipleOf incr, incr) |> MinIncr


        /// Create an `IncrMax` `ValueRange`.
        let incrMaxToValueRange incr max =
            (incr, max |> maxMultipleOf incr) |> IncrMax

        /// Create a `MinIncrMax` `ValueRange`. If **min** > **max** raises
        /// an `MinLargetThanMax` exception. If min equals max, a `ValueSet` with
        /// value min (=max).
        let minIncrMaxToValueRange onlyMinIncrMax min incr max =
            let min = min |> minMultipleOf incr
            let max = max |> maxMultipleOf incr

            if min |> minGTmax max then
                (min, max)
                |> Exceptions.Message.ValueRangeMinLargerThanMax
                |> raiseExc []
            else
                if onlyMinIncrMax && (min |> minEQmax max |> not) then
                    MinIncrMax(min, incr, max)
                else
                    let min = min |> Minimum.toValueUnit |> ValueUnit.getBaseValue
                    let max = max |> Maximum.toValueUnit |> ValueUnit.getBaseValue

                    incr
                    |> Increment.toValueUnit
                    |> ValueUnit.toBase
                    |> ValueUnit.applyToValue (fun incr ->
                        [|
                            for i in incr do
                                for mi in min do
                                    for ma in max do
                                        [|mi..i..ma|]
                        |]
                        |> Array.collect id
                    )
                    |> ValueUnit.toUnit
                    |> ValueSet.create
                    |> ValSet


        /// Create a `Minimum` `Range` that is
        /// either inclusive or exclusive.
        let createMin isIncl m = m |> Minimum.create isIncl |> Min


        /// Create a `Maximum` `Range` that is
        /// either inclusive or exclusive.
        let createMax isIncl m = m |> Maximum.create isIncl |> Max


        let createMinMax min minIncl max maxIncl =
            let min = min |> Minimum.create minIncl
            let max = max |> Maximum.create maxIncl

            minMaxToValueRange min max

        /// Create a `Range` with a `Minimum`, `Increment` and a `Maximum`.
        let createIncr = Increment.create >> Incr


        let createValSet brs = brs |> ValueSet.create |> ValSet


        /// Create a `MinIncr` `ValueRange`.
        let createMinIncr min minIncl incr =
            incr
            |> Increment.create
            |> minIncrToValueRange (Minimum.create min minIncl)


        let createIncrMax incr max maxIncl =
            max
            |> Maximum.create maxIncl
            |> incrMaxToValueRange (incr |> Increment.create)


        /// Create a `ValueRange` using a `ValueSet` **vs**
        /// an optional `Minimum` **min**, **incr** and `Maximum` **max**.
        /// If both **min**, **incr** and **max** are `None` an `Unrestricted`
        /// `ValueRange` is created.
        let create onlyMinIncrMax min incr max vs =
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
                | Some min, Some incr, Some max -> minIncrMaxToValueRange onlyMinIncrMax min incr max

            | Some vs ->
                vs
                |> filter min incr max
                |> ValSet

        /// Get an optional `Minimum` in a `ValueRange`
        let getMin =
            apply
                None
                None
                Some
                Option.none
                (fst >> Some)
                Option.none
                (fst >> Some)
                Option.none
                (Tuple.fstOf3 >> Some)
                ValueSet.getMin

        /// Get an optional `Maximum` in a `ValueRange`
        let getIncr =
            apply
                None
                None
                Option.none
                Option.none
                Option.none
                Some
                (snd >> Some)
                (fst >> Some)
                (Tuple.sndOf3 >> Some)
                Option.none

        /// Get an optional `Maximum` in a `ValueRange`
        let getMax =
            apply
                None
                None
                Option.none
                Some
                (snd >> Some)
                Option.none
                Option.none
                (snd >> Some)
                (Tuple.thrdOf3 >> Some)
                ValueSet.getMax

        /// Get an optional `ValueSet` in a `ValueRange`
        let getValSet =
            apply
                None
                None
                Option.none
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
                v |> ValueUnit.getBaseValue |> Array.forall (isBetweenMinMax min max) &&
                v |> ValueUnit.getBaseValue |> Array.forall (isMultipleOfIncr incr)


        /// Apply a `Minimum` **min** to a `ValueRange` **vr**.
        /// If minimum cannot be set the original `Minimum` is returned.
        /// So, it always returns a more restrictive, i.e. larger, or equal `Minimum`.
        let setMin onlyMinIncrMax newMin (vr: ValueRange) =
            let restrict = Minimum.restrict newMin

            let nonZero = newMin |> Minimum.nonZeroNonNeg |> Min

            let fMin = restrict >> Min

            let fMax max = minMaxToValueRange newMin max

            let fMinMax (min, max) = minMaxToValueRange (min |> restrict) max

            let fIncr incr = minIncrToValueRange newMin incr

            let fMinIncr (min, incr) =
                minIncrToValueRange (min |> restrict) incr

            let fIncrMax (incr, max) =
                minIncrMaxToValueRange onlyMinIncrMax newMin incr max

            let fMinIncrMax(min, incr, max) =
                minIncrMaxToValueRange onlyMinIncrMax (min |> restrict) incr max

            let fValueSet =
                filter (Some newMin) None None >> ValSet

            vr
            |> apply
                (newMin |> Min)
                nonZero
                fMin
                fMax
                fMinMax
                fIncr
                fMinIncr
                fIncrMax
                fMinIncrMax
                fValueSet


        /// Apply a **incr** to a `ValueRange` **vr**.
        /// If increment cannot be set the original is returned.
        /// So, the resulting increment is always more restrictive as the previous one
        let setIncr onlyMinIncrMax newIncr vr =
            let restrict = Increment.restrict newIncr

            let nonZero =
                newIncr
                |> Increment.toValueUnit
                |> ValueUnit.setSingleValue 0N
                |> Minimum.create false,
                newIncr


            let fMin min = minIncrToValueRange min newIncr

            let fMax max = incrMaxToValueRange newIncr max

            let fMinMax (min, max) = minIncrMaxToValueRange onlyMinIncrMax min newIncr max

            let fIncr = restrict >> Incr

            let fMinIncr (min, incr) =
                minIncrToValueRange min (incr |> restrict)

            let fIncrMax (incr, max) =
                incrMaxToValueRange (incr |> restrict) max

            let fMinIncrMax (min, incr, max) =
                minIncrMaxToValueRange onlyMinIncrMax min (incr |> restrict) max

            let fValueSet =
                filter None (Some newIncr) None >> ValSet

            vr
            |> apply
                (newIncr |> Incr)
                (nonZero |> MinIncr)
                fMin
                fMax
                fMinMax
                fIncr
                fMinIncr
                fIncrMax
                fMinIncrMax
                fValueSet


        let setMax onlyMinIncrMax newMax (vr: ValueRange) =
            let restrict = Maximum.restrict newMax

            let nonZero = newMax |> Maximum.nonZeroNonNeg |> MinMax

            let fMin min = minMaxToValueRange min newMax

            let fMax max = max |> restrict |> Max

            let fMinMax (min, max) = minMaxToValueRange min (max |> restrict)

            let fIncr incr = incrMaxToValueRange incr newMax

            let fMinIncr (min, incr) =
                minIncrMaxToValueRange onlyMinIncrMax min incr newMax

            let fIncrMax (incr, max) =
                incrMaxToValueRange incr (max |> restrict)

            let fMinIncrMax (min, incr, max) =
                minIncrMaxToValueRange onlyMinIncrMax min incr (max |> restrict)

            let fValueSet =
                filter None None (Some newMax) >> ValSet

            vr
            |> apply
                (newMax |> Max)
                nonZero
                fMin
                fMax
                fMinMax
                fIncr
                fMinIncr
                fIncrMax
                fMinIncrMax
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

            let nonZero = print exact None None None None

            let print min incr max = print exact min incr max None

            let fMin min = print (Some min) None None

            let fMax max = print None None (Some max)

            let fIncr incr = print None (Some incr) None

            let fMinIncr (min, incr)  = print (Some min) (Some incr) None

            let fIncrMax (incr, max)  = print None (Some incr) (Some max)

            let fMinIncrMax (min, incr, max) = print (Some min) (Some incr) (Some max)

            let fMinMax (min, max) = print (Some min) None (Some max)

            vr
            |> apply
                unr
                nonZero
                fMin
                fMax
                fMinMax
                fIncr
                fMinIncr
                fIncrMax
                fMinIncrMax
                fVs

        /// Functions to calculate the `Minimum`
        /// and `Maximum` in a `ValueRange`.
        /// I.e. what happens when you mult, div, add or subtr
        /// a `Range`, for example:
        /// <1N..3N> * <4N..5N> = <4N..15N>
        module MinMaxCalculator =


            /// Calculate **x1** and **x2** with operator **op**
            /// and use **incl1** and **inc2** to determine whether
            /// the result is inclusive. Use constructor **c** to
            /// create the optional result.
            let calc c op (x1, incl1) (x2, incl2) =
                // printfn "start minmax calc"
                let opIsMultOrDiv = (op |> ValueUnit.Operators.opIsMult || op |> ValueUnit.Operators.opIsDiv)

                let incl =
                    match incl1, incl2 with
                    | true, true -> true
                    | _ -> false
                // printfn "start minmax calc match"
                match x1, x2 with
                | Some v, _  when opIsMultOrDiv && v |> ValueUnit.isZero ->
                    let v = v |> ValueUnit.setSingleValue 0N
                    v |> c incl1 |> Some
                | Some v, _
                | _, Some v when op |> ValueUnit.Operators.opIsMult && v |> ValueUnit.isZero ->
                    let v = v |> ValueUnit.setSingleValue 0N
                    v |> c incl |> Some
                | Some v, None when op |> ValueUnit.Operators.opIsDiv ->
                    let v = v |> ValueUnit.setSingleValue 0N
                    v |> c incl |> Some
                | Some v1, Some v2 ->
                    if op |> ValueUnit.Operators.opIsDiv && v2 |> ValueUnit.isZero then None
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
                | Some min, _         when min |> ValueUnit.gtZero             -> PP
                | _,        Some max  when max |> ValueUnit.stZero             -> NN
                | Some min, Some max  when min |> ValueUnit.stZero &&
                                           max |> ValueUnit.gtZero             -> NP
                | None,     Some max  when max |> ValueUnit.gtZero             -> NP
                | Some min, None      when min |> ValueUnit.stZero             -> NP
                | None,     None                                               -> NP
                | _,        Some max  when max |> ValueUnit.isZero             -> NZ
                | Some min, _         when min |> ValueUnit.isZero             -> ZP
                // failing cases
                | Some min, Some max when min |> ValueUnit.isZero && max |> ValueUnit.isZero  ->
                    //printfn "failing case"
                    $"{min} = {max} = 0"
                    |> Exceptions.ValueRangeMinMaxException
                    |> Exceptions.raiseExc None []

                | Some min, Some max when min |> ValueUnit.gteZero && max |> ValueUnit.stZero ->
                    $"{min} > {max}"
                    |> Exceptions.ValueRangeMinMaxException
                    |> Exceptions.raiseExc None []

                | _ ->
                    printfn "could not handel failing case"

                    $"could not handle {min} {max}"
                    |> Exceptions.ValueRangeMinMaxException
                    |> Exceptions.raiseExc None []



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
                //printfn "start multiplication"
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
            let calcMinMax op =
                match op with
                | ValueUnit.Operators.Mult  -> multiplication
                | ValueUnit.Operators.Div   -> division
                | ValueUnit.Operators.Add   -> addition
                | ValueUnit.Operators.Subtr -> subtraction


        /// Applies an infix operator **op**
        /// to `ValueRange` **x1** and **x2**.
        /// Calculates `Minimum`, increment or `Maximum`
        /// if either **x1** or **x2** is not a `ValueSet`.
        /// Doesn't perform any calculation when both
        /// **x1** and **x2** are `Unrestricted`.
        let calc onlyMinIncrMax op (x1, x2) =
            //printfn "start valuerange calc"
            let calcMinMax min1 max1 min2 max2 =
                //printfn "start minmax calc"
                let getMin m =
                    let incl =
                        match m with
                        | Some v -> v |> Minimum.isIncl
                        | None   -> false
                    m |> Option.bind (Minimum.toValueUnit >> Some), incl

                let getMax m =
                    let incl =
                        match m with
                        | Some v -> v |> Maximum.isIncl
                        | None   -> false
                    m |> Option.bind (Maximum.toValueUnit >> Some), incl

                MinMaxCalculator.calcMinMax
                    op
                    (min1 |> getMin)
                    (max1 |> getMax)
                    (min2 |> getMin)
                    (max2 |> getMax)

            match x1, x2 with
            | Unrestricted, Unrestricted -> unrestricted
            | ValSet s1, ValSet s2 ->
                let min1, max1 = x1 |> getMin, x1 |> getMax
                let min2, max2 = x2 |> getMin, x2 |> getMax

                let min, max = calcMinMax min1 max1 min2 max2

                if not onlyMinIncrMax then ValueSet.calc op s1 s2 |> ValSet
                else
                    match min, max with
                    | None, None -> unrestricted
                    | _ -> create onlyMinIncrMax min None max None

            // A set with an increment results in a new set of increment
            // Need to match all scenarios with a valueset and an increment
            | ValSet s, Incr i
            | Incr i, ValSet s

            | ValSet s, MinIncr(_, i)
            | MinIncr(_, i), ValSet s

            | ValSet s, IncrMax(i, _)
            | IncrMax(i, _), ValSet s

            | ValSet s, MinIncrMax(_, i, _)
            | MinIncrMax(_, i, _), ValSet s ->

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
                | _ -> create onlyMinIncrMax min incr max None

            // In any other case calculate min, incr and max
            | _ ->
                let min1, incr1, max1 = x1 |> getMin, x1 |> getIncr, x1 |> getMax
                let min2, incr2, max2 = x2 |> getMin, x2 |> getIncr, x2 |> getMax

                let min, max = calcMinMax min1 max1 min2 max2

                // calculate a new increment based upon the incr1 and incr2
                let incr = Increment.calcOpt op incr1 incr2

                match min, incr, max with
                | None, None, None -> unrestricted
                | _ -> create onlyMinIncrMax min incr max None

        /// Checks whether a `ValueRange` vr1 is a subset of
        /// `ValueRange` vr2.
        let isSubSetOf vr2 vr1 =
            match vr1, vr2 with
            | ValSet s1, ValSet s2 ->
                s2 |> ValueSet.isSubset s1
            | _ -> false


        let toProperties vr =
            let unr = set []

            let nonZero = set []

            let fMin min = set [ min |> MinProp ]

            let fMax max = set [ max |> MaxProp ]

            let fMinMax (min, max) = set [ min |> MinProp; max |> MaxProp ]

            let fIncr incr = set [ incr |> IncrProp ]

            let fMinIncr (min, incr) = set [ min |> MinProp; incr |> IncrProp ]

            let fIncrMax (incr, max) = set [ incr |> IncrProp; max |> MaxProp ]

            let fMinIncrMax (min, incr, max) = set [ min |> MinProp; incr |> IncrProp; max |> MaxProp ]

            let fVs vs = set [ vs |> ValsProp ]

            vr
            |> apply
                unr
                nonZero
                fMin
                fMax
                fMinMax
                fIncr
                fMinIncr
                fIncrMax
                fMinIncrMax
                fVs


        let diffWith vr1 vr2 =
            vr1
            |> toProperties
            |> Set.difference (vr2 |> toProperties)


        /// Set a `ValueRange` expr to a `ValueRange` y.
        /// So, the result is equal to or more restrictive than the original `y`.
        let applyExpr onlyMinIncrMax y expr =
            let set get set vr =
                match expr |> get with
                | Some m -> vr |> set m
                | None   -> vr

            match expr with
            | Unrestricted -> y
            | ValSet vs    -> y |> setValueSet vs
            | _ ->
                y
                |> set getMin (setMin onlyMinIncrMax)
                |> set getIncr (setIncr onlyMinIncrMax)
                |> set getMax (setMax onlyMinIncrMax)


        module Operators =

            let inline (^*) vr1 vr2 = calc false (*) (vr1, vr2)

            let inline (^/) vr1 vr2 = calc false (/) (vr1, vr2)

            let inline (^+) vr1 vr2 = calc false (+) (vr1, vr2)

            let inline (^-) vr1 vr2 = calc false (-) (vr1, vr2)

            let inline (^<-) vr1 vr2 = applyExpr false vr1 vr2


            let inline (@*) vr1 vr2 = calc true (*) (vr1, vr2)

            let inline (@/) vr1 vr2 = calc true (/) (vr1, vr2)

            let inline (@+) vr1 vr2 = calc true (+) (vr1, vr2)

            let inline (@-) vr1 vr2 = calc true (-) (vr1, vr2)

            let inline (@<-) vr1 vr2 = applyExpr true vr1 vr2



    open Informedica.Utils.Lib.BCL
    open ValueRange.Operators

    module Minimum = ValueRange.Minimum
    module Maximum = ValueRange.Maximum


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
    let setValueRange onlyMinIncrMax v vr =
        let op = if onlyMinIncrMax then (@<-) else (^<-)

        try
            { v with
                Values = (v |> get).Values |> op <| vr
            }

        with
        | Exceptions.SolverException errs ->
            (v, vr)
            |> Exceptions.VariableCannotSetValueRange
            |> raiseExc errs

    /// Set the values to a `ValueRange`
    /// that prevents zero or negative values.
    let setNonZeroOrNegative v =
        { v with Values = NonZeroNoneNegative }

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
        try
            (v1 |> getValueRange) |> op <| (v2 |> getValueRange)
            |> createRes
        with
        | Exceptions.SolverException errs ->
            (v1, op, v2)
            |> Exceptions.VariableCannotCalcVariables
            |> raiseExc errs
        | e ->
            printfn "unrecognized error with calc operation"
            raise e


    module Operators =

        let inline (^*) vr1 vr2 = calc (^*) (vr1, vr2)

        let inline (^/) vr1 vr2 = calc (^/) (vr1, vr2)

        let inline (^+) vr1 vr2 = calc (^+) (vr1, vr2)

        let inline (^-) vr1 vr2 = calc (^-) (vr1, vr2)

        let inline (^<-) vr1 vr2 =
            try
                { vr1 with Values = (vr1 |> getValueRange) ^<- (vr2 |> getValueRange) }
            with
            | Exceptions.SolverException errs ->
                (vr1, vr2 |> getValueRange)
                |> Exceptions.VariableCannotSetValueRange
                |> Exceptions.raiseExc None errs


        let inline (@*) vr1 vr2 = calc (@*) (vr1, vr2)

        let inline (@/) vr1 vr2 = calc (@/) (vr1, vr2)

        let inline (@+) vr1 vr2 = calc (@+) (vr1, vr2)

        let inline (@-) vr1 vr2 = calc (@-) (vr1, vr2)

        let inline (@<-) vr1 vr2 =
            try
                { vr1 with Values = (vr1 |> getValueRange) @<- (vr2 |> getValueRange) }
            with
            | Exceptions.SolverException errs ->
                (vr1, vr2 |> getValueRange)
                |> Exceptions.VariableCannotSetValueRange
                |> Exceptions.raiseExc None errs


        /// Constant 0
        let zero =
            ValueUnit.Operators.zero
            |> ValueRange.createValSet
            |> createSucc (Name.createExc "zero")

        /// Constant 1
        let one =
            ValueUnit.Operators.one
            |> ValueRange.createValSet
            |> createSucc (Name.createExc "one")

        /// Constant 2
        let two =
            ValueUnit.Operators.two
            |> ValueRange.createValSet
            |> createSucc (Name.createExc "two")

        /// Constant 3
        let three =
            ValueUnit.Operators.three
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

        open Informedica.Utils.Lib

        module ValueSet = ValueRange.ValueSet
        module Increment = ValueRange.Increment

        /// The `Dto` representation of a `Variable`
        type Dto =
            {
                Name: string
                Min: ValueUnit.Dto.Dto option
                MinIncl: bool
                Incr : ValueUnit.Dto.Dto option
                Max: ValueUnit.Dto.Dto option
                MaxIncl: bool
                Vals: ValueUnit.Dto.Dto option
            }

        let isUnr (dto : Dto) =
            dto.Min.IsNone && dto.Max.IsNone &&
            dto.Incr.IsNone && dto.Vals.IsNone

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
        let createNew n = createDto n None false None None false None

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

            match p with
            | Vals     -> dto |> setVals vs
            | Incr     -> dto |> setIncr vs
            | MinIncl  -> dto |> setMin  vs true
            | MinExcl  -> dto |> setMin  vs false
            | MaxIncl  -> dto |> setMax  vs true
            | MaxExcl  -> dto |> setMax  vs false
            | _   -> dto


        /// Create a `Variable` from a `Dto` and
        /// raise a `DtoException` if this fails.
        let fromDto (dto: Dto) =
            let succ = id

            let n = dto.Name |> Name.create succ (fun m -> m |> raiseExc [])

            let vs = dto.Vals |> Option.bind (fun v -> v |> ValueUnit.Dto.fromDto |> Option.map ValueSet.create)
            let min = dto.Min |> Option.bind (fun v -> v |> ValueUnit.Dto.fromDto |> Option.map (Minimum.create dto.MinIncl))
            let max = dto.Max |> Option.bind (fun v -> v |> ValueUnit.Dto.fromDto |> Option.map  (Maximum.create dto.MaxIncl))
            let incr = dto.Incr |> Option.bind (fun v -> v |> ValueUnit.Dto.fromDto |> Option.map (Increment.create))

            let vr = ValueRange.create true min incr max vs

            create succ n vr


        /// Return a `string` representation of a `Dto`
        let toString exact = fromDto >> toString exact


        (*
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
                let vr = ValueRange.create true min incr max vs

                match n with
                | Some n' -> create succ n' vr
                | _ -> None
            with _ -> None
        *)


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
                |> Option.map Minimum.toValueUnit
                |> Option.bind (ValueUnit.Dto.toDto true ValueUnit.Dto.english)

            let max  =
                v.Values
                |> ValueRange.getMax
                |> Option.map Maximum.toValueUnit
                |> Option.bind (ValueUnit.Dto.toDto true ValueUnit.Dto.english)

            let vals =
                v.Values
                |> ValueRange.getValSet
                |> function
                | Some (ValueSet vs) -> vs |> ValueUnit.Dto.toDto true ValueUnit.Dto.english
                | None -> None

            { dto with
                Vals = vals
                Min = min
                MinIncl = minincl
                Max = max
                MaxIncl = maxincl }


/// Functions that handle the `Equation` type that
/// either represents a `ProductEquation` </br>
/// y = x1 \* x2 * ... \* xn </br>
/// or a `SumEquations` </br>
/// y = x1 + x2 + ... + xn
module Equation =

    open Informedica.Utils.Lib

    open Types
    open Variable.Operators

    module Name = Variable.Name
    module ValueRange = Variable.ValueRange

    module SolveResult =


        module Property = Variable.ValueRange.Property

        let toString = function
            | Unchanged -> "Unchanged"
            | Changed cs ->
                let toStr (var : Variable, props)  =
                    $"""changes: {var.Name |> Variable.Name.toString}: {props |> Set.map (Property.toString true) |> String.concat ", "}"""
                if cs |> List.isEmpty then ""
                else
                    cs
                    |> List.map toStr
                    |> String.concat ", "
            | Errored ms ->
                ms
                |> List.map string
                |> String.concat ", "


    /// Create an `Equation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    /// The type of Equation product or sum
    /// is determined by the constructor **c**.
    let create c succ fail (y, xs) =
        y::xs
        |> List.filter (fun v ->
            y::xs
            |> List.filter (Variable.eqName v) |> List.length > 1)
        |> function
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
    let createProductEqExc = createProductEq id (Exceptions.raiseExc None [])

    /// Create an `SumEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time raising an exception.
    let createSumEqExc = createSumEq id (Exceptions.raiseExc None [])

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


    let count eq =
        let vars = eq |> toVars
        let b =
            let n =
                vars
                |> List.filter Variable.isSolved
                |> List.length
            (vars |> List.length) - n = 1
        if b then -100
        else
            eq
            |> toVars
            |> List.fold (fun (acc : int) v ->
                (+) (v |> Variable.count) acc
            ) 0


    let countProduct eq =
        //match eq with
        //| SumEquation _ -> -1
        //| _ ->
        eq
        |> toVars
        |> List.fold (fun acc v ->
            let c = v |> Variable.count
            (if c = 0 then 1 else c) * acc
        ) 1


    let toString exact eq =
        let op = if eq |> isProduct then " * " else " + "
        let varToString = Variable.toString exact

        match eq |> toVars with
        | [] -> ""
        | [ _ ] -> ""
        | y::xs ->
            $"""{y |> varToString} = {xs |> List.map varToString |> String.concat op}"""


    /// Make sure that the `Variables` in the
    /// `Equation` can only contain positive
    /// non zero values.
    let nonZeroOrNegative eq =
        let set c y xs =
            let y = y |> Variable.setNonZeroOrNegative
            let xs = xs |> List.map Variable.setNonZeroOrNegative
            (y, xs) |> c
        let fp = set ProductEquation
        let fs = set SumEquation
        eq |> apply fp fs

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
    let find var eq =
        eq
        |> toVars
        |> List.filter (fun v -> v |> Variable.getName = (var |> Variable.getName))

    /// Find a `Variable` with `Name`
    /// **n** in an `Equation` **eq**
    /// and return the result as a list
    let findName n eq =
        eq
        |> toVars
        |> List.filter (fun vr -> vr |> Variable.getName = n)

    /// Replace a `Variable` **v** in the
    /// `Equation` **e**.
    let replace var eq =
        let r c v vs =
            let vs = vs |> List.replace (Variable.eqName v) v
            c id (fun _ -> eq) ((vs |> List.head), (vs|> List.tail))
        let fp y xs = r createProductEq var (y::xs)
        let fs y xs = r createSumEq var (y::xs)
        eq |> apply fp fs


    // Check whether an equation is solved
    let isSolved = function
        | ProductEquation (y, xs)
        | SumEquation (y, xs) ->
            y::xs |> List.forall Variable.isSolved


    // Check whether an equation will change by calc
    // This is not the same as `isSolved`!! If all
    // the variables are unrestricted than the equation
    // is not solvable but is also not solved.
    let isSolvable = function
        | ProductEquation (y, xs)
        | SumEquation (y, xs) ->
            let es = y::xs
            es |> List.exists Variable.isSolvable &&
            es |> List.filter Variable.isUnrestricted
               |> List.length > 1
               |> not


    let check eq =
        let isSub op (y : Variable) (xs : Variable list) =
            match xs with
            | [] -> true
            | _  ->
                if y.Values |> ValueRange.isValueSet &&
                   xs |> List.map Variable.getValueRange
                      |> List.forall ValueRange.isValueSet then

                    y.Values
                    |> ValueRange.isSubSetOf (xs |> List.reduce op).Values

                else true

        if eq |> isSolvable then
            match eq with
            | ProductEquation (y, xs) -> xs |> isSub (^*) y
            | SumEquation (y, xs) -> xs |> isSub (^+) y

        else true

    let calculationToString op1 op2 x y xs =
        let varToStr = Variable.toString true
        let opToStr op  = $" {op |> Variable.Operators.toString} "
        let filter x xs = xs |> List.filter (Variable.eqName x >> not)

        $"""{x |> varToStr} = {y |> varToStr}{op2 |> opToStr}{xs |> filter x |> List.map varToStr |> String.concat (op1 |> opToStr)} """


    /// Solve an equation **e**, return a list of
    /// changed `Variable`s.
    let solve onlyMinIncrMax log eq =
        // helper functions
        let without x xs = xs |> List.filter (Variable.eqName x >> not)
        let replAdd x xs = xs |> List.replaceOrAdd(Variable.eqName x) x

        let (<==) = if onlyMinIncrMax then (@<-) else (^<-)

        let rec calcXs op1 op2 y xs rest changed =
            match rest with
            | []  ->
                // log finishing the calculation
                (y::xs, changed)
                |> Events.EquationFinishedCalculation
                |> Logging.logInfo log
                // return the result and whether this is changed
                xs, changed

            | x::tail ->
                let newX =
                    match xs |> without x with
                    | [] ->  x <== y
                    | _  ->
                        if x |> Variable.isSolved then x
                        else
                            // log the calculation
                            (op1, op2, x, y, xs)
                            |> Events.EquationStartCalculation
                            |> Logging.logInfo log
                            // recalculate x
                            x <== (y |> op2 <| (xs |> without x |> List.reduce op1))

                (changed || (x.Values <> newX.Values))
                |> calcXs op1 op2 y (xs |> replAdd newX) tail

        let calcY op1 y xs =
            if y |> Variable.isSolved then y, false
            else
                // log the calculation
                (op1, op1, y, (xs |> List.head), (xs |> List.tail))
                |> Events.EquationStartCalculation
                |> Logging.logInfo log
                // recalculate y
                let temp = xs |> List.reduce op1
                let newY = y <== temp //(xs |> List.reduce op1)

                let yChanged = newY.Values <> y.Values

                // log finishing the calculation
                (newY::xs, yChanged)
                |> Events.EquationFinishedCalculation
                |> Logging.logInfo log
                // return the result and whether it changed
                newY, yChanged
        // op1 = (*) or (+) and op2 = (/) or (-)
        let rec loop op1 op2 y xs changed =
            let y, yChanged, xs, xChanged =
                // for performance reasons pick the most efficient order of
                // calculations, first xs then y or vice versa.
                if xs |> List.forall (Variable.count >> ((<) (y |> Variable.count))) then
                    // Calculate x1 = y op2 (x2 op1 x3 .. op1 xn)
                    //       and x2 = y op2 (x1 op1 x3 .. op1 xn)
                    //       etc..
                    let xs, xChanged = calcXs op1 op2 y xs xs false
                    // Calculate y = x1 op1 x2 op1 .. op1 xn
                    let y, yChanged = calcY op1 y xs

                    y, yChanged, xs, xChanged
                else
                    // Calculate y = x1 op1 x2 op1 .. op1 xn
                    let y, yChanged = calcY op1 y xs
                    // Calculate x1 = y op2 (x2 op1 x3 .. op1 xn)
                    //       and x2 = y op2 (x1 op1 x3 .. op1 xn)
                    //       etc..
                    let xs, xChanged = calcXs op1 op2 y xs xs false

                    y, yChanged, xs, xChanged

            // If something has changed restart until nothing changes anymore
            if not (yChanged || xChanged) then (y, xs, changed)
            else
                // equation has changed so loop
                loop op1 op2 y xs true

        let calcResult (y, xs, isChanged) =
            let result =
                // nothing has changed!
                if not isChanged then Unchanged
                // calculate the changes
                else
                    let vars = eq |> toVars

                    y::xs
                    |> List.map (fun v2 ->
                        vars
                        |> List.tryFind (Variable.eqName v2)
                        |> function
                        | Some v1 ->
                            v2, v2.Values
                            |> Variable.ValueRange.diffWith v1.Values
                        | None ->
                            $"cannot find {v2}! in {vars}!"
                            |> failwith
                    )
                    |> List.filter (snd >> Set.isEmpty >> not)
                    |> Changed

            let eq =
                match eq with
                | ProductEquation _ -> createProductEqExc (y, xs)
                | SumEquation _     -> createSumEqExc (y, xs)
            // log finishing equation solving
            (eq, result)
            |> Events.EquationFinishedSolving
            |> Logging.logInfo log
            // return the eq and solve result
            eq, result

        if eq |> isSolved then eq, Unchanged
        else
            // log starting the equation solve
            eq
            |> Events.EquationStartedSolving
            |> Logging.logInfo log
            // get the right operators
            let y, xs, op1, op2 =
                match eq with
                | ProductEquation (y, xs) ->
                    if onlyMinIncrMax then
                        y, xs, (@*), (@/)
                    else
                        y, xs, (^*), (^/)
                | SumEquation (y, xs) ->
                    if onlyMinIncrMax then
                        y, xs, (@+), (@-)
                    else
                        y, xs, (^+), (^-)

            match xs with
            | [] -> eq, Unchanged
            | _  ->
                try
                    loop op1 op2 y xs false
                    |> calcResult
                with
                | Exceptions.SolverException errs ->
                    errs
                    |> List.iter (Logging.logError log)

                    eq, Errored errs


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
            | [ _ ] -> ""
            | y::xs ->
                let s =
                    $"%s{y |> varToString} = " +
                    (xs |> List.fold (fun s v -> s + (v |> varToString) + " " + op + " ") "")
                s.Substring(0, s.Length - 2)


        /// Create a `Dto` and raise an exception if it fails
        let fromDto dto =
            let succ = id
            let fail = Exceptions.raiseExc None []

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
    module Name = Variable.Name

    open Types

    let sortByName eqs =
        eqs
        |> List.sortBy (fun e ->
            e
            |> Equation.toVars
            |> List.head
            |> Variable.getName)


    /// Format a set of equations to print.
    /// Using **f** to allow additional processing
    /// of the string.
    let printEqs exact pf eqs =

        "equations result:\n" |> pf
        eqs
        |> sortByName
        |> List.map (Equation.toString exact)
        |> List.iteri (fun i s -> $"%i{i}.\t%s{s}"  |> pf)
        "-----" |> pf

        eqs


    /// Checks whether a list of `Equation` **eqs**
    /// contains an `Equation` **eq**
    let contains eq eqs = eqs |> List.exists ((=) eq)


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
        |> List.fold (fun acc v ->
            acc
            |> List.map (Equation.replace v)
        ) rpl
        , rst


    let memSolve f =
        let cache = ref Map.empty
        fun e ->
            match cache.Value.TryFind(e) with
            | Some r -> r
            | None ->
                let r = f e
                cache.Value <- cache.Value.Add(e, r)
                r

    let sortQue que =
        if que |> List.length = 0 then que
        else
            que
            |> List.sortBy Equation.count //Equation.countProduct


    /// Create the equation solver using a
    /// product equation and a sum equation solver
    /// and function to determine whether an
    /// equation is solved
    let solve onlyMinIncrMax log sortQue var eqs =

        let solveE n eqs eq =
            try
                Equation.solve onlyMinIncrMax log eq
            with
            | Exceptions.SolverException errs ->
                (n, errs, eqs)
                |> Exceptions.SolverErrored
                |> Exceptions.raiseExc None errs
            | e ->
                let msg = $"didn't catch {e}"
                printfn $"{msg}"
                msg |> failwith

        let rec loop n que acc =
            match acc with
            | Error _ -> acc
            | Ok acc  ->
                let n = n + 1
                if n > ((que @ acc |> List.length) * Constants.MAX_LOOP_COUNT) then
                    printfn $"too many loops: {n}"
                    (n, que @ acc)
                    |> Exceptions.SolverTooManyLoops
                    |> Exceptions.raiseExc None []

                let que = que |> sortQue

                //(n, que)
                //|> Events.SolverLoopedQue
                //|> Logging.logInfo log

                match que with
                | [] ->
                    match acc |> List.filter (Equation.check >> not) with
                    | []      -> acc |> Ok
                    | invalid ->
                        printfn "invalid equations"
                        invalid
                        |> Exceptions.SolverInvalidEquations
                        |> Exceptions.raiseExc None []

                | eq::tail ->
                    // need to calculate result first to enable tail call optimization
                    let q, r =
                        // If the equation is already solved, or not solvable
                        // just put it to  the accumulated equations and go on with the rest
                        if eq |> Equation.isSolvable |> not then
                            tail,
                            [ eq ]
                            |> List.append acc
                            |> Ok
                        // Else go solve the equation
                        else
                            match eq |> solveE n (acc @ que) with
                            // Equation is changed, so every other equation can
                            // be changed as well (if changed vars are in the other
                            // equations) so start new
                            | eq, Changed cs ->
                                let vars = cs |> List.map fst
                                // find all eqs with vars in acc and put these back on que
                                acc
                                |> replace vars
                                |> function
                                | rpl, rst ->
                                    // replace vars in tail
                                    let que =
                                        tail
                                        |> replace vars
                                        |> function
                                        | es1, es2 ->
                                            es1
                                            |> List.append es2
                                            |> List.append rpl

                                    que,
                                    rst
                                    |> List.append [ eq ]
                                    |> Ok

                            // Equation did not in fact change, so put it to
                            // the accumulated equations and go on with the rest
                            | eq, Unchanged ->
                                tail,
                                [eq]
                                |> List.append acc
                                |> Ok

                            | eq, Errored m ->
                                [],
                                [eq]
                                |> List.append acc
                                |> List.append que
                                |> fun eqs ->
                                    Error (eqs, m)
                    loop n q r

        match var with
        | None     -> eqs, []
        | Some var -> eqs |> replace [var]
        |> function
        | rpl, rst ->
            rpl
            |> Events.SolverStartSolving
            |> Logging.logInfo log

            try
                match rpl with
                | [] -> eqs |> Ok
                | _  -> loop 0 rpl (Ok rst)
            with
            | Exceptions.SolverException errs  ->
                 Error (rpl @ rst, errs)
            | e ->
                let msg = $"something unexpected happened, didn't catch {e}"
                printfn $"{msg}"
                msg |> failwith

            |> function
            | Ok eqs ->
                eqs
                |> Events.SolverFinishedSolving
                |> Logging.logInfo log

                eqs |> Ok
            | Error (eqs, m) ->
                eqs
                |> Events.SolverFinishedSolving
                |> Logging.logInfo log

                Error (eqs, m)


    let solveVariable onlyMinIncrMax log sortQue vr eqs =
        solve onlyMinIncrMax log sortQue (Some vr) eqs


    let solveAll onlyMinIncrMax log eqs =
        solve onlyMinIncrMax log sortQue None eqs



module Constraint =

    open Types

    module ValueRange = Variable.ValueRange
    module Property = ValueRange.Property
    module ValueSet = ValueRange.ValueSet
    module Name = Variable.Name


    let eqsName (c1 : Constraint) (c2 : Constraint) = c1.Name = c2.Name


    let toString { Name = n; Property = p } = $"{n |> Name.toString}: {p}"


    let scoreConstraint c =
            match c.Property with
            | ValsProp vs ->
                let n = vs |> ValueSet.count
                if n = 1 then    -3, c
                else              n, c
            | MinProp _   -> -5, c
            | IncrProp _      -> -4, c
            | _               -> -2, c


    let orderConstraints log cs =
        cs
        // calc min and max from valsprop constraints
        |> List.fold (fun acc c ->
            match c.Property with
            | ValsProp vs ->
                if vs |> ValueSet.count <= 1 then [c] |> List.append acc
                else
                    let min = vs |> ValueSet.getMin |> Option.map MinProp
                    let max = vs |> ValueSet.getMax |> Option.map MaxProp
                    [
                        c
                        if min.IsSome then { c with Property = min.Value }
                        if max.IsSome then { c with Property = max.Value }
                    ]
                    |> List.append acc
            | _ -> [c] |> List.append acc
        ) []
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


    let apply onlyMinIncrMax log (c : Constraint) eqs =

        eqs
        |> List.collect (Equation.findName c.Name)
        |> function
        | [] ->
            (c, eqs)
            |> Exceptions.ConstraintVariableNotFound
            |> Exceptions.raiseExc (Some log) []

        | vr::_ ->
            c.Property
            |> Property.toValueRange
            |> Variable.setValueRange onlyMinIncrMax vr
        |> fun var ->
            c
            |> Events.ConstraintApplied
            |> Logging.logInfo log

            var


    let solve onlyMinIncrMax log sortQue (c : Constraint) eqs =
        let var = apply onlyMinIncrMax log c eqs

        eqs
        |> Solver.solveVariable onlyMinIncrMax log sortQue var
        |> fun eqs ->
            c
            |> Events.ConstrainedSolved
            |> Logging.logInfo log

            eqs



/// Public funtions to use the library
module Api =

    open System

    open Informedica.Utils.Lib.BCL

    module VRD = Variable.Dto
    module EQD = Equation.Dto

    module ValueRange = Variable.ValueRange
    module Property = ValueRange.Property
    module Name = Variable.Name


    /// Initialize the solver returning a set of equations
    let init eqs =
        let notEmpty = String.IsNullOrWhiteSpace >> not
        let prodEqs, sumEqs = eqs |> List.partition (String.contains "*")
        let createProdEqs = List.map (EQD.createProd >> EQD.fromDto)
        let createSumEqs  = List.map (EQD.createSum  >> EQD.fromDto)

        let parse eqs op =
            eqs
            |> List.map (String.splitAt '=')
            |> List.map (Array.collect (String.splitAt op))
            |> List.map (Array.map String.trim)
            |> List.map (Array.filter notEmpty)
            |> List.map (Array.map VRD.createNew)

        (parse prodEqs '*' |> createProdEqs) @ (parse sumEqs '+' |> createSumEqs)


    let setVariableValues onlyMinIncrMax n p eqs =
        eqs
        |> List.collect (Equation.findName n)
        |> function
        | [] -> None
        | var::_ ->
            p
            |> Property.toValueRange
            |> Variable.setValueRange onlyMinIncrMax var
            |> Some


    let solveAll = Solver.solveAll


    /// Solve an `Equations` list with
    ///
    /// * f: function used to process string message
    /// * n: the name of the variable to be updated
    /// * p: the property of the variable to be updated
    /// * vs: the values to update the property of the variable
    /// * eqs: the list of equations to solve
    let solve onlyMinIncrMax sortQue log n p eqs =
        eqs
        |> setVariableValues onlyMinIncrMax n p
        |> function
        | None -> eqs |> Ok
        | Some var ->
            eqs
            |> Solver.solveVariable onlyMinIncrMax log sortQue var


    /// Make a list of `EQD`
    /// to contain only positive
    /// values as solutions
    let nonZeroNegative eqs =
        eqs
        |> List.map Equation.nonZeroOrNegative


    let applyConstraints onlyMinIncrMax log eqs cs =
        let apply = Constraint.apply onlyMinIncrMax log

        cs
        |> List.fold (fun acc c ->
            acc
            |> apply c
            |> fun var ->
                acc
                |> List.map (Equation.replace var)
        ) eqs


    let solveConstraints onlyMinIncrMax log cs eqs =
        cs
        |> Constraint.orderConstraints log
        |> applyConstraints false log eqs
        |> Solver.solveAll onlyMinIncrMax log


module SolverLogging =

    open Informedica.Utils.Lib.BCL

    open Types
    open Types.Logging
    open Types.Events

    module Name = Variable.Name
    module ValueRange = Variable.ValueRange


    let private eqsToStr eqs =
        let eqs =
            eqs
            |> List.sortBy (fun e ->
                e
                |> Equation.toVars
                |> List.tryHead
                |> function
                | Some v -> Some v.Name
                | None -> None
            )
        $"""{eqs |> List.map (Equation.toString true) |> String.concat "\n"}"""


    let private varsToStr vars =
        $"""{vars |> List.map (Variable.toString true) |> String.concat ", "}"""


    let rec printException = function
    | Exceptions.ValueRangeEmptyValueSet ->
        "ValueRange cannot have an empty value set"

    | Exceptions.EquationEmptyVariableList ->
        "An equation should at least contain one variable"

    | Exceptions.SolverInvalidEquations eqs ->
        $"The following equations are invalid {eqs |> eqsToStr} "

    | Exceptions.ValueRangeMinLargerThanMax (min, max) ->
        $"{min} is larger than {max}"

    | Exceptions.ValueRangeMinOverFlow min ->
        $"Min overflow: {min}"

    | Exceptions.ValueRangeMaxOverFlow max ->
        $"Max overflow: {max}"

    | Exceptions.ValueRangeNotAValidOperator ->
        "The value range operator was invalid or unknown"

    | Exceptions.EquationDuplicateVariables vars ->
        $"""The list of variables for the equation contains duplicates
{vars |> List.map (Variable.getName >> Name.toString) |> String.concat ", "}
"""

    | Exceptions.NameLongerThan1000 s ->
        $"This name contains more than 1000 chars: {s}"

    | Exceptions.NameNullOrWhiteSpaceException ->
        "A name cannot be a blank string"

    | Exceptions.VariableCannotSetValueRange (var, vlr) ->
        $"This variable:\n{var |> Variable.toString true}\ncannot be set with this range:{vlr |> ValueRange.toString true}\n"

    | Exceptions.SolverTooManyLoops (n, eqs) ->
        $"""Looped (total {n}) more than {Constants.MAX_LOOP_COUNT} times the equation list count ({eqs |> List.length})
{eqs |> eqsToStr}
"""

    | Exceptions.SolverErrored (n, msgs, eqs) ->
        $"=== Solver Errored Solving ({n} loops) ===\n{eqs |> eqsToStr}"
        |> fun s ->
            msgs
            |> List.map (fun msg ->
                match msg with
                | Exceptions.SolverErrored _ -> s
                | _ ->
                        $"Error: {msg |> printException}"
            )
            |> String.concat "\n"
            |> fun es -> $"{s}\n{es}"

    | Exceptions.ValueRangeEmptyIncrement -> "Increment can not be an empty set"

    | Exceptions.ValueRangeTooManyValues c ->
        $"Trying to calculate with {c} values, which is higher than the max calc count {Constants.MAX_CALC_COUNT}"

    | Exceptions.ConstraintVariableNotFound (c, eqs) ->
        $"""=== Constraint Variable not found ===
        {c
        |> sprintf "Constraint %A cannot be set"
        |> (fun s ->
            eqs
            |> List.map (Equation.toString true)
            |> String.concat "\n"
            |> sprintf "%s\In equations:\%s" s
        )
        }
        """
    | _ -> "not a recognized msg"

    let printMsg = function
    | ExceptionMessage m ->
        m
        |> printException
    | SolverMessage m ->
        let toString eq =
            let op = if eq |> Equation.isProduct then " * " else " + "
            let varName = Variable.getName >> Variable.Name.toString

            match eq |> Equation.toVars with
            | [] -> ""
            | [ _ ] -> ""
            | y::xs ->
                $"""{y |> varName } = {xs |> List.map varName |> String.concat op}"""


        match m with
        | EquationStartedSolving eq ->
            $"=== Start solving Equation ===\n{eq |> toString}"

        | EquationStartCalculation (op1, op2, y, x, xs) ->
            $"start calculating: {Equation.calculationToString op1 op2 y x xs}"

        | EquationFinishedCalculation (xs, changed) ->
            $"""finished calculation: {if (not changed) then "No changes" else xs |> varsToStr}"""

        | EquationFinishedSolving (eq, b) ->
            $"""=== Equation Finished Solving ===
{eq |> Equation.toString true}
{b |> Equation.SolveResult.toString}
"""

        | EquationCouldNotBeSolved eq ->
            $"=== Cannot solve Equation ===\n{eq |> Equation.toString true}"

        | SolverStartSolving eqs ->
            $"=== Solver Start Solving ===\n{eqs |> eqsToStr}"

        | SolverLoopedQue (n, eqs) ->
            $"solver looped que {n} times with {eqs |> List.length} equations"

        | SolverFinishedSolving eqs ->
            $"=== Solver Finished Solving ===\n{eqs |> eqsToStr}"

        | ConstraintSortOrder cs ->
            let s =
                cs
                |> List.map (fun (i, c) ->
                    c
                    |> Constraint.toString
                    |> sprintf "%i: %s" i
                )
                |> String.concat "\n"
            $"=== Constraint sort order ===\n{s}"

        | ConstraintApplied c -> $"Constraint {c |> Constraint.toString} applied"

        | ConstrainedSolved c -> $"Constraint {c |> Constraint.toString} solved"


    let logger f =
        {
            Log =
                fun { TimeStamp = _; Level = _; Message = msg } ->
                    match msg with
                    | :? Logging.SolverMessage as m ->
                        m |> printMsg |> f
                    | _ -> $"cannot print msg: {msg}" |> f

        }
