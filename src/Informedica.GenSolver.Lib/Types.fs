namespace Informedica.GenSolver.Lib


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


    type ValueSet = ValueSet of BigRational Set


    type Increment = Increment of BigRational Set

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
        | Min of Minimum
        | Max of Maximum
        | MinMax of Minimum * Maximum
        | Incr of Increment
        | MinIncr of min: Minimum * incr: Increment
        | IncrMax of incr: Increment * max: Maximum
        | ValSet of ValueSet // Set<BigRational>

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
        | MinInclProp of BigRational
        | MinExclProp of BigRational
        | MaxInclProp of BigRational
        | MaxExclProp of BigRational
        | IncrProp of BigRational Set
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
            | ValueRangeEmptyIncrement
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

