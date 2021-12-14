namespace Informedica.GenSolver.Lib

open System
open MathNet.Numerics

module Types =

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


    /// The increment in a `Range`. This is the set of multiples by which each
    /// value in a `ValueRange` must be divisible. 
    /// So for each value in valuerange there is an incr -> value % incr = 0
    ///
    /// An increment cannnot be zero or negative.   
    type Increment = Increment of Set<BigRational>


    /// `ValueRange` represents a discrete set of
    /// rational numbers.
    /// A `ValueRange` is either unrestricted,
    /// a finite set of `BigRational` or a `Range`.
    ///
    /// Notation:
    /// * Unrestricted: <..>
    /// * ValueSet: [1N/2N, 2N, 3N/5N, 5N]
    /// * Range: <0N..[2N,3N]..20N] 
    type ValueRange =
        | Unrestricted
// ToDo need to model this
//        | NotZero
        | ValueSet of Set<BigRational>
        | Range of Range


    /// A `Range` is restricted by either a
    /// `Minimum`, a `Maximum`, a `Minimum`
    /// and a increment, an increment and
    /// a `Maximum` or a `Minimum` and a
    /// `Maximum`. 
    and Range =
        | Min of Minimum
        | Max of Maximum
        | MinIncr of Minimum * Increment
        | IncrMax of Increment * Maximum
        | MinMax  of Minimum * Maximum
        | MinIncrMax of Minimum * Increment * Maximum


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
    /// * `Increment`: A set of distinct increments
    /// * `MinIncl`: An inclusive minimum
    /// * `MinExcl`: An exclusive minimum
    /// * `MaxIncl`: An inclusive maximum
    /// * `MaxExcl`: An exclusive maximum
    type Property =
        | ValsProp of BigRational Set
//        | IncrProp of BigRational Set
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
        | IncrementZeroNegativeOrEmpty of BigRational Set
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

