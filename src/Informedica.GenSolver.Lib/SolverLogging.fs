namespace Informedica.GenSolver.Lib

module SolverLogging =
    
    open Informedica.Utils.Lib.BCL

    open Types
    open Types.Logging
    open Types.Events

    module Name = Variable.Name
    module ValueRange = Variable.ValueRange

    let printException = function
    | Exceptions.IncrementZeroNegativeOrEmpty vs ->
        if vs |> Set.isEmpty then "Increment has no values"
        else
            vs
            |> Set.map (BigRational.toString)
            |> String.concat ", "
            |> sprintf "Increment contains the zero or negative values: %s"
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
