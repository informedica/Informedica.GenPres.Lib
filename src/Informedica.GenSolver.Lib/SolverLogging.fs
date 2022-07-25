namespace Informedica.GenSolver.Lib


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
        $"""The following equations are invalid
        {eqs |> List.map (Equation.toString true) |> String.concat "\n"}
        """
    | Exceptions.ValueRangeMinLargerThanMax (min, max) ->
        $"{min} is larger than {max}"
    | Exceptions.ValueRangeNotAValidOperator ->
        "The value range operator was invalid or unknown"
    | Exceptions.EquationDuplicateVariables vrs ->
        $"""The list of variables for the equation contains duplicates
        {vrs |> List.map (Variable.getName >> Name.toString) |> String.concat ", "}
        """
    | Exceptions.NameLongerThan1000 s ->
        $"This name contains more than 1000 chars: {s}"
    | Exceptions.NameNullOrWhiteSpaceException ->
        "A name cannot be a blank string"
    | Exceptions.VariableCannotSetValueRange (var, vlr) ->
        $"This variable:\n{var |> Variable.toString true}\ncannot be set with this range:{vlr |> ValueRange.toString true}\n"
    | Exceptions.SolverTooManyLoops eqs ->
        $"""The following equations are looped more than {Constants.MAX_LOOP_COUNT} times the equation list count
        {eqs |> List.map (Equation.toString true) |> String.concat "\n"}
        """
    | Exceptions.ValueRangeEmptyIncrement -> "Increment can not be an empty set"
    | Exceptions.ValueRangeTooManyValues c ->
        $"Trying to calculate with {c} values, which is higher than the max calc count {Constants.MAX_CALC_COUNT}"


    let printMsg = function
    | ExceptionMessage m ->
        m 
        |> printException
    | SolverMessage m ->
        match m with
        | EquationCouldNotBeSolved eq -> 
            $"=== Cannot solve Equation ===\n{eq |> Equation.toString true}" 
        | EquationStartedCalculation vars -> ""
        | EquationCalculation (op1, op2, y, x, xs) ->
            let s = Equation.calculationToString op1 op2 y x xs
            $"calculating: {s}"
        | EquationStartedSolving eq -> 
            $"=== Start solving Equation ===\n{eq |> Equation.toString true}"
        | EquationFinishedCalculation (xs, changed) -> 
            $"""=== Equation finished calculation ===
            {
                if (not changed) then "No changes"
                else
                    xs |> List.map (Variable.toString true) |> String.concat ", "
            }
            """
        | EquationVariableChanged var -> 
            $"=== Equation Variable changed ===\n{var |> Variable.toString true}"
        | EquationFinishedSolving vars -> ""
        | EquationLoopedSolving (b, var, changed, vars) -> 
            "=== Equation loop solving"
        | SolverLoopedQue eqs -> ""
        | ConstraintSortOrder cs -> 
            $"""=== Constraint sort order ===
            { cs |> List.map (fun (i, c) ->
                c
                |> Constraint.toString
                |> sprintf "%i: %s" i
            )
            |> String.concat "\n"
            }
            """
        | ConstraintVariableNotFound (c, eqs) -> 
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
        | ConstraintLimitSetToVariable (l, var) -> ""
        | ConstraintVariableApplied (c, var) -> 
            $"""=== Constraint apply Variable ===
            {c
            |> Constraint.toString
            |> fun s -> 
                var
                |> Variable.getName
                |> Name.toString
                |> sprintf "%s apply to %s" s
            }
            """
        | ConstrainedEquationsSolved (c, eqs) -> 
            $"""=== Equations solved ===
            {c 
            |> Constraint.toString
            |> fun s ->
                eqs
                |> List.sort
                |> List.map (Equation.toString true)
                |> String.concat "\n"
                |> sprintf "Constraint: %s applied to\n%s" s

            }
            """
        | ApiSetVariable (var, eqs) -> ""
        | ApiEquationsSolved eqs -> ""
        | ApiAppliedConstraints (cs, eqs) -> ""


    let logger f =
        {
            Log = 
                fun { TimeStamp = _; Level = _; Message = msg } ->
                    match msg with
                    | :? Logging.SolverMessage as m ->
                        m |> printMsg |> f
                    | _ -> $"cannot print msg: {msg}" |> f 
                        
        }
