namespace Informedica.GenSolver.Lib


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
            |> Property.matchProp
            |> Variable.setValueRange vr
            |> fun vr ->
                match lim with
                | Some l ->
                    if vr |> Variable.count > l then
                        vr
                        |> Variable.getValueRange
                        |> ValueRange.getValSet
                        |> function
                        | Some (ValueSet vs) -> 
                            vs 
                            |> Seq.sort 
                            |> Seq.take l
                            |> Set.ofSeq
                            |> ValueRange.createValSet
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