namespace Informedica.GenSolver.Lib



module Constraint =

    open Types

    module ValueRange = Variable.ValueRange
    module Property = ValueRange.Property
    module ValueSet = ValueRange.ValueSet
    module Name = Variable.Name

    let eqsName (c1 : Constraint) (c2 : Constraint) = c1.Name = c2.Name


    let toString { Name = n; Property = p; Limit = l} = $"{n |> Name.toString}: {p} {l}"


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
                        if min.IsSome then { c with Property = min.Value ; Limit = NoLimit }
                        if max.IsSome then { c with Property = max.Value ; Limit = NoLimit }
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

        let lim l b vr =
            if vr |> Variable.count <= l then vr
            else
                vr
                |> Variable.getValueRange
                |> ValueRange.getValSet
                |> function
                | Some (ValueSet vs) ->
                    vs
                    |> Set.toList
                    |> fun xs ->
                        if b then xs |> List.sort
                        else xs |> List.sortDescending
                    |> List.take l
                    |> Set.ofList
                    |> ValueRange.createValSet
                    |> Variable.setValueRange onlyMinIncrMax vr
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
            |> Property.toValueRange
            |> Variable.setValueRange onlyMinIncrMax vr
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
        | None -> eqs, None
        | Some var ->
            (c, var)
            |> Events.ConstraintVariableApplied
            |> Logging.logInfo log

            eqs, Some var


    let solve onlyMinIncrMax log sortQue (c : Constraint) eqs =
        match apply onlyMinIncrMax log c eqs with
        | eqs, None -> eqs
        | eqs, Some var ->
            eqs
            |> Solver.solveVariable onlyMinIncrMax log sortQue var
            |> fun eqs ->
                (c, eqs)
                |> Events.ConstrainedEquationsSolved
                |> Logging.logInfo log

                eqs
