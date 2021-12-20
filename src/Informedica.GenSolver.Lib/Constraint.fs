namespace Informedica.GenSolver.Lib



module Props =

    open Types

    module ValueRange = Variable.ValueRange

    let matchProp p =

        match p with
        | ValsProp vs -> 
            vs
            |> ValueRange.ValueSet
        | MinInclProp v -> v |> ValueRange.createMinRange true 
        | MinExclProp v -> v |> ValueRange.createMinRange false 
        | MaxInclProp v -> v |> ValueRange.createMaxRange true 
        | MaxExclProp v -> v |> ValueRange.createMaxRange false 
        | DeltaProp _ -> "not implemented" |> failwith


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


