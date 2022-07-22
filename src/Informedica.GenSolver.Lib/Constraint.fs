namespace Informedica.GenSolver.Lib


module Property =

    open Types

    module ValueRange = Variable.ValueRange
    module Minimum = ValueRange.Minimum
    module Maximum = ValueRange.Maximum
    module Increment = ValueRange.Increment
    module ValueSet = ValueRange.ValueSet
                

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


    let matchProp p =

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



module Constraint =

    open Types

    module ValueRange = Variable.ValueRange
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
            |> Property.matchProp
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
