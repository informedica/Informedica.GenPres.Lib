namespace Informedica.GenSolver.Lib


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
            |> Variable.getName)


    /// Format a set of equations to print.
    /// Using **f** to allow additional processing
    /// of the string.
    let printEqs exact pf eqs = 

        "equations result:\n" |> pf
        eqs
        |> sortByName
        |> List.map (Equation.toString exact)
        |> List.iteri (fun i s -> sprintf "%i.\t%s" i s  |> pf)
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
            match (cache.Value).TryFind(e) with
            | Some r -> r
            | None ->
                let r = f e
                cache.Value <- (cache.Value).Add(e, r)
                r

    let sortQue que =
        if que |> List.length = 0 then que
        else
            que 
            |> List.sortBy Equation.countProduct

        
    /// Create the equation solver using a 
    /// product equation and a sum equation solver
    /// and function to determine whether an 
    /// equation is solved
    let solveOpt log sortQue vr eqs =

        let solveE = Equation.solve log
            
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
                | []      -> acc
                | invalid -> 
                    let msg =
                        invalid
                        |> Exceptions.SolverInvalidEquations
                    
                    msg |> Logging.logError log
                    msg |> Exception.raiseExc
                
            | eq::tail ->
                // If the equation is already solved, or not solvable 
                // just put it to  the accumulated equations and go on with the rest
                if eq |> Equation.isSolvable |> not then
                    [ eq ] 
                    |> List.append acc
                    |> loop (n + 1) tail
                // Else go solve the equation
                else
                    match eq |> solveE with
                    // Equation is changed, so every other equation can 
                    // be changed as well (if changed vars are in the other
                    // equations) so start new
                    | eq, Changed cs ->
                        let vars = cs |> List.map fst
                        // don't need to do this anymore
                        //let eq = [ eq ] |> replace vars |> fst

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

                            rst
                            |> List.append [ eq ]
                            |> loop (n + 1) que 

                    // Equation did not in fact change, so put it to
                    // the accumulated equations and go on with the rest
                    | eq, Unchanged ->
                        [eq] 
                        |> List.append acc
                        |> loop (n + 1) tail

        match vr with
        | None -> loop 0 eqs []
        | Some var ->
            eqs 
            |> replace [var]
            |> function 
            | (rpl, rst) -> loop 0 rpl rst
            
    
    let solve log sortQue vr eqs =
        solveOpt log sortQue (Some vr) eqs


    let solveAll log eqs =
        solveOpt log sortQue None eqs
