namespace Informedica.GenOrder.Lib


/// Helper functions to
/// facilitate the use of the
/// `Informedica.GenSolver.Lib`
module Solver =

    open Informedica.Utils.Lib
    open Informedica.GenUnits.Lib
    open Informedica.GenCore.Lib.Types.GenOrder
    open Informedica.GenSolver.Lib.Types

    module Variable = Informedica.GenSolver.Lib.Variable
    module Name = Variable.Name
    module ValueRange = Variable.ValueRange
    module Property = ValueRange.Property
    module Equation = Informedica.GenSolver.Lib.Equation
    module Solver = Informedica.GenSolver.Lib.Solver
    module Api = Informedica.GenSolver.Lib.Api



    let filterEqsWithUnits =
        List.filter (fun eq ->
            match eq with
            | OrderProductEquation (y, xs)
            | OrderSumEquation     (y, xs) ->
                y::xs |> List.forall OrderVariable.hasUnit
        )


    let scaleOrderEqs scalar eqs =
        let eqs = eqs |> filterEqsWithUnits
        let toBase y xs =
            (y |> scalar, xs |> List.map scalar)

        eqs
        |> List.map (fun eq ->
            match eq with
            | OrderProductEquation (y, xs) -> toBase y xs |> OrderProductEquation
            | OrderSumEquation     (y, xs) -> toBase y xs |> OrderSumEquation
        )


    let orderEqsToBase = scaleOrderEqs OrderVariable.toBase


    let orderEqsToUnit = scaleOrderEqs OrderVariable.toUnit


    let mapToSolverEqs eqs =
        eqs
        |> filterEqsWithUnits
        |> List.map (fun eq ->
            match eq with
            | OrderProductEquation (y, xs) -> (y.Variable, xs |> List.map (fun v -> v.Variable)) |> ProductEquation
            | OrderSumEquation     (y, xs) -> (y.Variable, xs |> List.map (fun v -> v.Variable)) |> SumEquation
        )


    let mapToOrderEqs ordEqs eqs =
        let vars =
            eqs
            |> List.collect Equation.toVars
        let repl v =
            { v with
                Variable =
                    vars
                    |> List.find (Variable.getName >> ((=) v.Variable.Name))
            }
        ordEqs
        |> List.map (fun eq ->
            match eq with
            | OrderProductEquation (y, xs) ->
                (y |> repl, xs |> List.map repl)
                |> OrderProductEquation
            | OrderSumEquation (y, xs) ->
                (y |> repl, xs |> List.map repl)
                |> OrderSumEquation
        )


    let replaceUnit log n u eqs =
        (n, u)
        |> Events.SolverReplaceUnit
        |> Logging.logInfo log

        let repl c ovar ovars =
            if ovar |> OrderVariable.getName = n then
                (ovar |> OrderVariable.setUnit u, ovars)
            else
                ovar,
                ovars
                |> List.map (fun vru ->
                    if vru |> OrderVariable.getName = n then
                        vru
                        |> OrderVariable.setUnit u
                    else vru
                )
            |> c

        eqs
        |> List.map (fun e ->
            match e with
            | OrderSumEquation (ovar, ovars) -> repl OrderSumEquation ovar ovars
            | OrderProductEquation (ovar, ovars) -> repl OrderProductEquation ovar ovars
        )


    /// calculate the units for all variable units in
    /// all eqs
    let solveUnits log eqs =
        let hasUnit = OrderVariable.hasUnit
        let noUnit = hasUnit >> not

        let rec solve acc eqs =
            match eqs with
            | []      -> acc
            | h::tail ->
                match h with
                | OrderProductEquation (y, xs) ->
                    if y::xs  |> List.hasExactlyOne noUnit then
                        if y |> noUnit then
                            let y =
                                { y with
                                    Unit =
                                        xs
                                        |> List.map OrderVariable.getUnit
                                        |> List.reduce (ValueUnit.calcUnit (*))
                                }
                            let h = (y, xs) |> OrderProductEquation
                            // start from scratch
                            h::tail
                            |> List.append acc
                            |> replaceUnit log y.Variable.Name y.Unit
                            |> solve []

                        else
                            let xs, n, u =
                                // actually y = x
                                if xs |> List.length = 1 then
                                    let x = xs.Head
                                    [ x |> OrderVariable.setUnit y.Unit ],
                                    Some x.Variable.Name, Some y.Unit
                                // y = x1 * x2 ... so
                                // the x without a unit = y / multiple of all xs with units, i.e. (x1 * x2 .. )
                                else
                                    xs
                                    |> List.fold (fun acc x ->
                                        let xs', n, u = acc
                                        if x |> noUnit then // found the x without a unit
                                            let x =
                                                { x with
                                                    Unit =
                                                        xs
                                                        |> List.filter hasUnit
                                                        |> List.map OrderVariable.getUnit
                                                        |> List.reduce (ValueUnit.calcUnit (*))
                                                        |> ValueUnit.calcUnit (/)
                                                            (y |> OrderVariable.getUnit)
                                                }
                                            (x::xs', (Some x.Variable.Name), (Some x.Unit))
                                        else
                                            (x::xs', n, u)
                                    ) ([], None, None)

                            let h = (y, xs) |> OrderProductEquation
                            // start from scratch
                            h::tail
                            |> List.append acc
                            |> replaceUnit log (n |> Option.get) (u |> Option.get)
                            |> solve []

                    else
                        solve (h::acc) tail

                | OrderSumEquation (y, xs) ->
                    if y::xs |> List.forall hasUnit ||
                       y::xs |> List.forall noUnit then
                        solve (h::acc) tail

                    else
                        // get the names of order variables with no unit
                        let ns =
                            y::xs
                            |> List.filter noUnit
                            |> List.map OrderVariable.getName
                        // find the vru with a unit
                        let x =
                            y::xs
                            |> List.find hasUnit
                        // start from scratch
                        ({ y with Unit = x.Unit },
                         xs |> List.map (OrderVariable.setUnit x.Unit))
                        |> OrderSumEquation
                        |> List.singleton
                        |> List.append tail
                        |> List.append acc
                        // make sure that all order variables in all eqs get the unit
                        |> (fun eqs ->
                            ns
                            |> List.fold (fun acc n ->
                                acc |> replaceUnit log n x.Unit
                            ) eqs
                        )
                        |> solve []

        solve [] eqs


    let solveMinMax = Api.solveAll true


    let solve = Api.solveAll false


