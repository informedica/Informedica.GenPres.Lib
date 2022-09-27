namespace Informedica.GenOrder.Lib

/// Helper functions to
/// facilitate the use of the
/// `Informedica.GenSolver.Lib`
module Solver =

    open Informedica.Utils.Lib
    open Informedica.GenUnits.Lib
    open Informedica.GenSolver.Lib

    open Types

    module Name = Variable.Name
    module Api = Api
    module ValueRange = Variable.ValueRange
    module Property = ValueRange.Property
    module Logging = Informedica.GenOrder.Lib.Logging


    let productEq = function
    | h::tail -> (h, tail) |> OrderProductEquation
    | _ -> "not a valid product equation" |> failwith

    let sumEq = function
    | h::tail -> (h, tail) |> OrderSumEquation
    | _ -> "not a valid sum equation" |> failwith

    /// Create an `Equation` using a constructor **cr**
    /// a result `VariableUnit` **y** and a list of
    /// `VariableUnit` list **xs**
    let toEq cr y xs =
        (y |> VariableUnit.getVar, xs |> List.map VariableUnit.getVar)
        |> cr

    /// Create a `ProdEquation` from `VariableUnit`s
    let toProdEq succ fail y xs =
        toEq (Equation.createProductEq succ fail) y xs

    /// Create a `SumEquation` from `VariableUnit`s
    let toSumEq succ fail y xs =
        toEq (Equation.createSumEq succ fail) y xs

    let mapToSolverEqs =
        List.fold (fun acc eq ->
            match eq with
            | OrderProductEquation (y, xs) -> toProdEq id (string >> exn >> raise) y xs
            | OrderSumEquation     (y, xs) -> toSumEq id  (string >> exn >> raise) y xs
            |> List.singleton
            |> List.append acc
        ) []


    let replaceUnit log n u eqs =
        (n, u)
        |> Events.SolverReplaceUnit
        |> Logging.logInfo log

        let repl c vru vrus =
            if vru |> VariableUnit.getName = n then
                (vru |> VariableUnit.setUnit u, vrus)
            else
                vru,
                vrus
                |> List.map (fun vru ->
                    if vru |> VariableUnit.getName = n then
                        vru
                        |> VariableUnit.setUnit u
                    else vru
                )
            |> c

        eqs
        |> List.map (fun e ->
            match e with
            | OrderSumEquation (vru, vrus) ->
                repl OrderSumEquation vru vrus
            | OrderProductEquation (vru, vrus) ->
                repl OrderProductEquation vru vrus
        )


    /// calculate the units for all vrus in
    /// all eqs
    let solveUnits log eqs =
        let hasUnit = VariableUnit.hasUnit
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
                                        |> List.map VariableUnit.getUnit
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
                                    [ x |> VariableUnit.setUnit y.Unit ],
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
                                                        |> List.map VariableUnit.getUnit
                                                        |> List.reduce (ValueUnit.calcUnit (*))
                                                        |> ValueUnit.calcUnit (/)
                                                            (y |> VariableUnit.getUnit)
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
                        // get the names of vrus with no unit
                        let ns =
                            y::xs
                            |> List.filter noUnit
                            |> List.map VariableUnit.getName
                        // find the vru with a unit
                        let x =
                            y::xs
                            |> List.find hasUnit
                        // start from scratch
                        ({ y with Unit = x.Unit },
                         xs |> List.map (VariableUnit.setUnit x.Unit))
                        |> OrderSumEquation
                        |> List.singleton
                        |> List.append tail
                        |> List.append acc
                        // make sure that all vrus in all eqs get the unit
                        |> (fun eqs ->
                            ns
                            |> List.fold (fun acc n ->
                                acc |> replaceUnit log n x.Unit
                            ) eqs
                        )
                        |> solve []

        solve [] eqs


    let toVariableUnits =
        List.map (fun eq ->
            match eq with
            | OrderProductEquation (y, xs)
            | OrderSumEquation     (y, xs) -> y::xs
        )


    /// Turn a set of values `vs` to base values
    let toBase n eqs v =

        eqs
        |> toVariableUnits
        |> List.tryFindInList (VariableUnit.getName >> ((=) n))
        |> function
        | Some vru ->
            vru
            |> VariableUnit.getUnit
            |> fun u ->
                v
                |> ValueUnit.create u
                |> ValueUnit.toBase

        | None ->
            $"could not find %A{n} in toBase n eqs vs"
            |> failwith


    let mapFromSolverEqs orig eqs =
        let vrusl = orig |> toVariableUnits
        let vars =
            eqs
            |> List.collect Equation.toVars
            |> List.distinct

        vrusl
        |> List.map (fun vrus ->
            vrus
            |> List.map (fun vru ->
                { vru with
                    Variable =
                        vars
                        |> List.tryFind (fun v -> v.Name = vru.Variable.Name)
                        |> function
                        | Some v -> v
                        | None ->
                            $"could not find %A{vru.Variable.Name}"
                            |> failwith
                }
            )
        )



    let setVals n p eqs =
        eqs
        |> Api.setVariableValues n p


    let filterEqsWithUnits =
        List.filter (fun eq ->
            match eq with
            | OrderProductEquation (y, xs)
            | OrderSumEquation     (y, xs) ->
                y::xs |> List.forall VariableUnit.hasUnit
        )


    let propToBase n eqs p = p |> Property.mapValue (toBase n eqs)


    // Solve a set of equations setting a property `p` with
    // name `n`, to a valueset `vs`.
    let applySolve sortQue log n p eqs =

        let toBase = propToBase n eqs

        eqs
        // use only eqs with all vrus have units
        |> filterEqsWithUnits
        |> mapToSolverEqs
        |> Api.solve true sortQue log n (p |> toBase)
        |> mapFromSolverEqs eqs


    let solve log =
        let sortQue = Solver.sortQue
        applySolve sortQue log //(printfn "%s") //|> memSolve


    let applyConstraints log (cs : Constraint list) eqs =
        let cs =
            cs
            |> List.map (fun c ->
                { c with
                    Property =
                        c.Property
                        |> propToBase c.Name eqs
                }
            )

        eqs
        // use only eqs with all vrus have units
        |> filterEqsWithUnits
        |> mapToSolverEqs
        |> fun eqs -> Api.applyConstraints true log eqs cs
        |> mapFromSolverEqs eqs


    let solveConstraints log (cs : Constraint list) eqs =
        let cs =
            cs
            |> List.map (fun c ->
                { c with
                    Property =
                        c.Property
                        |> propToBase c.Name eqs
                }
            )

        eqs
        // use only eqs with all vrus have units
        |> filterEqsWithUnits
        |> mapToSolverEqs
        |> Api.solveConstraints true log cs
        |> mapFromSolverEqs eqs
