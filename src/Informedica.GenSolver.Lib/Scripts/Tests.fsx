
#r "nuget: Expecto"
#r "nuget: Expecto.FSCheck"


#load "../Scripts/load.fsx"

#time

open Informedica.GenSolver.Lib

open System
open System.IO

module Name = Variable.Name
module ValueRange = Variable.ValueRange
module Minimum = ValueRange.Minimum
module Maximum = ValueRange.Maximum
module Increment = ValueRange.Increment
module ValueSet = ValueRange.ValueSet

open Informedica.Utils.Lib


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


module Solve =

    let procss s =
        File.AppendAllLines("order.log", [s])
        $"{s} " |> printfn "%s"

    let setProp n p eqs =
        let n = n |> Name.createExc
        match eqs |> Api.setVariableValues true n p with
        | Some var ->
            eqs
            |> List.map (fun e ->
                e |> Equation.replace var
            )
        | None -> eqs

    let setMinIncl n min = min |> Minimum.create true |> MinProp |> setProp n
    let setMinExcl n min = min |> Minimum.create false |> MinProp |> setProp n
    let setMaxIncl n max = max |> Maximum.create true |> MaxProp |> setProp n
    let setMaxExcl n max = max |> Maximum.create true |> MaxProp |> setProp n
    let setValues n vals = vals |> ValueSet.create |> ValsProp |> setProp n


    let printEqs = Solver.printEqs true procss
    let solve n p eqs =
        let logger =
            fun s ->
                File.AppendAllLines("order.log", [s])
            |> SolverLogging.logger
        try
            eqs
            |> Api.solve true Solver.sortQue logger (n |> Name.createExc) p
            |> Result.get
        with
        | :? Exceptions.SolverException as e ->
            printfn $"{e.Data0}"
            raise e

    let solveMinIncl n min = solve n (min |> Minimum.create true |> MinProp)
    let solveMinExcl n min = solve n (min |> Minimum.create false |> MinProp)
    let solveMaxIncl n max = solve n (max |> Maximum.create true |> MaxProp)
    let solveMaxExcl n max = solve n (max |> Maximum.create false |> MaxProp)
    let solveIncr n incr = solve n (set [incr] |> Increment.create |> IncrProp)
    let solveValues n vals = solve n (vals |> ValueSet.create |> ValsProp)

    let init     = Api.init
    let nonZeroNegative = Api.nonZeroNegative


    let findValidValues n (eqs : Equation list) =
        let var =
            eqs
            |> List.collect Equation.toVars
            |> List.tryFind (fun v ->
                v
                |> Variable.getName
                |> Name.toString
                |> fun x -> x = n
            )
            |> Option.get

        match var.Values |> ValueRange.getValSet with
        | None    -> ()
        | Some (ValueSet vs) ->
            for v in vs do
                try
                    eqs
                    |> solveValues n [v]
                    |> ignore
                    printfn $"can set {v}"
                with
                | _ ->
                    printfn $"cannot set {v}"



module Equations =


    type Orb =
        {
            Components : Component list
        }
    and Component =
        {
            Name : string
            Items : string list
        }


    let create (orb : Orb) =
        [
            "itm_cmp_qty = itm_cmp_cnc * cmp_qty"
            "itm_orb_qty = itm_orb_cnc * orb_qty"
            "itm_orb_qty = itm_cmp_cnc * cmp_orb_qty"
            "itm_dos_qty = itm_cmp_cnc * cmp_dos_qty"
            "itm_dos_qty = itm_orb_cnc * orb_dos_qty"
            "itm_dos_qty = itm_dos_rte * pres_time"
            "itm_dos_qty = itm_dos_qty_adj * ord_adj"
            "itm_dos_tot = itm_cmp_cnc * cmp_dos_tot"
            "itm_dos_tot = itm_orb_cnc * orb_dos_tot"
            "itm_dos_tot = itm_dos_qty * pres_freq"
            "itm_dos_tot = itm_dos_tot_adj * ord_adj"
            "itm_dos_rte = itm_cmp_cnc * cmp_dos_rte"
            "itm_dos_rte = itm_orb_cnc * orb_dos_rte"
            "itm_dos_rte = itm_dos_rte_adj * ord_adj"
            "itm_dos_ord = itm_dos_tot * ord_time"
            "itm_dos_ord = itm_dos_rte * ord_time"
            "itm_dos_qty_adj = itm_cmp_cnc * cmp_dos_qty_adj"
            "itm_dos_qty_adj = itm_orb_cnc * orb_dos_qty_adj"
            "itm_dos_qty_adj = itm_dos_rte_adj * pres_time"
            "itm_dos_tot_adj = itm_cmp_cnc * cmp_dos_tot_adj"
            "itm_dos_tot_adj = itm_orb_cnc * orb_dos_tot_adj"
            "itm_dos_tot_adj = itm_dos_qty_adj * pres_freq"
            "itm_dos_rte_adj = itm_cmp_cnc * cmp_dos_rte_adj"
            "itm_dos_rte_adj = itm_orb_cnc * orb_dos_rte_adj"
            "cmp_orb_qty = cmp_orb_cnc * orb_qty"
            "cmp_orb_qty = cmp_qty * cmp_orb_cnt"
            "cmp_ord_qty = cmp_qty * cmp_ord_cnt"
            "cmp_ord_qty = cmp_dos_tot * ord_time"
            "cmp_ord_qty = cmp_dos_rte * ord_time"
            "cmp_dos_qty = cmp_orb_cnc * orb_dos_qty"
            "cmp_dos_qty = cmp_dos_rte * pres_time"
            "cmp_dos_qty = cmp_dos_qty_adj * ord_adj"
            "cmp_dos_tot = cmp_orb_cnc * orb_dos_tot"
            "cmp_dos_tot = cmp_dos_qty * pres_freq"
            "cmp_dos_tot = cmp_dos_tot_adj * ord_adj"
            "cmp_dos_rte = cmp_orb_cnc * orb_dos_rte"
            "cmp_dos_rte = cmp_dos_rte_adj * ord_adj"
            "cmp_dos_qty_adj = cmp_orb_cnc * orb_dos_qty_adj"
            "cmp_dos_qty_adj = cmp_dos_rte_adj * pres_time"
            "cmp_dos_tot_adj = cmp_orb_cnc * orb_dos_tot_adj"
            "cmp_dos_tot_adj = cmp_dos_qty_adj * pres_freq"
            "cmp_dos_rte_adj = cmp_orb_cnc * orb_dos_rte_adj"
            "orb_ord_qty = orb_ord_cnt * orb_qty"
            "orb_ord_qty = orb_dos_tot * ord_time"
            "orb_ord_qty = orb_dos_rte * ord_time"
            "orb_dos_qty = orb_dos_rte * pres_time"
            "orb_dos_qty = orb_dos_qty_adj * ord_adj"
            "orb_dos_tot = orb_dos_qty * pres_freq"
            "orb_dos_tot = orb_dos_tot_adj * ord_adj"
            "orb_dos_rte = orb_dos_rte_adj * ord_adj"
            "orb_dos_qty_adj = orb_dos_rte_adj * pres_time"
            "orb_dos_tot_adj = orb_dos_qty_adj * pres_freq"
            "orb_qty = orb_qty_adj * ord_adj"
            // "orb_qty = sum(cmp_orb_qty)"
            // only add these when single values are used!!
            // "orb_dos_qty = sum(cmp_dos_qty)"
            // "orb_dos_tot = sum(cmp_dos_tot)"
            // "orb_dos_rte = sum(cmp_dos_rte)"
            // "orb_dos_qty_adj = sum(cmp_dos_qty_adj)"
            // "orb_dos_tot_adj = sum(cmp_dos_tot_adj)"
            // "orb_dos_rte_adj = sum(cmp_dos_rte_adj)"
        ]
        |> fun eqs ->
            let eqs = eqs |> List.map (fun s -> $" {s}")
            let sumEqs =
                eqs
                |> List.filter (fun e ->
                    e.Contains("sum")
                )
            let eqs = eqs |> List.filter (fun e -> e.Contains("sum") |> not)
            let itmEqs =
                eqs
                |> List.filter (fun e ->
                    e.Contains("itm")
                )
            let cmpEqs =
                eqs
                |> List.filter (fun e ->
                    itmEqs
                    |> List.exists ((=) e)
                    |> not &&
                    e.Contains("cmp")
                )
            let orbEqs =
                eqs
                |> List.filter (fun e ->
                    itmEqs
                    |> List.exists ((=) e)
                    |> not &&
                    cmpEqs
                    |> List.exists((=) e)
                    |> not
                )

            orb.Components
            |> List.fold (fun acc c ->
                let itms =
                    c.Items
                    |> List.collect (fun i ->
                        itmEqs
                        |> List.map (fun s -> s.Replace(" cmp", $" {c.Name}").Replace(" itm", $" {i}"))
                    )
                let cmps =
                    cmpEqs
                    |> List.map (fun s1 ->
                        s1.Replace(" cmp", $" {c.Name}")
                    )
                itms @ cmps @ acc
            ) []
            |> fun es ->
                let sumEqs =
                    sumEqs
                    |> List.map (fun e ->
                        match e.Replace("sum(", "").Replace(")", "").Split(" = ") with
                        | [|lv; rv|] ->
                            orb.Components
                            |> List.map(fun c -> rv.Replace("cmp", c.Name))
                            |> String.concat(" + ")
                            |> fun s -> $"{lv} = {s}"
                        | _ -> ""
                    )
                es @ orbEqs @ sumEqs
        |> Api.init



let eqs =
    [
        "a = b + c"
        "d = e * a"
        "d = f * b"
    ]
    |> Api.init

//[ "a = b" ] |> Api.init |> Solver.printEqs true (printfn "%s")

open Solve
open Expecto
open Expecto.Logging
open Expecto.Flip
open MathNet.Numerics


let run = runTestsWithCLIArgs [ CLIArguments.Verbosity LogLevel.Verbose ] [||]



module Generators =

    open FsCheck

    let createBigR (n, d) =
        let d = if d = 0I then 1I else d
        let n = if n = 0I then 1I else n
        (n |> BigRational.FromBigInt) / (d |> BigRational.FromBigInt)


    let bigRGen =
        gen {
            let! n = Arb.generate<bigint>
            let! d = Arb.generate<bigint>
            return createBigR(n, d)
        }


    type BigR = BigR of BigRational
    let bigRArb () =
        bigRGen
        |> Arb.fromGen
        |> Arb.convert BigR (fun (BigR br) -> br)


    type MinMax = MinMax of BigRational * BigRational
    let minMaxArb () =
        bigRGen
        |> Gen.map abs
        |> Gen.two
        |> Gen.map (fun (br1, br2) ->
            let br1 = br1.Numerator |> BigRational.FromBigInt
            let br2 = br2.Numerator |> BigRational.FromBigInt
            if br1 >= br2 then br2, br1 else br1, br2
            |> fun (br1, br2) ->
                if br1 = br2 then br1, br2 + 1N else br1, br2
        )
        |> Arb.fromGen
        |> Arb.convert MinMax (fun (MinMax (min, max)) -> min, max)


    type ListOf37<'a> = ListOf37 of 'a List
    let listOf37Arb() =
        Gen.listOfLength 37 Arb.generate
        |> Arb.fromGen
        |> Arb.convert ListOf37 (fun (ListOf37 xs) -> xs)


    let addToConfig config =
        {
            config with
                maxTest = 1000
                arbitrary = typeof<BigR>.DeclaringType::config.arbitrary
        }





testList "test setting min and max to abcdef eqs" [

    let config = Generators.addToConfig FsCheckConfig.defaultConfig
    let abdef = ["a"; "b"; "d"; "e"; "f"]

    testPropertyWithConfig config "can set any"
    <| fun (i1: int) i2 i3 i4 i5
           (Generators.MinMax (min1, max1))
           (Generators.MinMax (min2, max2))
           (Generators.MinMax (min3, max3))
           (Generators.MinMax (min4, max4)) ->
        let x1, x2, x3, x4 =
            [i1; i2; i3; i4; i5]
            |> List.mapi (fun i x -> i, x)
            |> List.sortBy snd
            |> List.map fst
            |> function
            | i1::i2::i3::i4::_ ->
                (abdef[i1],abdef[i2], abdef[i3], abdef[i4])
            | _ -> failwith "cannot process"

        try
//            printfn $"solving {x1}, {x2}, {x3}, {x4}"
            eqs
//            |> nonZeroNegative
            |> solveMinIncl x1 min1
            |> solveMaxIncl x1 max1
            |> solveMinIncl x2 min2
            |> solveMaxIncl x2 max2
            |> solveMinIncl x3 min3
            |> solveMaxIncl x3 max3
            |> solveMinIncl x4 min4
            |> solveMaxIncl x4 max4
            // |> solveMinIncl x5 min5
            // |> solveMaxIncl x5 max5
            // |> printEqs
            |> ignore
            Expect.isTrue "true is true" true
        with
        | :? Exceptions.SolverException as e ->
            match e.Data0 with
            | [Exceptions.SolverTooManyLoops (n, xs)] ->
                printfn "ran into a loop with:"
                xs
                |> printEqs
                |> ignore
                Expect.isTrue "not true" false
            | _ ->
                Expect.isTrue "true is true" true
        | _ -> Expect.isTrue "true is true" true
]
|> run



open Equations


let twoCompEqs =
    {
        Components =
            [
                {
                    Name = "CMPA"
                    Items = ["ITMA"]
                }
                {
                    Name = "CMPB"
                    Items = ["ITMB"]
                }
            ]
    }
    |> create



testList "can set any var of compA or qty or cnc of compB" [
    let config = Generators.addToConfig FsCheckConfig.defaultConfig

    let vars =
        twoCompEqs
        |> List.collect Equation.toVars
        |> List.map (fun v -> v.Name |> Name.toString)
        |> List.distinct
        |> fun vars ->
            let cmps =
                vars
                |> List.filter (fun s ->
                    s.Contains("CMPB") || s.Contains("ITMB")
                )
                |> List.filter (fun s ->
                    s.Contains("cmp_cnc") || s.Contains("CMPB_qty")
                )
            vars
            |> List.filter (fun s ->
                s.Contains("CMPB") |> not &&
                s.Contains("ITMB") |> not &&
                s.Contains("ord") |> not
            )
//            |> List.append cmps


    testPropertyWithConfig config "can set any"
    <| fun (Generators.ListOf37 (indxs : int list))
           (Generators.ListOf37 (xs : Generators.MinMax list))
           ->
        let vars =
            indxs
            |> List.take vars.Length
            |> List.mapi (fun i x -> i, x)
            |> List.sortBy snd
            |> List.map fst
            |> List.take 12
            |> List.map (fun i -> vars[i])

        try
            vars
            |> List.fold (fun acc v ->
                let i, eqs = acc
                let eqs =
                    let (Generators.MinMax(min, max)) =
                        xs[i]
                    eqs
                    |> solveMinIncl v min
                    |> solveMaxIncl v max
                (i + 1, eqs)
            ) (0, twoCompEqs)
            // |> snd
            // |> printEqs
            |> ignore
            Expect.isTrue "true is true" true
        with
        | :? Exceptions.SolverException as e ->
            match e.Data0 with
            | [Exceptions.SolverTooManyLoops (n, xs)] ->
                vars
                |> String.concat ", "
                |> printfn "ran into a loop with:%s\n"
                xs |> printEqs |> ignore
                Expect.isTrue "not true" false
            | _ ->
                Expect.isTrue "true is true" true
        | _ -> Expect.isTrue "true is true" true
]
|> run

