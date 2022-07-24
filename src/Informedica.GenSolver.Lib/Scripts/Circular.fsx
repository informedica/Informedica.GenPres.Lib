// throw this line first to the fsi

#load "../Scripts/load.fsx"

#time

open Informedica.GenSolver.Lib

open System
open System.IO

open Informedica.GenSolver.Lib
open Informedica.Utils.Lib.BCL
open MathNet.Numerics
open Types

module Name = Variable.Name
module ValueRange = Variable.ValueRange
module Minimum = ValueRange.Minimum
module Maximum = ValueRange.Maximum
module ValueSet = ValueRange.ValueSet

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__



module Solve =

    let procss s =
        File.AppendAllLines("order.log", [s])
        $"{s} " |> printfn "%s"


    let printEqs = Solver.printEqs true procss
    let solve n p eqs =
        let logger = 
            fun s ->
                File.AppendAllLines("order.log", [s])
            |> SolverLogging.logger
        let n = n |> Name.createExc
        try
            Api.solve Solver.sortQue logger None n p eqs
        with
        | _ -> 
            procss $"cannot set {n |> Name.toString} with {p}"
            eqs 


    let solveValues n vals = solve n (vals |> ValueSet.create |> ValsProp)


    let init     = Api.init
    let nonZeroNegative = Api.nonZeroNegative


    type Take = | TakeFromMin of int | TakeFromMax of int


    let pick s incr take (eqs : Equation list) =
        let var = 
            eqs
            |> List.collect Equation.toVars
            |> List.tryFind (fun v ->
                v 
                |> Variable.getName 
                |> Name.toString 
                |> fun x -> x = s
            )
            |> Option.get

        match var.Values |> ValueRange.getValSet with
        | None    -> 
            match incr with
            | None -> eqs
            | Some incr ->
                let min, max = 
                    var.Values |> ValueRange.getMin,
                    var.Values |> ValueRange.getMax
                match min, max with
                | Some min, Some max ->
                    let (incl_min, min), (incl_max, max) = 
                        min |> ValueRange.Minimum.toBoolBigRational,
                        max |> ValueRange.Maximum.toBoolBigRational
                    let min =
                        if min = 0N then incr
                        else 
                            let m = min |> BigRational.toMultipleOf incr
                            if incl_min || m > min then m
                            else 
                                m + incr
                    let max =
                        let m = max |> BigRational.toMultipleOf incr
                        if incl_max || m < max then m
                        else 
                            m - incr
                    let vs = 
                        take
                        |> function
                        | TakeFromMin x -> [min..incr..max] |> List.take x
                        | TakeFromMax x -> [min..incr..max] |> List.rev |> List.take x
                    eqs |> solveValues s vs
                | _ -> eqs
        | Some (ValueSet vs) ->
            match take with
            | TakeFromMin x -> 
                    let x = if x > vs.Count then vs.Count else x
                    let vs = vs |> Set.toList |> List.take x
                    try
                        eqs
                        |> solveValues s vs
                    with
                    | _ -> 
                        printfn $"cannot set {vs}"
                        eqs
            | TakeFromMax x -> 
                    let x = if x > vs.Count then vs.Count else x
                    let vs = vs |> Set.toList |> List.rev |> List.take x
                    try
                        eqs
                        |> solveValues s vs
                    with
                    | _ -> 
                        printfn $"cannot set {vs}"
                        eqs



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


    let createEqs (orb : Orb) = 
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
            "orb_qty = sum(cmp_orb_qty)"
            // only add these when single values are used!!
            "orb_dos_qty = sum(cmp_dos_qty)"
            "orb_dos_tot = sum(cmp_dos_tot)"
            "orb_dos_rte = sum(cmp_dos_rte)"
            "orb_dos_qty_adj = sum(cmp_dos_qty_adj)"
            "orb_dos_tot_adj = sum(cmp_dos_tot_adj)"
            "orb_dos_rte_adj = sum(cmp_dos_rte_adj)"
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



open Equations


let eqs =
    {
        Components =
            [
                { 
                    Name = "cmp_a"
                    Items = ["itm_a"]
                }
                {
                    Name = "cmp_b"
                    Items = ["itm_b"]
                }
            ]
    }
    |> createEqs 
    |> Api.init
    |> List.map Equation.toVars
    |> List.map (fun vars -> 
        vars
        |> List.map (fun v -> v.Name |> Name.toString)
    )

let vars = eqs |> List.collect id |> List.distinct

vars
|> List.iter (fun v ->
    let eqs =
        eqs 
        |> List.filter (fun eq -> eq |> List.exists ((=) v))
        |> List.map (String.concat ", ")
    printfn $"""
{v} in: 
{eqs |> String.concat "\n"}"""
)

