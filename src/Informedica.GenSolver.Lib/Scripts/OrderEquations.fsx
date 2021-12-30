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

module Api = Informedica.GenSolver.Lib.Api
module Solver = Informedica.GenSolver.Lib.Solver
module Name = Variable.Name
module ValueRange = Variable.ValueRange

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
    Api.solve id logger None n p eqs
//    |> fun eqs -> eqs |> printEqs |> printfn "%A"; eqs

let setMinIncl n min = solve n (MinInclProp min)
let setMinExcl n min = solve n (MinExclProp min)
let setMaxIncl n max = solve n (MaxInclProp max)
let setMaxExcl n max = solve n (MaxExclProp max)
let setValues n vals = solve n (ValsProp (vals |> Set.ofSeq))

let init     = Api.init
let nonZeroNegative = Api.nonZeroNegative


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
        "orb_qty = sum(cmp_orb_qty)"
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
    |> Api.init
    |> nonZeroNegative


let pcmEqs =
    createEqs { Components = [ {Name = "pcm_supp"; Items = ["paracetamol"]} ] }


pcmEqs
// dose
|> setValues "pres_freq" [3N;4N]
|> setMinIncl "paracetamol_dos_tot_adj" 60N
|> setMaxIncl "paracetamol_dos_tot_adj" 90N
|> setMaxIncl "paracetamol_dos_qty" 1000N
|> setMaxIncl "paracetamol_dos_tot" 4000N
// administration
|> setValues "orb_dos_qty" [1N]
// preparation
|> setValues "orb_qty" [1N]
// patient
|> setValues "ord_adj" [10N]
// product 
|> setValues "paracetamol_cmp_qty" [60N; 120N; 240N; 500N; 1000N]
|> setValues "pcm_supp_qty" [1N]
|> printEqs
|> ignore


let gentaEqs =
    {
        Components =
            [
                { 
                    Name = "genta_sol"
                    Items = ["gentamicin"]
                }
                {
                    Name = "saline"
                    Items = ["sodium"; "chloride"]
                }
            ]
    }
    |> createEqs 


gentaEqs
// dose
|> setValues "pres_freq" [1N]
|> setMinIncl "gentamicin_dos_tot_adj" 5N
|> setMaxIncl "gentamicin_dos_tot_adj" 7N
// administration
|> setMinIncl "pres_time" (1N/2N)
|> setMaxIncl "pres_time" 1N
|> setMaxIncl "orb_dos_rte" 999N
|> setMaxIncl "orb_dos_qty" 50N
// preparation
|> setMaxIncl "orb_qty" 50N
|> setMaxIncl "gentamicin_orb_cnc" 2N
// patient
|> setValues "ord_adj" [10N]
// product 
|> setValues "genta_sol_qty" [2N; 10N]
|> setValues "gentamicin_cmp_cnc" [10N]
|> printEqs
|> ignore