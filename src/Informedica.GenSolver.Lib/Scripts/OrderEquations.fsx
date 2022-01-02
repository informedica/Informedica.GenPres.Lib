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


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


module Solve =

    let procss s =
        File.AppendAllLines("order.log", [s])
        $"{s} " |> printfn "%s"


    let printEqs = Solver.printEqs true procss
    let solve n p eqs =
        try
            let logger = 
                fun s ->
                    File.AppendAllLines("order.log", [s])
                |> SolverLogging.logger
            let n = n |> Name.createExc
            Api.solve Solver.sortQue logger None n p eqs
        with
        |  e -> 
            printfn $"{e}"
            eqs

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

open Solve


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
// patient
|> setValues "ord_adj" [1N]
// dose
|> setValues "pres_freq" [1N]
|> setMinExcl "gentamicin_dos_tot_adj" 5N
|> setMaxExcl "gentamicin_dos_tot_adj" 7N
// administration
|> setMinIncl "pres_time" (1N/2N)
|> setMaxIncl "pres_time" 1N
|> setMinIncl "orb_dos_rte" 1N
|> setMaxIncl "orb_dos_rte" 999N
|> setMinIncl "orb_dos_qty" (1N/10N)
|> setMaxIncl "orb_dos_qty" 500N
|> setMinIncl "orb_dos_qty_adj" (1N/10N)
|> setMaxIncl "orb_dos_qty_adj" 5N
// preparation
|> setMinIncl "orb_qty" 1N
|> setMaxIncl "orb_qty" 500N
|> setMinExcl "gentamicin_orb_cnc" (1N/100N)
|> setMaxExcl "gentamicin_orb_cnc" 2N
// product 
|> setValues "genta_sol_qty" [2N; 10N]
|> setValues "gentamicin_cmp_cnc" [10N; 40N]
|> setValues "gentamicin_cmp_qty" [20N;80N;400N]
// set values
// |> setValues "orb_qty" [2N/5N..1N/10N..10N]
// |> setValues "orb_dos_qty" [2N/5N..1N/10N..5N]
// |> setValues "orb_dos_rte" [13N/5N..1N/10N..10N]
|> printEqs
|> ignore


// Problem case results in a loop
gentaEqs
// dose
|> setValues "pres_freq" [1N]
|> setMinExcl "gentamicin_dos_tot_adj" 5N
|> setMaxExcl "gentamicin_dos_tot_adj" 7N
// administration
|> setMinIncl "pres_time" (1N/2N)
|> setMaxIncl "pres_time" 1N
|> setMinIncl "orb_dos_rte" 1N
|> setMaxIncl "orb_dos_rte" 999N
|> setMinIncl "orb_dos_qty" (1N/10N)
|> setMaxIncl "orb_dos_qty" 500N
|> setMinIncl "orb_dos_qty_adj" (1N/10N)
|> setMaxIncl "orb_dos_qty_adj" 5N
// preparation
|> setMinIncl "orb_qty" 1N
|> setMaxIncl "orb_qty" 500N
|> setMinExcl "gentamicin_orb_cnc" (1N/100N)
|> setMaxExcl "gentamicin_orb_cnc" 2N
// patient
|> setMinExcl "ord_adj" (2N/10N)
|> setMaxIncl "ord_adj" 250N
// product
|> setValues "genta_sol_qty" [2N; 10N]
|> setValues "gentamicin_cmp_cnc" [10N; 40N]
|> setValues "gentamicin_cmp_qty" [20N;80N;400N]
// set values
|> printEqs
|> setValues "ord_adj" [1N]
|> printEqs
|> ignore


// gentamicin_dos_tot <1N..1000N> = gentamicin_dos_tot_adj <5N..7N> * ord_adj [1] 
// gentamicin_dos_tot <5N..7N> = gentamicin_dos_qty <1N..1000N> * pres_freq [1] 
// gentamicin_dos_qty <5N..7N> = gentamicin_cmp_cnc [10, 40] * genta_sol_dos_qty <1/40N..100N> 
// genta_sol_dos_tot <1/40N..100N> = genta_sol_dos_qty <1/8N..7/10N> * pres_freq [1] 
// orb_dos_tot <1/2N..500N] = genta_sol_dos_tot <1/8N..7/10N> + saline_dos_tot <9/25N..19999/40N> 
// genta_sol_dos_tot <1/8N..7/10N> = genta_sol_orb_cnc <1/40N..7/25N> * orb_dos_tot <1/2N..500N] 
// orb_dos_tot <1/2N..28N> = genta_sol_dos_tot <1/8N..7/10N> + saline_dos_tot <9/25N..3999/8N> 
// saline_dos_tot <9/25N..223/8N> = saline_dos_qty <9/25N..3999/8N> * pres_freq [1] 
// saline_dos_qty <9/25N..223/8N> = saline_dos_rte <9/25N..39959/40N> * pres_time [1/2N..1N] 
// orb_dos_rte [1N..999N] = genta_sol_dos_rte <1/40N..999/5N> + saline_dos_rte <9/25N..223/4N> 
// gentamicin_dos_rte <1N..1998N> = gentamicin_orb_cnc <1N..2N> * orb_dos_rte [1N..5111/20N> 
// gentamicin_dos_rte <1N..5111/10N> = gentamicin_cmp_cnc [10, 40] * genta_sol_dos_rte <1/40N..999/5N> 
// orb_dos_rte [1N..5111/20N> = genta_sol_dos_rte <1/40N..5111/100N> + saline_dos_rte <9/25N..223/4N> 
// gentamicin_dos_rte <1N..5111/10N> = gentamicin_orb_cnc <1N..2N> * orb_dos_rte [1N..5343/50N> 
// gentamicin_dos_rte <1N..5343/25N> = gentamicin_cmp_cnc [10, 40] * genta_sol_dos_rte <1/40N..5111/100N> 
// orb_dos_rte [1N..5343/50N> = genta_sol_dos_rte <1/40N..5343/250N> + saline_dos_rte <9/25N..223/4N> 
// gentamicin_dos_rte <1N..5343/25N> = gentamicin_orb_cnc <1N..2N> * orb_dos_rte [1N..38561/500N> 
// gentamicin_dos_rte <1N..38561/250N> = gentamicin_cmp_cnc [10, 40] * genta_sol_dos_rte <1/40N..5343/250N> 
// orb_dos_rte [1N..38561/500N> = genta_sol_dos_rte <1/40N..38561/2500N> + saline_dos_rte <9/25N..223/4N> 
// gentamicin_dos_rte <1N..38561/250N> = gentamicin_orb_cnc <1N..2N> * orb_dos_rte [1N..44484/625N> 
// gentamicin_dos_rte <1N..88968/625N> = gentamicin_cmp_cnc [10, 40] * genta_sol_dos_rte <1/40N..38561/2500N> 
// orb_dos_rte [1N..44484/625N> = genta_sol_dos_rte <1/40N..44484/3125N> + saline_dos_rte <9/25N..223/4N> 
// gentamicin_dos_rte <1N..88968/625N> = gentamicin_orb_cnc <1N..2N> * orb_dos_rte [1N..874811/12500N> 
// gentamicin_dos_rte <1N..874811/6250N> = gentamicin_cmp_cnc [10, 40] * genta_sol_dos_rte <1/40N..44484/3125N> 
// orb_dos_rte [1N..874811/12500N> = genta_sol_dos_rte <1/40N..874811/62500N> + saline_dos_rte <9/25N..223/4N> 
// gentamicin_dos_rte <1N..874811/6250N> = gentamicin_orb_cnc <1N..2N> * orb_dos_rte [1N..2179593/31250N> 
// gentamicin_dos_rte <1N..2179593/15625N> = gentamicin_cmp_cnc [10, 40] * genta_sol_dos_rte <1/40N..874811/62500N> 
// orb_dos_rte [1N..2179593/31250N> = genta_sol_dos_rte <1/40N..2179593/156250N> + saline_dos_rte <9/25N..223/4N> 




// minimal failing case
// a [1N..999N] = b <1/40N..999/5N> + c <9/25N..223/4N> 
// d <1N..1998N> = f <1N..2N> * a [1N..5111/20N> 
// d <1N..5111/10N> = e [10, 40] * b <1/40N..999/5N> 
let failEqs =
    [
        "a = b + c"
        "d = f * a"
        "d = e * b"
    ]
    |> Api.init
    |> nonZeroNegative


failEqs
|> setMinExcl "b" (1N/40N)
|> printEqs
|> setMaxExcl "b" (999N/5N)
|> printEqs
|> setMinExcl "c" (9N/25N)
|> printEqs
|> setMaxExcl "c" (223N/4N)
|> printEqs
|> setMinIncl "e" 10N
|> printEqs
|> setMaxIncl "e" 40N
|> printEqs
|> setMinExcl "f" (1N)
|> printEqs
|> setMaxExcl "f" (2N)
|> printEqs
|> ignore

// d <77/200N..5343/25N> = e [10N..40N] * b <1/40N..5111/100N> 
(5343N/25N) < 40N * (5111N/100N)
5343N/250N * 40N
5343N/250N < 5111N/100N
