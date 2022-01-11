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

    let set n p eqs = 
        let n = n |> Name.createExc
        match eqs |> Api.setVariableValues None n p with
        | Some var ->
            eqs
            |> List.map (fun e ->
                e |> Equation.replace var
            )
        | None -> eqs

    let setMinIncl n min = set n (MinInclProp min)
    let setMinExcl n min = set n (MinExclProp min)
    let setMaxIncl n max = set n (MaxInclProp max)
    let setMaxExcl n max = set n (MaxExclProp max)
    let setValues n vals = set n (ValsProp (vals |> Set.ofSeq))


    // x = b/(b + c)
    // y = (b + c)/b
    let calcXY n p (eqs : Equation list) = eqs
        // let eqs = eqs |> set n p
            
        // let getVar n = 
        //     eqs 
        //     |> List.collect Equation.toVars
        //     |> List.find (Variable.getName >> Name.toString >>((=) n))
        
        // let b = (getVar "b").Values |> ValueRange.getValueSet |> Option.defaultValue Set.empty
        // let c = (getVar "c").Values |> ValueRange.getValueSet |> Option.defaultValue Set.empty
        
        // let eqs =
        //     if b |> Set.isEmpty || c |> Set.isEmpty then eqs
        //     else
        //         let xs =
        //             [
        //                 for i in b do
        //                     for j in c do
        //                         i / (i + j)
        //             ]
        //         let ys = xs |> List.map (fun x -> 1N / x)
        //         eqs 
        //         |> setValues "x" xs
        //         |> setValues "y" ys
        // //eqs
        // let b_min, b_max = 
        //     getVar "b" 
        //     |> Variable.getValueRange
        //     |> fun vr -> 
        //         vr |> ValueRange.getMin,
        //         vr |> ValueRange.getMax

        // let c_min, c_max = 
        //     getVar "c" 
        //     |> Variable.getValueRange
        //     |> fun vr -> 
        //         vr |> ValueRange.getMin,
        //         vr |> ValueRange.getMax

        // let b_max, x_max = 
        //     match b_max, c_min with
        //     | Some max, Some min -> 
        //         let b1, max = max |> ValueRange.Maximum.maxToBoolBigRational
        //         let b2, min = min |> ValueRange.Minimum.minToBoolBigRational
        //         (b1 && b2), max / (max + min) |> Some
        //     | _ -> false, None

        // let b_min, x_min = 
        //     match b_min, c_max with
        //     | Some min, Some max -> 
        //         let b1, max = max |> ValueRange.Maximum.maxToBoolBigRational
        //         let b2, min = min |> ValueRange.Minimum.minToBoolBigRational
        //         (b1 && b2), min / (min + max) |> Some
        //     | _ -> false, None


        // match x_max with
        // | Some max ->
        //     eqs
        //     |> (if b_max then setMaxIncl "x" max else setMaxExcl "x" max)
        //     |> (if b_max then setMinIncl "y" (1N/max) else setMinExcl "y" (1N/max))
        // | None -> eqs
        // |> fun eqs ->
        //     match x_min with
        //     | Some min ->
        //         eqs
        //         |> (if b_min then setMinIncl "x" min else setMinExcl "x" min)
        //         |> (if b_min then setMaxIncl "y" (1N/min) else setMaxExcl "y" (1N/min))
        //     | None -> eqs
        // |> fun eqs ->
        //             // b_{max} = c_{max}*1/(e_{min}/f_{max}-1). 
        //             // a_{max} = c_{max}*1/(1-f_{max}/e_{min}).
        //             let e_min, f_max = 
        //                 let e =
        //                     getVar "e" 
        //                     |> Variable.getValueRange
        //                     |> fun vr -> 
        //                         vr |> ValueRange.getMin
        //                 let f =
        //                     getVar "f" 
        //                     |> Variable.getValueRange
        //                     |> fun vr -> 
        //                         vr |> ValueRange.getMax
        //                 (e, f)

        //             match e_min, f_max, c_max with
        //             | Some e_min, Some f_max, Some c_max ->
        //                 let b_e_min, e_min = e_min |> ValueRange.Minimum.minToBoolBigRational
        //                 let b_f_max, f_max = f_max |> ValueRange.Maximum.maxToBoolBigRational
        //                 let b_c_max, c_max = c_max |> ValueRange.Maximum.maxToBoolBigRational
        //                 if (e_min/f_max) <= 1N then eqs
        //                 else
        //                     let b_b_max, b_max =
        //                         (b_e_min && b_f_max && b_c_max),
        //                         (c_max/(e_min/f_max - 1N))
        //                     eqs
        //                     |> (if b_b_max then setMaxIncl "b" b_max else setMaxExcl "b" b_max)
        //             | _ -> eqs


    let printEqs = Solver.printEqs true procss
    let solve n p eqs =
        let logger = 
            fun s ->
                File.AppendAllLines("order.log", [s])
            |> SolverLogging.logger
        eqs
//        |> calcXY n p
        |> Api.solve Solver.sortQue logger None (n |> Name.createExc) p

    let solveMinIncl n min = solve n (MinInclProp min)
    let solveMinExcl n min = solve n (MinExclProp min)
    let solveMaxIncl n max = solve n (MaxInclProp max)
    let solveMaxExcl n max = solve n (MaxExclProp max)
    let solveValues n vals = solve n (ValsProp (vals |> Set.ofSeq))

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

        match var.Values |> ValueRange.getValueSet with
        | None    -> ()
        | Some vs ->
            for v in vs do
                try
                    eqs
                    |> solveValues n [v]
                    |> ignore
                    printfn $"can set {v}"
                with
                | _ -> 
                    printfn $"cannot set {v}"



open Solve



// minimal failing case
// f = eb/(b + c)
// e = f(b + c)/b
// x = b/(b + c)
// y = (b + c)/b
// y = a/b => a = yb
// x = b/a => b = ax
let failEqs =
    [
        "a = b + c"
        "d = f * a"
        "d = e * b"
    ]
    |> Api.init
    |> nonZeroNegative


failEqs
//|> solveValues "z" [1N]
//|> solveValues "a" [2N]
|> solveValues "b" [1N]
//|> calcXY
//|> printEqs
|> solveValues "c" [1N]
//|> calcXY
//|> printEqs
|> solveValues "e" [1N]
//|> calcXY
|> printEqs
|> findValidValues "f"
//|> solveValues "a" [2N]
//|> printEqs
//|> solveValues "f" [3N] // this fails
//|> solveValues "f" [5N] // but this doesn't fail


//|> ignore
//|> solveMaxExcl "a" 10N // this doesn't start a loop
//|> solveMaxExcl "a" 25N // this doesn't start a loop
//|> solveMaxExcl "a" 26N // this starts a loop
//|> calcXY
|> solveMaxExcl "a" 25N // this starts a loop
|> printEqs
|> ignore


failEqs
//|> solveValues "z" [1N]
|> solveMinExcl "b" 1N
|> printEqs
|> solveMinExcl "c" 1N
|> printEqs
|> solveMaxExcl "c" 20N
|> printEqs
|> solveMinIncl "e" 10N
|> printEqs
|> solveMaxIncl "e" 20N
|> printEqs
// |> solveMinExcl "f" 1N
// |> printEqs
|> solveMaxExcl "a" 20N
|> printEqs
// |> solveMaxExcl "a" (25N) // a = 5/4*c_{max}
|> printEqs
|> ignore


failEqs
|> nonZeroNegative
|> solveValues "z" [1N]
|> printEqs
|> solveMinExcl "b" 1N
|> printEqs
|> solveMinExcl "c" 1N
|> printEqs
|> solveMaxExcl "c" 20N
|> printEqs
|> solveMinIncl "e" 10N
|> printEqs
|> solveMaxIncl "e" 20N
|> printEqs
|> solveMaxExcl "f" 2N
|> printEqs
|> solveMaxIncl "a" 10N
|> printEqs
|> ignore


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
//    |> nonZeroNegative



failEqs
|> solveMinExcl "b" 1N
|> printEqs
|> solveMinExcl "c" 1N
|> printEqs
|> solveMaxExcl "c" 20N
|> printEqs
|> solveMinIncl "e" 10N
|> printEqs
|> solveMaxIncl "e" 20N
|> printEqs
|> solveMinExcl "f" 1N
|> printEqs
|> solveMaxExcl "f" 2N
|> printEqs
//|> solveMaxExcl "a" 10N // this doesn't start a loop
//|> solveMaxExcl "a" 25N // this doesn't start a loop
//|> solveMaxExcl "a" 26N // this starts a loop
|> solveMaxExcl "a" 30N // this starts a loop
|> printEqs
|> ignore



failEqs
//|> solveValues "b" [1N]
//|> printEqs
//|> solveValues "c" [1N]
//|> printEqs
|> solveValues "c" [20N]
|> printEqs
//|> solveValues "e" [10N]
//|> printEqs
|> solveValues "e" [20N]
|> printEqs
//|> solveMinExcl "f" 1N
//|> printEqs
|> solveValues "f" [2N]
|> printEqs
//|> solveMaxExcl "a" 10N // this doesn't start a loop
//|> solveMaxExcl "a" 25N // this doesn't start a loop
//|> solveMaxExcl "a" 26N // this starts a loop
|> solveValues "a" [30N] // this starts a loop
|> printEqs
|> ignore