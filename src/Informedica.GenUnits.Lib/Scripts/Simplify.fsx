
//#I __SOURCE_DIRECTORY__

#load "load-utils.fsx"
#load "load.fsx"

open MathNet.Numerics

open Informedica.GenUnits.Lib
open Informedica.Utils.Lib.BCL
open ValueUnit

module List =
    
    let remove1 pred =
        List.fold (fun acc x ->
            let b, xs = acc
            if b then (true, x::(acc |> snd))
            else 
                if x |> pred then (true, xs)
                else (false, x::(acc |> snd))
        ) (false, [])
        >> snd

let simplify2 u =

    let simpl u =
        let rec numDenom b u =
            match u with
            | CombiUnit(ul, OpTimes, ur) ->
                let lns, lds = ul |> numDenom b
                let rns, rds = ur |> numDenom b 
                lns @ rns, lds @ rds

            | CombiUnit(ul, OpPer, ur) ->
                if b then
                    let lns, lds = ul |> numDenom true
                    let rns, rds = ur |> numDenom false 
                    lns @ rns, lds @ rds
                else 
                    let lns, lds = ur |> numDenom true
                    let rns, rds = ul |> numDenom false 
                    lns @ rns, lds @ rds
            | _ -> if b then (u |> getUnits, []) else ([], u |> getUnits)

        let rec build ns ds u =
            match ns, ds with
            | [], _ -> 
                match ds with
                | [] -> u
                | _ ->
                    let d = ds |> List.reduce times
                    if u = NoUnit then 
                        Count(Times 1N) |> per d
                    else u |> per d
            | h::tail, _ ->
                if ds |> List.exists (Group.eqsGroup h) then
                    build tail (ds |> List.remove1 (Group.eqsGroup h)) u
                else
                    if u = NoUnit then h
                    else u |> times h
                    |> build tail ds 

        let ns, ds = u |> numDenom true 
    
        printfn "num: %A" ns
        printfn "den: %A" ds

        NoUnit
        |> build ns ds
        |> (fun u -> if u = NoUnit then count else u)

    u
    |> function
    | _ when u = NoUnit -> u
    | _ ->
        u
        |> simpl


let piece = Units.General.general "piece" 
let mg = Units.Mass.milliGram

// No unit is no unit
NoUnit
|> simplify2

// No unit / no unit = no unit
NoUnit
|> per NoUnit 
|> simplify2

// piece = piece
piece
|> simplify2

// piece / piece = 1
piece |> per piece
|> simplify2

// piece / mg = piece / mg
piece |> per mg
|> simplify2

// piece * mg / piece = mg
piece |> times mg |> per piece
|> simplify2

// piece * mg / piece / mg = 1
piece |> times mg |> per piece |> per mg
|> simplify2


// (mg / piece) / (mg / (piece / mg)) = 1 / mg
(mg |> per piece) |> per (mg |> per (piece |> per mg))
|> simplify2

