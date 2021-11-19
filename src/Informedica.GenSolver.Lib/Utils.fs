namespace Informedica.GenSolver.Lib

/// ToDo: should move to Informedica.Utils.Lib


/// Helper functions for `BigRational`
module BigRational = 
    
    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL.BigRational
    
    /// ToDo: doesn't return `NoOp` but fails, 
    /// have to rewrite
    /// 
    /// Match an operator `op` to either
    /// multiplication, division, addition
    /// or subtraction, returns `NoOp` when
    /// the operation is neither.
    let (|Mult|Div|Add|Subtr|) op =
        match op with
        | _ when op |> opIsMult  -> Mult
        | _ when op |> opIsDiv   -> Div
        | _ when op |> opIsAdd   -> Add
        | _ when op |> opIsSubtr -> Subtr
        | _ -> failwith "Operator is not supported"

    let private toMultipleOf b d n  =
        let m = (n / d) |> BigRational.ToBigInt |> BigRational.FromBigInt
        if b then
            if m * d < n then (m + 1N) * d else m * d
        else 
            if m * d > n then (m - 1N) * d else m * d


    let toMinMultipleOf = toMultipleOf true

    let toMaxMultipleOf = toMultipleOf false

        
    let calcMinOrMaxToMultiple isMax isIncl incrs m =
        incrs
        |> Set.fold (fun (b, acc) i ->
            let ec = if isMax then (>=) else (<=)
            let nc = if isMax then (>) else (<)
            let ad = if isMax then (-) else (+)

            let m' = 
                if isMax then m |> toMaxMultipleOf i
                else m |> toMinMultipleOf i
                
            let m' = 
                if (isIncl |> not) && (m' |> ec <| m) then 
                    printfn $"recalc because is excl: {(m' |> ad <| i) }"
                    (m' |> ad <| i) 
                else m'
            
            match acc with 
            | Some a -> if (m' |> nc <| a) then (true, Some m') else (b, Some a)
            | None   -> (true, Some m')
        ) (isIncl, None)
        |> fun (b, r) -> b, r |> Option.defaultValue m


    let maxInclMultipleOf = calcMinOrMaxToMultiple true true 

    let maxExclMultipleOf = calcMinOrMaxToMultiple true false 

    let minInclMultipleOf = calcMinOrMaxToMultiple false true

    let minExclMultipleOf = calcMinOrMaxToMultiple false false


/// Helper functions for `List`
module List =

    /// Replace an element in a list
    /// when the `pred` function returns `true`.
    let replace pred x xs =
        match xs |> List.tryFindIndex pred with
        | Some(ind) ->
            (xs |> Seq.take ind |> Seq.toList) @ [x] @ 
            (xs |> Seq.skip (ind + 1) |> Seq.toList)
        | None -> xs

    let distinct xs = xs |> Seq.ofList |> Seq.distinct |> Seq.toList


    let replaceOrAdd pred x xs =
        if xs |> List.exists pred then
            xs 
            |> List.map (fun x' ->
                if x' |> pred then x else x'
            )
        else x::xs


/// Helper functions for `Array`
module Array = 
    
    let replace pred x xs = 
        xs 
        |> Array.toList 
        |> List.replace pred x
        |> List.toArray


/// Helper functions for `Option`
module Option = 

    let none _ = None


