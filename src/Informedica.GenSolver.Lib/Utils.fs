namespace Informedica.GenSolver.Lib

[<AutoOpen>]
module Utils =

    module Constants =


        let MAX_LOOP_COUNT = 10


        let MAX_CALC_COUNT = 5000


        let MAX_BIGINT = 1000000000000000000000I



    /// Helper functions for `BigRational`
    [<RequireQualifiedAccess>]
    module BigRational =

        open MathNet.Numerics
        open Informedica.Utils.Lib.BCL

        /// ToDo: doesn't return `NoOp` but fails,
        /// have to rewrite
        ///
        /// Match an operator `op` to either
        /// multiplication, division, addition
        /// or subtraction, returns `NoOp` when
        /// the operation is neither.
        let (|Mult|Div|Add|Subtr|) op =
            match op with
            | _ when op |> BigRational.opIsMult  -> Mult
            | _ when op |> BigRational.opIsDiv   -> Div
            | _ when op |> BigRational.opIsAdd   -> Add
            | _ when op |> BigRational.opIsSubtr -> Subtr
            | _ -> failwith "Operator is not supported"


        let private toMultipleOf b d n  =
            if d = 0N then n
            else
                let m = (n / d) |> BigRational.ToBigInt |> BigRational.FromBigInt
                if b then
                    if m * d < n then (m + 1N) * d else m * d
                else
                    if m * d > n then (m - 1N) * d else m * d


        let toMinMultipleOf = toMultipleOf true


        let toMaxMultipleOf = toMultipleOf false


        let calcMinOrMaxToMultiple isMax isIncl incrs minOrMax =
            incrs
            |> Set.filter ((<) 0N) // only accept positive incrs
            |> Set.fold (fun (b, acc) i ->
                let ec = if isMax then (>=) else (<=)
                let nc = if isMax then (>) else (<)
                let ad = if isMax then (-) else (+)

                let m =
                    if isMax then minOrMax |> toMaxMultipleOf i
                    else minOrMax |> toMinMultipleOf i

                let m =
                    if (isIncl |> not) && (m |> ec <| minOrMax) then
                        (m |> ad <| i)
                    else m

                match acc with
                | Some a -> if (m |> nc <| a) then (true, Some m) else (b, Some a)
                | None   -> (true, Some m)
            ) (isIncl, None)
            |> fun (b, r) -> b, r |> Option.defaultValue minOrMax


        let maxInclMultipleOf = calcMinOrMaxToMultiple true true

        let maxExclMultipleOf = calcMinOrMaxToMultiple true false

        let minInclMultipleOf = calcMinOrMaxToMultiple false true

        let minExclMultipleOf = calcMinOrMaxToMultiple false false


    [<RequireQualifiedAccess>]
    module Tuple =


        let fstOf3 (one, _, _) = one


        let sndOf3 (_, two, _) = two


        let thdOf3 (_, _, three) = three


    /// Helper functions for `Option`
    [<RequireQualifiedAccess>]
    module Option =

        let none _ = None



    [<RequireQualifiedAccess>]
    module Boolean =

        let returnFalse _ = false

        let returnTrue _ = true


    [<RequireQualifiedAccess>]
    module Set =

        open Informedica.Utils.Lib.BCL

        let removeBigRationalMultiples xs =
            if xs |> Set.isEmpty then xs
            else
                xs
                |> Set.fold (fun acc x1 ->
                    acc
                    |> Set.filter (fun x2 ->
                        x1 = x2 ||
                        x2 |> BigRational.isMultiple x1 |> not
                    )
                ) xs



    /// Helper functions for `List`
    [<RequireQualifiedAccess>]
    module List =

        /// Replace an element in a list
        /// when the `pred` function returns `true`.
        let replace pred x xs =
            xs |> List.map (fun a -> if pred a then x else a)


        let distinct xs = xs |> Seq.ofList |> Seq.distinct |> Seq.toList


        let replaceOrAdd pred x xs =
            if xs |> List.exists pred then
                xs |> replace pred x
            else
                x :: xs


