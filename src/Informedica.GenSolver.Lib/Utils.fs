namespace Informedica.GenSolver.Lib

[<AutoOpen>]
module Utils =

    module Constants =


        let MAX_LOOP_COUNT = 10


        let MAX_CALC_COUNT = 5000


        let MAX_BIGINT = 999999999999999999999999999999999999999999999999I


    (*
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

        *)




