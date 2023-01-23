namespace Informedica.GenCore.Lib




module MinIncrMax =


    open MathNet.Numerics
    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL


    module Errors =

        type Msg =
            | MinLargerThanMax of minIncl: bool * min: BigRational * maxIncl: bool * max: BigRational
            | NoValidIncrements of BigRational Set


        let toString = function
            | MinLargerThanMax (minIncl, min, maxIncl, max) ->
                let left = if minIncl then "[" else "<"
                let right = if maxIncl then "]" else ">"
                $"{left}{min} > {max}{right}"
            | NoValidIncrements incr ->
                incr |> Set.toString


    let maxMultipleOf incr min =
        let b, br = min
        if b then br |> BigRational.maxInclMultipleOf incr
        else
            br |> BigRational.maxExclMultipleOf incr


    let minMultipleOf incr min =
        let b, br = min
        if b then br |> BigRational.minInclMultipleOf incr
        else
            br |> BigRational.minExclMultipleOf incr


    let minGTmax (maxIncl, max) (minIncl, min) =
        if minIncl && maxIncl then min > max
        else
            min >= max


    let calcIncrement brs =
            brs
            |> Set.filter ((<) 0N)
            |> Set.removeBigRationalMultiples
            |> fun brs1 ->
                if brs1 |> Set.isEmpty then
                    brs
                    |> Errors.NoValidIncrements
                    |> Error
                else
                    brs1
                    |> Ok


    let validate min incr max =
        match min, incr, max with
        | None, None, None
        | Some _, None, None
        | None, None, Some _
        | Some _, None, Some _ -> (min, incr, max) |> Ok
        | None, Some incr, None ->
            incr
            |> calcIncrement
            |> Result.map (fun incr -> (min, incr |> Some, max))
        | Some min, Some incr, None ->
            incr
            |> calcIncrement
            |> Result.map (fun incr ->
                (min |> minMultipleOf incr |> Some,
                incr |> Some, max)
            )
        | None, Some incr, Some max ->
            incr
            |> calcIncrement
            |> Result.map (fun incr ->
                (min,
                incr |> Some,
                max |> maxMultipleOf incr |> Some)
            )
        | Some min, Some incr, Some max ->
            incr
            |> calcIncrement
            |> Result.map (fun incr ->
                (min |> minMultipleOf incr |> Some,
                incr |> Some,
                max |> maxMultipleOf incr |> Some)
            )
        |> function
        | Ok (Some min, incr, Some max) ->
            if min |> minGTmax max then
                let minIncl, min = min
                let maxIncl, max = max
                (minIncl, min, maxIncl, max)
                |> Errors.MinLargerThanMax
                |> Error
            else
                (Some min, incr, Some max)
                |> Ok
        | result -> result


    let toString_  st ste gt gte dotsL dotsR dotsM brToStr min incr max =
        let toStr xs =
            xs
            |> Seq.map brToStr
            |> String.concat ","

        match min, incr, max with
        | None, None, None -> ""
        | Some (minIncl, min), None, None ->
            let gte = if minIncl then gte else gt
            $"{gte}%s{min |> brToStr}"
        | None, Some incr, None ->
            $"{dotsL}{incr |> toStr}{dotsR}"
        | None, None, Some (maxIncl, max) ->
            let ste = if maxIncl then ste else st
            $"{ste}%s{max |> brToStr}"
        | Some (minIncl, min), Some incr, None ->
            let gte = if minIncl then gte else gt
            $"{gte}%s{min |> brToStr}{dotsL}{incr |> toStr}"
        | None, Some incr, Some (maxIncl, max) ->
            let ste = if maxIncl then ste else st
            $"{incr |> toStr}{dotsR}{ste}%s{max |> brToStr}"
        | Some (_, min), None, Some (_, max) ->
            // let gte = if minIncl then gte else gt
            // let ste = if maxIncl then ste else st
            $"%s{min |> brToStr}{dotsM}%s{max |> brToStr}"
        | Some (minIncl, min), Some incr, Some (maxIncl, max) ->
            let gte = if minIncl then gte else gt
            let ste = if maxIncl then ste else st
            $"{gte}%s{min |> brToStr}{dotsL}{incr |> toStr}{dotsR}{ste}%s{max |> brToStr}"


    let toString brToStr min incr max =
        let gt, gte = ">", "\u2265"
        let st, ste = "<", "\u2264"
        let dotsL, dotsR, dotsM = "..", "..", ".."

        toString_ st ste gt gte dotsL dotsR dotsM brToStr min incr max


    let toStringNL brToStr min incr max =
        let gt, gte = "vanaf ", "vanaf "
        let st, ste = " tot ", " tot en met "
        let dotsL, dotsR, dotsM = " per ", "", " - "

        toString_ st ste gt gte dotsL dotsR dotsM brToStr min incr max
