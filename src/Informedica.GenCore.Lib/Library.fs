namespace Informedica.GenCore.Lib




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



module MinIncrMax =

    open MathNet.Numerics
    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL


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
            |> fun brs ->
                if brs |> Set.isEmpty then None
                else
                    brs
                    |> Some


    let toIncrement min incr max =
        match min, incr, max with
        | None, None, None
        | Some _, None, None
        | None, None, Some _ -> (min, incr, max) |> Some
        | None, Some incr, None ->
            match incr |> calcIncrement with
            | None -> None
            | Some incr ->
                (min, incr |> Some, max) |> Some
        | Some min, None, Some max ->
            if min |> minGTmax max then None
            else
                (min |> Some, None, max |> Some) |> Some
        | Some min, Some incr, None ->
            match incr |> calcIncrement with
            | None -> None
            | Some incr ->
                (min |> minMultipleOf incr |> Some,
                incr |> Some, max)
                |> Some
        | None, Some incr, Some max ->
            match incr |> calcIncrement with
            | None -> None
            | Some incr ->
                (min,
                incr |> Some,
                max |> maxMultipleOf incr |> Some)
                |> Some
        | Some min, Some incr, Some max ->
            match incr |> calcIncrement with
            | None -> None
            | Some incr ->
                if min |> minGTmax max then None
                else
                    (min |> minMultipleOf incr |> Some,
                    incr |> Some,
                    max |> maxMultipleOf incr |> Some)
                    |> Some


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
        | Some (minIncl, min), None, Some (maxIncl, max) ->
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
