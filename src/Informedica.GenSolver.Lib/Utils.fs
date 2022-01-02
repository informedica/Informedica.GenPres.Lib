namespace Informedica.GenSolver.Lib

/// ToDo: should move to Informedica.Utils.Lib



/// Helper functions for `Option`
module Option = 

    let none _ = None



module Boolean =

    let returnFalse _ = false

    let returnTrue _ = true



/// Helper functions for `List`
module List =

    /// Replace an element in a list
    /// when the `pred` function returns `true`.
    let replace pred x xs =
        xs
        |> List.map (fun x' ->
            if pred x' then x else x'
        )
        // match xs |> List.tryFindIndex pred with
        // | Some(ind) ->
        //     (xs |> Seq.take ind |> Seq.toList) @ [x] @ 
        //     (xs |> Seq.skip (ind + 1) |> Seq.toList)
        // | None -> xs

    let distinct xs = xs |> Seq.ofList |> Seq.distinct |> Seq.toList


    let replaceOrAdd pred x xs =
        if xs |> List.exists pred then
            xs 
            |> List.map (fun x' ->
                if x' |> pred then x else x'
            )
        else x::xs
