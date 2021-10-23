namespace Informedica.Utils.Lib

/// Additional utitilty functions
/// for lists
module List =

    open Informedica.Utils.Lib.BCL


    let remove pred xs =
        match xs |> List.tryFindIndex pred with
        | Some (ind) ->
            (xs |> Seq.take ind |> Seq.toList) @  
            (xs |> Seq.skip (ind + 1) |> Seq.toList)
        | None -> xs            


    /// Replace an element **x** in a list **xs**
    /// when the **pred** function returns `true`. </br>
    /// Note: will only replace the *first* element
    /// that satisfies the condition in **pred**
    let replace pred x xs =
        match xs |> List.tryFindIndex pred with
        | Some(ind) ->
            (xs |> Seq.take ind |> Seq.toList) @ [x] @ 
            (xs |> Seq.skip (ind + 1) |> Seq.toList)
        | None -> xs


    let listFilter p xs =
        xs
        |> List.filter (fun r -> 
            r |> List.exists (fun x -> p x )) 


    let collectLists p xs =
        xs
        |> List.collect (fun r -> 
            r
            |> List.filter (fun x -> p x))


    let pickList pl (xs: 'a List) =
        match pl with
        | [] -> xs
        | _ -> [ for i in pl -> xs.[i] ]


    let inline toString xs =
        match xs with
        | [] -> "[]"
        | _ ->
            let s =
                xs 
                |> List.fold (fun s x -> s + (string) x + ";") "["
            (s
            |> String.subString 0 ((s |> String.length) - 1)) + "]"


