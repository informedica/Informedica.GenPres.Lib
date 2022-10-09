namespace Informedica.ZForm.Lib

[<AutoOpen>]
module Utils =


    module List =

        open System

        open Informedica.Utils.Lib.BCL
        open Informedica.Utils.Lib

        let tryFindRest pred xs =
            let rec find x xs notx =
                match xs with
                | [] -> x, notx
                | h::tail ->
                    if h |> pred then find (Some h) tail notx
                    else find x tail ([h] |> List.append notx)

            find None xs []


        let inline toString xs =
            let toStr = Informedica.Utils.Lib.List.toString

            xs
            |> toStr
            |> String.replace "[" ""
            |> String.replace "]" ""
            |> String.replace ";" ","


        let prepend l1 l2 = List.append l2 l1


        let headTail xs =
            match xs with
            | [] -> (None, None)
            | h::tail ->
                match tail |> List.rev with
                | []   -> (Some h, None)
                | t::_ -> (Some h, Some t)


        let inline isConsecutive zero diff xs =
            match xs with
            | []  | [_] -> false
            | _ ->
                xs
                |> List.sort
                |> List.fold (fun acc x ->
                    let isC, prev = acc

                    if prev = zero then (true, x)
                    else
                        (x - prev = diff && isC, x)

                ) (true, zero)
                |> fst
