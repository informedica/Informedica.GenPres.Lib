namespace Informedica.ZForm.Lib


[<AutoOpen>]
module Utils =

    open System
    open System.IO
    open System.Net.Http
    open Informedica.Utils.Lib.BCL


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



    module Csv =


        type DataType =
            | StringData
            | FloatData
            | FloatOptionData

        let tryCast dt (x: string) =
            match dt with
            | StringData -> box (x.Trim())
            | FloatData ->
                match Double.TryParse(x) with
                | true, n -> n |> box
                | _ ->
                    $"cannot parse {x} to double"
                    |> failwith
            | FloatOptionData ->
                match Double.TryParse(x) with
                | true, n -> n |> Some |> box
                | _ -> None |> box


        let getColumn dt columns sl s =
            columns
            |> Array.tryFindIndex ((=) s)
            |> function
                | None ->
                    $"""cannot find column {s} in {columns |> String.concat ", "}"""
                    |> failwith
                | Some i ->
                    sl
                    |> Array.item i
                    |> tryCast dt


        let getStringColumn columns sl s =
            getColumn StringData columns sl s |> unbox<string>


        let getFloatColumn columns sl s =
            getColumn FloatData columns sl s |> unbox<float>


        let getFloatOptionColumn columns sl s =
            getColumn FloatOptionData columns sl s
            |> unbox<float option>


        let parseCSV (s: string) =
            s.Split("\n")
            |> Array.filter (String.isNullOrWhiteSpace >> not)
            |> Array.map (String.replace "\",\"" "")
            |> Array.map (String.replace "\"" "")
            |> Array.map (fun s ->
                s.Split("")
                |> Array.map (fun s -> s.Trim())
            )


    module Web =

        let createUrl sheet id =
            $"https://docs.google.com/spreadsheets/d/{id}/gviz/tq?tqx=out:csv&sheet={sheet}"

        //https://docs.google.com/spreadsheets/d/1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g/edit?usp=sharing
        [<Literal>]
        let dataUrlId = "1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g"

        let client = new HttpClient()

        let download url =
            async {
                use! resp = client.GetAsync(Uri(url)) |> Async.AwaitTask
                use! stream = resp.Content.ReadAsStreamAsync() |> Async.AwaitTask
                use reader = new StreamReader(stream)
                return reader.ReadToEnd()
            }


        let getDataFromSheet sheet =
            createUrl sheet dataUrlId
            |> download
            |> Async.RunSynchronously
            |> Csv.parseCSV

