namespace Informedica.GenOrder.Lib


[<AutoOpen>]
module Utils =

    open System
    open System.IO
    open System.Net.Http

    open Informedica.Utils.Lib.BCL


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
            |> Array.map (String.replace "\",\"" "|")
            |> Array.map (String.replace "\"" "")
            |> Array.map (fun s ->
                s.Split("|")
                |> Array.map (fun s -> s.Trim())
            )


    module Web =


        let createUrl sheet id =
            $"https://docs.google.com/spreadsheets/d/%s{id}/gviz/tq?tqx=out:csv&sheet=%s{sheet}"

        //https://docs.google.com/spreadsheets/d/1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g/edit?usp=sharing
        let [<Literal>] constraints = "1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g"


        //https://docs.google.com/spreadsheets/d/1ccW2b6vYVZQsyb8TGpZtJoqtllCbmwrZiIFZ7BN1sB4/edit?usp=sharing
        let [<Literal>] genpres = "1ccW2b6vYVZQsyb8TGpZtJoqtllCbmwrZiIFZ7BN1sB4"


        let client = new HttpClient()


        let download url =
            async {
                use! resp = client.GetAsync(Uri(url)) |> Async.AwaitTask
                use! stream = resp.Content.ReadAsStreamAsync() |> Async.AwaitTask
                use reader = new StreamReader(stream)
                return reader.ReadToEnd()
            }


        let getDataFromExcelSheet excel sheet =
            createUrl sheet excel
            |> download
            |> Async.RunSynchronously
            |> Csv.parseCSV



        let getDataFromConstraints = getDataFromExcelSheet constraints


        let getDataFromGenPres = getDataFromExcelSheet genpres

