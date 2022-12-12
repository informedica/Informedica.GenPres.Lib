namespace Informedica.GenForm.Lib


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
        let [<Literal>] dataUrlId = "1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g"

        //https://docs.google.com/spreadsheets/d/1AEVYnqjAbVniu3VuczeoYvMu3RRBu930INhr3QzSDYQ/edit?usp=sharing
        let [<Literal>] dataUrlId2 = "1AEVYnqjAbVniu3VuczeoYvMu3RRBu930INhr3QzSDYQ"


        let client = new HttpClient()


        let download url =
            async {
                use! resp = client.GetAsync(Uri(url)) |> Async.AwaitTask
                use! stream = resp.Content.ReadAsStreamAsync() |> Async.AwaitTask
                use reader = new StreamReader(stream)
                return reader.ReadToEnd()
            }


        let getDataFromSheet urlId sheet =
            createUrl sheet urlId
            |> download
            |> Async.RunSynchronously
            |> Csv.parseCSV



    module BigRational =

        open MathNet.Numerics


        let toStringNl (br : BigRational) =
            if br.Denominator = 1I then
                br |> BigRational.ToInt32 |> Int32.toStringNumberNL
            else
                br |> BigRational.toFloat |> Double.toStringNumberNLWithoutTrailingZeros


        let toBrs s =
            s
            |> String.splitAt ';'
            |> Array.choose Double.tryParse
            |> Array.choose BigRational.fromFloat


        let toBrOpt brs = brs |> Array.tryHead


        let tupleBrOpt brs1 brs2 =
            brs1 |> Array.tryHead,
            brs2 |> Array.tryHead
