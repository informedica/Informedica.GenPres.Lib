namespace Informedica.ZForm.Lib


[<AutoOpen>]
module Utils =

    open System
    open System.IO
    open System.Net.Http
    open Informedica.Utils.Lib.BCL

    (*
    module List =

        open Informedica.Utils.Lib

        let toString xs = List.toString2 xs



    module Csv_ =


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
    *)

    module MinIncrMax =

        open MathNet.Numerics
        
        open Informedica.GenUnits.Lib
        open Informedica.GenCore.Lib.Ranges


        //ToDo move to use case lib
        let ageToString { Min = min; Max = max } =
            let oneWk = 1N |> ValueUnit.createSingle ValueUnit.Units.Time.week
            let oneMo = 1N |> ValueUnit.createSingle ValueUnit.Units.Time.month
            let oneYr = 1N |> ValueUnit.createSingle ValueUnit.Units.Time.year

            let convert =
                let c vu =
                    match vu with
                    | _ when vu <? oneWk -> vu ==> ValueUnit.Units.Time.day
                    | _ when vu <? oneMo -> vu ==> ValueUnit.Units.Time.week
                    | _ when vu <? oneYr -> vu ==> ValueUnit.Units.Time.month
                    | _ -> vu ==> ValueUnit.Units.Time.year
                Option.bind (Limit.apply c c >> Some)

            { Min = min |> convert; Incr = None; Max = max |> convert } |> MinIncrMax.toString "van " "van " "tot " "tot "


        //ToDo move to use case lib
        let gestAgeToString { Min = min; Max = max } =

            let convert =
                let c vu = vu ==> ValueUnit.Units.Time.week
                Option.bind (Limit.apply c c >> Some)

            { Min = min |> convert; Incr = None;  Max = max |> convert } |> MinIncrMax.toString "van " "van " "tot " "tot "



    module Web =

        open Informedica.Utils.Lib

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

