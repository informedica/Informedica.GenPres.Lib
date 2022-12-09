namespace Informedica.GenOrder.Lib



module Api =

    open System
    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL
    open Informedica.GenOrder.Lib


    let inline isBetween (min, max) x =
        match x with
        | None -> true
        | Some x ->
            match min, max with
            | None,     None     -> true
            | Some min, Some max -> x >= min && x <= max
            | Some min, None     -> x >= min
            | None,     Some max -> x <= max


    let filter a w ind med shape route  =
        Data.getDoseRules ()
        |> List.filter (fun d ->
            (ind |> Option.isNone || ind = Some d.Indication) &&
            (med |> Option.isNone || med = Some d.Medication) &&
            (shape |> Option.isNone || shape = Some d.Shape) &&
            (route |> Option.isNone || route = Some d.Route) &&
            a |> isBetween d.Age &&
            w |> isBetween d.Weight
        )


    let filterIndications a w med shape route =
        filter a w None med shape route
        |> List.map (fun d -> d.Indication)
        |> List.distinct


    let filterMedications a w ind shape route =
        filter a w ind None shape route
        |> List.map (fun d -> d.Medication)
        |> List.distinct

    let filterShapes a w ind med route =
        filter a w ind med None route
        |> List.map (fun d -> d.Route)
        |> List.distinct


    let filterRoutes a w ind med shape =
        filter a w ind med shape None
        |> List.map (fun d -> d.Route)
        |> List.distinct


    let tryHead m = (List.map m) >> List.tryHead >> (Option.defaultValue "")
