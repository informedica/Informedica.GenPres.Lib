namespace Informedica.ZForm.Lib


module Route =

    open Informedica.Utils.Lib.BCL
    open Informedica.GenCore.Lib.Types.ZIndex
    open Informedica.ZIndex.Lib


    let routeMapping = RuleFinder.routeMapping

    let createRoute s =
        let m =
            routeMapping
            |> List.tryFind (fun (_, rs) ->
                rs
                |> List.exists (String.equalsCapInsens s)
            )
        match m with
        | Some (r, _) -> r
        | _ -> Route.NoRoute


    let eqsRoute r s = s |> createRoute = r


    let toString r =
        match routeMapping |> List.tryFind (fun (r', _) -> r' = r) with
        | Some (_, rl) ->
            match rl |> List.tryHead with
            | Some h -> h
            | None   -> ""
        | None -> ""


    let fromString s =
        routeMapping
        |> List.tryFind (fun (_, rs) -> rs |> List.exists ((=) s))
        |> Option.bind (fun (r, _) -> Some r)