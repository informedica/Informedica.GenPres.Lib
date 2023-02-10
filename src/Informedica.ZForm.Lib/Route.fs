namespace Informedica.ZForm.Lib


module Route =

    open Informedica.Utils.Lib.BCL
    open Informedica.ZIndex.Lib



    let createRoute s = Route.c


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