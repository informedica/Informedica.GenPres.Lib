namespace Informedica.GenForm.Lib


module Mapping =

    open Informedica.Utils.Lib.BCL


    let routeMapping =
        Web.getDataFromSheet Web.dataUrlId2 "Routes"
        |> fun data ->
            let getColumn =
                data
                |> Array.head
                |> Csv.getStringColumn

            data
            |> Array.tail
            |> Array.map (fun r ->
                let get = getColumn r

                {|
                    Long = get "Long" 
                    Short = get "Short"
                |}
            )


    let unitMapping =
        Web.getDataFromSheet Web.dataUrlId2 "Units"
        |> fun data ->
            let getColumn =
                data
                |> Array.head
                |> Csv.getStringColumn

            data
            |> Array.tail
            |> Array.map (fun r ->
                let get = getColumn r

                {|
                    Long = get "Z-IndexUnitLong" 
                    Short = get "Z-IndexUnitShort"
                    MV = get "MetaVisionUnit"
                |}
            )


    let mapRoute rte =
        routeMapping
        |> Array.tryFind (fun r ->
            r.Long |> String.equalsCapInsens rte
        )
        |> Option.map (fun r -> r.Short)


    let mapUnit unt =
        unitMapping
        |> Array.tryFind (fun r ->
            r.Long |> String.equalsCapInsens unt
        )
        |> Option.map (fun r -> r.MV)

