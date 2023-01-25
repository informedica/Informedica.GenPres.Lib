namespace Informedica.ZIndex.Lib


module Substance =

    open Informedica.Utils.Lib


    let create id nm ms mr =
        {
            Id = id
            Name = nm
            Mole = ms
            MoleReal = mr
        }


    let cache (sbs : Substance []) = Json.cache FilePath.substanceCache sbs


    let parse () =
        Zindex.BST750T.records ()
        |> Array.filter (fun r -> r.MUTKOD <> 1)
        |> Array.map (fun r ->
            create r.GNGNK r.GNGNAM r.GNMOLE r.GNMOLS)


    let _get _ =
        if FilePath.substanceCache  |> File.exists then
            FilePath.substanceCache
            |> Json.getCache
        else
            printfn "No cache creating Substance"
            let substs = parse ()
            substs |> Json.cache FilePath.substanceCache
            substs


    let get : unit -> Substance [] =
        Memoization.memoize _get


    let load () = get () |> ignore
