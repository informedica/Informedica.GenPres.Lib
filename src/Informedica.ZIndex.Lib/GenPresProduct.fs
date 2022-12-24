namespace Informedica.ZIndex.Lib


module GenPresProduct =

    open Informedica.Utils.Lib.BCL
    open Informedica.Utils.Lib


    type GenPresProduct =
        {
            Name : string
            Shape : string
            Route : string []
            Pharmacologic : string []
            GenericProducts : GenericProduct.GenericProduct []
            DisplayName: string
            Unit : string
            Synonyms: string []
        }


    let create nm sh rt ph gps dpn unt sns =
        {
            Name = nm
            Shape = sh
            Route = rt
            Pharmacologic = ph
            GenericProducts = gps
            DisplayName = dpn
            Unit = unt
            Synonyms = sns
        }


    let private parse (prs : ProductRange.ProductRange []) =
        let gpks =  prs |> Array.map (fun pr -> pr.GPK |> Option.get)

        GenericProduct.get (gpks |> Array.toList)
        |> Array.map (fun gp ->
            let n =
                gp.Substances
                |> Array.map (fun s -> s.SubstanceName)
                |> Array.distinct
                |> Array.fold (fun a s ->
                    if a = "" then s
                    else a + "/" + s) ""
            ((n, gp.Shape), gp))
        |> Array.groupBy (fun (key, gp) -> key)
        |> Array.map (fun ((nm, sh), xs) ->
            let gps = xs |> Array.map (fun (_, gp) -> gp)

            let dpn =
                prs
                |> Array.filter (fun pr ->
                    if pr.GPK |> Option.isNone then false
                    else
                        gps
                        |> Array.exists (fun gp ->
                            pr.GPK |> Option.get = gp.Id
                        )

                )
                |> Array.fold (fun acc pr ->
                    if acc = "" then pr.Generic
                    else acc
                ) ""

            let ph =
                gps
                |> Array.collect (fun gp ->
                    Zindex.BST801T.records ()
                    |> Array.filter (fun atc ->
                        atc.ATCODE
                        |> String.trim
                        |> String.equalsCapInsens (gp.ATC |> String.subString 0 5)
                    )
                    |> Array.map (fun atc -> atc.ATOMS))
                |> Array.distinct

            let unt =
                gps
                |> Array.fold (fun acc gp ->
                    if acc <> "" then acc
                    else
                        gp.PrescriptionProducts
                        |> Array.fold (fun acc pp ->
                            if acc <> "" then acc
                            else  pp.Unit
                        ) ""
                ) ""

            let rt =
                gps
                |> Array.collect (fun gp ->
                    gp.Route
                )
                |> Array.distinct

            create nm sh rt ph gps dpn unt [||])


    let private _get (prs : ProductRange.ProductRange []) =
        if FilePath.productCache |> File.exists then
            FilePath.productCache
            |> Json.getCache
            |> (fun gpps ->
                if prs |> Array.isEmpty then gpps
                else
                    gpps
                    |> Array.filter (fun gpp ->
                        gpp.GenericProducts
                        |> Array.exists (fun gp ->
                            prs
                            |> Array.exists (fun pr -> pr.GPK |> Option.get = gp.Id)
                        )
                    )
            )
        else
            printfn "No cache creating GenPresProduct"
            let gsps = parse prs
            gsps |> Json.cache FilePath.productCache
            gsps


    let private memGet = Memoization.memoize _get


    let private getAssortment () =
        let gpks =
            ProductRange.data ()
            |> Array.filter (fun pr -> pr.GPK |> Option.isSome)
            |> Array.map (fun pr -> pr.GPK |> Option.get)

        Array.empty
        |> memGet
        |> Array.filter (fun pr ->
            gpks
            |> Array.exists (fun gpk -> pr.GenericProducts |> Array.exists (fun gp -> gp.Id = gpk))
        )


    let private getAll () =
        Array.empty |> memGet


    let get all = if all then getAll () else getAssortment ()


    let getGPKS all =
        get all
        |> Array.collect (fun gpp ->
            gpp.GenericProducts
            |> Array.map (fun gp -> gp.Id)
        )
        |> Array.distinct


    let toString (gpp : GenPresProduct) =
        gpp.Name + " " + gpp.Shape + " " + (gpp.Route |> String.concat "/")


    let filter all n s r =
        if all then getAll () else getAssortment ()
        |> Array.filter (fun gpp ->
            (n = "" || gpp.Name  |> String.equalsCapInsens n) &&
            (s = "" || gpp.Shape |> String.equalsCapInsens s) &&
            (r = "" || gpp.Route |> Array.exists (fun r' -> r' |> String.equalsCapInsens r))
        )


    let findByGPK gpk =
        getAll ()
        |> Array.filter (fun gpp ->
            gpp.GenericProducts
            |> Array.exists (fun gp -> gp.Id = gpk)
        )


    let load all =
        if all then getAll () else getAssortment ()
        |> ignore


    let getRoutes =
        fun () ->
            getAssortment ()
            |> Array.collect (fun gpp ->
                gpp.Route
            )
            |> Array.distinct
            |> Array.sort
        |> Memoization.memoize


    let getShapes =
        fun () ->
            getAssortment ()
            |> Array.map (fun gpp ->
                gpp.Shape
            )
            |> Array.distinct
            |> Array.sort
        |> Memoization.memoize


    let getUnits =
        fun () ->
            getAssortment ()
            |> Array.map (fun gpp ->
                gpp.Unit
            )
            |> Array.distinct
            |> Array.sort
        |> Memoization.memoize


    let getShapeRoutes =
        fun () ->
            getAssortment ()
            |> Array.map (fun gpp ->
                gpp.Shape, gpp.Route
            )
            |> Array.groupBy fst
            |> Array.map (fun (k, vs) ->
                k,
                vs
                |> Array.collect snd
                |> Array.distinct
            )
            |> Array.distinct
            |> Array.sort


    let getShapeUnits =
        fun () ->
            getAssortment ()
            |> Array.map (fun gpp ->
                gpp.Shape, gpp.Unit
            )
            |> Array.distinct
            |> Array.sort
        |> Memoization.memoize


    let getSubstanceUnits =
        fun () ->
            getAssortment ()
            |> Array.collect (fun gpp ->
                gpp.GenericProducts
                |> Array.collect (fun gp ->
                    gp.Substances
                    |> Array.map (fun s -> s.SubstanceUnit)
                )
            )
            |> Array.distinct
            |> Array.sort
        |> Memoization.memoize



    let getGenericUnits =
        fun () ->
            getAssortment ()
            |> Array.collect (fun gpp ->
                gpp.GenericProducts
                |> Array.collect (fun gp ->
                    gp.Substances
                    |> Array.map (fun s -> s.GenericUnit)
                )
            )
            |> Array.distinct
            |> Array.sort
        |> Memoization.memoize


    let getSubstQtyUnit gpk =
        getAll ()
        |> Array.collect (fun gpp ->
            gpp.GenericProducts
            |> Array.filter (fun gp -> gp.Id = gpk)
            |> Array.collect (fun gp ->
                gp.Substances
                |> Array.map (fun s ->
                    s.SubstanceName, s.SubstanceQuantity, s.SubstanceUnit
                )
            )
        )

(*
    let getPediatric () =
        getAssortment ()
        |> Array.filter (fun gpp ->
            FormularyParser.WebSiteParser.getFormulary ()
            |> Array.exists (fun d ->
                gpp.GenericProducts
                |> Array.exists (fun gp ->
                    let gpAtc, dAtc = gp.ATC.Trim(), d.Atc.Trim()
                    let gpn, dn =
                        gpp.Name |> String.toLower |> String.trim,
                        d.Generic |> String.toLower |> String.trim
                    gpAtc = dAtc ||
                    (gpAtc |> String.startsWith dAtc &&
                     (dn |> String.contains gpn || (gpn |> String.contains dn)))
                )
            )
        )
*)