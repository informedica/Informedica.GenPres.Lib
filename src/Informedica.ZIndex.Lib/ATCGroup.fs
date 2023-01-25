namespace Informedica.ZIndex.Lib


module ATCGroup =

    open Informedica.Utils.Lib.BCL
    open Informedica.Utils.Lib


    let create atc1 ang ange atc2 thg thge atc3 ths thse atc4 phg phge atc5 sub sube gen shp rts =
        {
            ATC1 = atc1
            AnatomicalGroup = ang
            AnatomicalGroupEng = ange
            ATC2 = atc2
            TherapeuticMainGroup = thg
            TherapeuticMainGroupEng = thge
            ATC3 = atc3
            TherapeuticSubGroup = ths
            TherapeuticSubGroupEng = thse
            ATC4 = atc4
            PharmacologicalGroup = phg
            PharmacologicalGroupEng = phge
            ATC5 = atc5
            Substance = sub
            SubstanceEng = sube
            Generic = gen
            Shape = shp
            Routes = rts
        }


    let empty = create "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""


    let parse gpks =
        query {
            for gpk in Zindex.BST711T.records () do
            join main in Zindex.BST801T.records ()
                on (gpk.ATCODE.Substring(0, 1) = main.ATCODE.Trim())
            join ther in Zindex.BST801T.records ()
                on (gpk.ATCODE.Substring(0, 3) = ther.ATCODE.Trim())
            join thes in Zindex.BST801T.records ()
                on (gpk.ATCODE.Substring(0, 4) = thes.ATCODE.Trim())
            join phar in Zindex.BST801T.records ()
                on (gpk.ATCODE.Substring(0, 5) = phar.ATCODE.Trim())
            join subs in Zindex.BST801T.records ()
                on (gpk.ATCODE.Substring(0, 7) = subs.ATCODE.Trim())

            let shape = Names.getThes gpk.GPKTVR Names.Shape Names.Fifty

            let generic =
                query {
                    for spk in
                        Zindex.BST720T.records ()
                        |> Array.filter (fun r -> r.SPKODE = gpk.SPKODE) do
                    join ssk in Zindex.BST725T.records ()
                        on (spk.SSKODE = ssk.SSKODE)
                    join gngnk in Zindex.BST750T.records ()
                        on (ssk.GNSTAM = gngnk.GNGNK)
                    select gngnk.GNGNAM
                }
                |> Seq.toArray
                |> Seq.fold (fun a s ->
                    if a = "" then s
                    else a + "/" + s) ""

            let route =

                let rt =
                    Zindex.BST051T.records ()
                    |> Array.filter (fun r ->
                        r.MUTKOD <> 1 &&
                        r.GPKODE = gpk.GPKODE
                    )
                    |> Array.collect (fun r ->
                        Zindex.BST031T.records ()
                        |> Array.filter (fun r' ->
                            r'.MUTKOD <> 1 &&
                            r'.PRKODE = r.PRKODE
                        )
                    )
                    |> Array.collect (fun r ->
                        Zindex.BST760T.records ()
                        |> Array.filter (fun r' ->
                            r'.MUTKOD <> 1 &&
                            r'.HPKODE = r.HPKODE
                        )
                        |> Array.map(fun r' -> r'.ENKTDW)
                    )
                    |> Array.distinct
                    |> Array.map (fun tdw ->
                        Names.getThes tdw Names.Route Names.Fifty
                    )
                    |> Array.fold (fun a s ->
                            if a = "" then s
                            else a + "," + s
                        ) ""

                if rt <> "" then rt
                else
                    match
                        DoseRule.getGenericProducts ()
                        |> Array.tryFind (fun r -> r.Id = gpk.GPKODE) with
                    | Some p ->
                        if p.Route |> Array.isEmpty || p.Route |> Array.length > 1 then ""
                        else p.Route.[0]
                    | None -> ""


            where (gpk.MUTKOD  <> 1 &&
                   main.MUTKOD <> 1 &&
                   ther.MUTKOD <> 1 &&
                   thes.MUTKOD <> 1 &&
                   phar.MUTKOD <> 1 &&
                   subs.MUTKOD <> 1 &&
                   gpks |> Array.exists ((=) gpk.GPKODE))

            select
                ({
                    empty with
                        ATC1 = main.ATCODE.Trim()
                        AnatomicalGroup = main.ATOMS.Trim()
                        AnatomicalGroupEng = main.ATOMSE.Trim()
                        ATC2 = ther.ATCODE.Trim()
                        TherapeuticMainGroup = ther.ATOMS.Trim()
                        TherapeuticMainGroupEng = ther.ATOMSE.Trim()
                        ATC3 = thes.ATCODE.Trim()
                        TherapeuticSubGroup = thes.ATOMS.Trim()
                        TherapeuticSubGroupEng = thes.ATOMSE.Trim()
                        ATC4 = phar.ATCODE.Trim()
                        PharmacologicalGroup = phar.ATOMS.Trim()
                        PharmacologicalGroupEng = phar.ATOMSE.Trim()
                        ATC5 = subs.ATCODE.Trim()
                        Substance = subs.ATOMS.Trim()
                        SubstanceEng = subs.ATOMSE.Trim()
                        Generic = generic
                        Shape = shape
                        Routes = route
                })
        }
        |> Seq.toArray
        |> Array.distinct


    let _get () =
        if FilePath.groupCache |> File.exists then
            FilePath.groupCache
            |> Json.getCache
        else
            printfn "No cache creating group.cache"
            let grps = GenPresProduct.getGPKS true |> parse
            grps |> Json.cache FilePath.groupCache
            grps


    let get : unit -> ATCGroup [] = Memoization.memoize _get


    let findByATC5 all atc =
        get all
        |> Array.filter (fun g ->
            g.ATC5 |> String.equalsCapInsens atc
        )


    let load () = get () |> ignore

