namespace Informedica.ZIndex.Lib


module GenericProduct =

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL

    type GenericProduct =
        {
            Id : int
            Name : string
            Label : string
            ATC : string
            ATCName : string
            Shape : string
            Route : string []
            Substances : Substance []
            PrescriptionProducts : PrescriptionProduct.PrescriptionProduct []
        }
    and Substance =
        {
            SubstanceId : int
            SubstanceName : string
            SubstanceQuantity : float
            SubstanceUnit : string
            GenericId : int
            GenericName : string
            GenericQuantity : float
            GenericUnit : string
            ShapeUnit : string
        }

    let createSubstance si sn sq su gi gn gq gu un =
        {
            SubstanceId = si
            SubstanceName = sn
            SubstanceQuantity = sq
            SubstanceUnit = su
            GenericId = gi
            GenericName = gn
            GenericQuantity = gq
            GenericUnit = gu
            ShapeUnit = un
        }

    let create id nm lb ac an sh rt ss ps =
        {
            Id = id
            Name = nm
            Label = lb
            ATC = ac
            ATCName = an
            Shape = sh
            Route = rt
            Substances = ss
            PrescriptionProducts = ps
        }

    let getRoutes (gp : Zindex.BST711T.BST711T) =
        // try to get the 'enkelvoudige toedieningsweg'
        let rt =
            Zindex.BST051T.records ()
            |> Array.filter (fun r ->
                r.MUTKOD <> 1 &&
                r.GPKODE = gp.GPKODE
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
            |> Array.filter String.notEmpty

        if rt |> Array.isEmpty |> not then rt
        else
            [| Names.getThes gp.GPKTWG Names.Route Names.Fifty |]


    let getSubstances log un (gp : Zindex.BST711T.BST711T) hpks =
        Zindex.BST715T.records ()
        |> Array.filter (fun gs ->
            gs.GSKODE = gp.GSKODE &&
            gs.MUTKOD <> 1 &&
            gs.GNMWHS = "W"
        )
        |> Array.collect (fun gs ->
            Zindex.BST750T.records ()
            |> Array.filter (fun gn ->
                gn.MUTKOD <> 1 && gs.GNNKPK = gn.GNGNK
            )
            |> Array.map (fun gn -> gs, gn)
        )
        |> Array.collect (fun (gs, gn) ->
            let stam =
                Zindex.BST750T.records ()
                |> Array.find (fun s -> s.GNGNK = gn.GNSTAM)

            match hpks with
            | _ when hpks |> Array.isEmpty ->
                let un1 = Names.getThes gs.XNMOME Names.GenericUnit Names.Fifty
                createSubstance gn.GNSTAM stam.GNGNAM gs.GNMOMH un1 gn.GNGNK gn.GNGNAM gs.GNMOMH un1 un
                |> Array.singleton
            | _  ->
                hpks
                |> Array.collect (fun hpk ->
                    Zindex.BST701T.records ()
                    |> Array.filter (fun ig ->
                        ig.HPKODE = hpk &&
                        ig.GNGNK = gn.GNGNK &&
                        ig.MUTKOD <> 1
                    )
                    |> function
                    | igs when igs |> Array.isEmpty ->
                        Zindex.BST701T.records ()
                        |> Array.filter (fun ig ->
                            ig.HPKODE = hpk &&
                            ig.GNSTAM = gn.GNSTAM &&
                            ig.MUTKOD <> 1
                        )
                    | igs -> igs
                    |> Array.map (fun ig ->
                        let un1 = Names.getThes ig.XNMINE Names.GenericUnit Names.Fifty
                        let un2 = Names.getThes gs.XNMOME Names.GenericUnit Names.Fifty
                        createSubstance ig.GNSTAM stam.GNGNAM ig.GNMINH un1 gn.GNGNK gn.GNGNAM gs.GNMOMH un2 un
                    )
                )
        )
        |> Array.distinct


    let private _get log gpks =
        Zindex.BST711T.records ()
        |> Array.filter (fun gp ->
            gp.MUTKOD <> 1 &&
            gp.GPKTVR <> 980 && // filter shape <> "NIET VAN TOEPASSING"
            (gpks |> List.isEmpty ||
             gpks
             |> List.exists ((=) gp.GPKODE)))
        |> Array.map (fun gp ->
            let nm = Names.getName gp.GPNMNR Names.Full
            let lb = Names.getName gp.GPNMNR Names.Label

            let an =
                match
                    Zindex.BST801T.records ()
                    |> Array.tryFind (fun atc ->
                        atc.MUTKOD <> 1 &&
                        atc.ATCODE = gp.ATCODE
                    ) with
                | Some atc' -> atc'.ATOMS
                | None      -> ""
            let sh = Names.getThes gp.GPKTVR Names.Shape Names.Fifty
            let rt = getRoutes gp
            let ps = PrescriptionProduct.get gp.GPKODE
            let un = Names.getThes gp.XPEHHV Names.ShapeUnit Names.Fifty

            let ss =
                ps
                |> Array.collect (fun pp ->
                    pp.TradeProducts
                    |> Array.map (fun tp -> tp.Id)
                )
                |> getSubstances log un gp
            printfn $"creating: {nm}"
            create gp.GPKODE nm lb gp.ATCODE an sh rt ss ps
        )

    let get : int list -> GenericProduct [] = Memoization.memoize (_get (fun _ -> ()))

    let getWithLog = _get
