namespace Informedica.GenForm.Lib



module Product =

    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL
    open Informedica.ZIndex.Lib


    let toBrs s =
        s
        |> String.splitAt ';'
        |> Array.choose Double.tryParse
        |> Array.choose BigRational.fromFloat


    let toBrOpt brs = brs |> Array.tryHead


    let tupleBrOpt brs1 brs2 =
        brs1 |> Array.tryHead,
        brs2 |> Array.tryHead


    let filter generic shape (prods : Product array) =
        let repl s =
            s
            |> String.replace "/" ""
            |> String.replace "+" ""
        let eqs s1 s2 =
            let s1 = s1 |> repl
            let s2 = s2 |> repl
            s1 |> String.equalsCapInsens s2
        prods
        |> Array.filter (fun p -> p.Generic |> eqs generic && p.Shape |> eqs shape)


    // GPK
    // Generic
    // Shape
    // Route
    // DoseType
    // Dep
    // CVL
    // PVL
    // DiluentVol
    // Diluent
    let getReconstitution () =
        Web.getDataFromSheet Web.dataUrlId2 "Reconstitution"
        |> fun data ->

            let getColumn =
                data
                |> Array.head
                |> Csv.getStringColumn

            data
            |> Array.tail
            |> Array.map (fun r ->
                let get = getColumn r
                let toBrOpt = toBrs >> toBrOpt

                {|
                    GPK = get "GPK"
                    Route = get "Route"
                    DoseType = get "DoseType"
                    Dep = get "Dep"
                    DilentVol = get "DiluentVol" |> toBrOpt
                    Diluent = get "Diluent"
                |}
            )


    let products () =
        let reconst = getReconstitution ()

        Web.getDataFromSheet Web.dataUrlId2 "Formulary"
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
                    GPKODE = get "GPKODE" |> Int32.parse
                    Apotheek = get "Apotheek"
                    ICC = get "ICC"
                    NICU = get "NICU"
                    PICU = get "PICU"
                    Leeuw = get "Leeuw"                
                |}
            )
            |> Array.collect (fun r ->
                r.GPKODE
                |> GenPresProduct.findByGPK
            )
            |> Array.collect (fun gpp ->
                gpp.GenericProducts
                |> Array.map (fun gp -> gpp, gp)
            )
            |> Array.map (fun (gpp, gp) ->
                let atc =
                    gp.ATC
                    |> ATCGroup.findByATC5 ()
                {
                    GPK =  $"{gp.Id}"
                    ATC = gp.ATC |> String.trim
                    MainGroup =
                        atc
                        |> Array.map (fun g -> g.AnatomicalGroup)
                        |> Array.tryHead
                        |> Option.defaultValue ""
                    SubGroup =
                        atc
                        |> Array.map (fun g -> g.TherapeuticSubGroup)
                        |> Array.tryHead
                        |> Option.defaultValue ""
                    Generic = gpp.Name |> String.toLower
                    TallMan = "" //r.TallMan
                    Synonyms =
                        gpp.GenericProducts
                        |> Array.collect (fun gp ->
                            gp.PrescriptionProducts
                            |> Array.collect (fun pp ->
                                pp.TradeProducts
                                |> Array.map (fun tp -> tp.Brand)
                            )
                        )
                    Product =
                        gp.PrescriptionProducts
                        |> Array.collect (fun pp ->
                            pp.TradeProducts
                            |> Array.map (fun tp -> tp.Label)
                        )
                        |> Array.distinct
                        |> function
                        | [| p |] -> p
                        | _ -> ""
                    Label = gp.Label
                    Shape = gp.Shape |> String.toLower
                    ShapeQuantity = Some 1N
                    ShapeUnit =
                        gp.Substances[0].ShapeUnit
                        |> Mapping.mapUnit
                        |> Option.defaultValue ""
                    Reconstitution =
                        reconst
                        |> Array.filter (fun r ->
                            r.GPK = $"{gp.Id}" &&
                            r.DilentVol |> Option.isSome
                        )
                        |> Array.map (fun r ->
                            {
                                Route = r.Route
                                Volume = r.DilentVol.Value
                                Diluents = [| r.Diluent |]
                            }
                        )
                        
                    Divisible = None
                    Substances =
                        gp.Substances
                        |> Array.map (fun s ->
                            {
                                Name =
                                    s.SubstanceName
                                    |> String.toLower
                                Quantity =
                                    s.SubstanceQuantity
                                    |> BigRational.fromFloat
                                Unit =
                                    s.SubstanceUnit
                                    |> Mapping.mapUnit
                                    |> Option.defaultValue ""
                                MultipleQuantity = None
                                MultipleUnit = ""
                            }
                        )
                }
            )


    let generics (products : Product array) =
        products
        |> Array.map (fun p ->
            p.Generic
        )
        |> Array.distinct


    let synonyms (products : Product array) =
        products
        |> Array.collect (fun p ->
            p.Synonyms
        )
        |> Array.append (generics products)
        |> Array.distinct


    let shapes  (products : Product array) =
        products
        |> Array.map (fun p -> p.Shape)
        |> Array.distinct


