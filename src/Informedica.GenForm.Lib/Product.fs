namespace Informedica.GenForm.Lib



module Product =


    open MathNet.Numerics
    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL
    open Informedica.ZIndex.Lib


    module Location =


        let toString = function
            | PVL -> "PVL"
            | CVL -> "CVL"
            | AnyLocation -> ""


        let fromString s =
            match s with
            | _ when s |> String.equalsCapInsens "PVL" -> PVL
            | _ when s |> String.equalsCapInsens "CVL" -> CVL
            | _ -> AnyLocation



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
    // Route
    // DoseType
    // Dep
    // CVL
    // PVL
    // DiluentVol
    // ExpansionVol
    // Diluents
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
                    Location =
                        match get "CVL", get "PVL" with
                        | s1, _ when s1 |> String.isNullOrWhiteSpace |> not -> CVL
                        | _, s2 when s2 |> String.isNullOrWhiteSpace |> not -> PVL
                        | _ -> AnyLocation
                    DoseType = get "DoseType" |> DoseType.fromString
                    Dep = get "Dep"
                    DiluentVol = get "DiluentVol" |> toBrOpt
                    ExpansionVol = get "ExpansionVol" |> toBrOpt
                    Diluents = get "Diluents"
                |}
            )


    let parenteral () =
        Web.getDataFromSheet Web.dataUrlId2 "ParentMeds"
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
                    Name = get "Name"
                    Substances =
                        [|
                            "volume mL", get "volume mL" |> toBrOpt
                            "energie kCal", get "energie kCal" |> toBrOpt
                            "eiwit g", get "eiwit g" |> toBrOpt
                            "KH g", get "KH g" |> toBrOpt
                            "vet g", get "vet g" |> toBrOpt
                            "Na mmol", get "Na mmol" |> toBrOpt
                            "K mmol", get "K mmol" |> toBrOpt
                            "Ca mmol", get "Ca mmol" |> toBrOpt
                            "P mmol", get "P mmol" |> toBrOpt
                            "Mg mmol", get "Mg mmol" |> toBrOpt
                            "Fe mmol", get "Fe mmol" |> toBrOpt
                            "VitD IE", get "VitD IE" |> toBrOpt
                            "Cl mmol", get "Cl mmol" |> toBrOpt

                        |]
                    Oplosmiddel = get "volume mL" 
                    Verdunner = get "volume mL" 
                |}
            )
            |> Array.map (fun r ->
                {
                    GPK =  r.Name
                    ATC = ""
                    MainGroup = ""
                    SubGroup = ""
                    Generic = r.Name
                    TallMan = "" //r.TallMan
                    Synonyms = [||]
                    Product = r.Name
                    Label = r.Name
                    Shape = ""
                    ShapeQuantity = Some 1N
                    ShapeUnit = "mL"
                    Reconstitution = [||]
                    Divisible = 10N |> Some
                    Substances =
                        r.Substances
                        |> Array.map (fun (s, q) ->
                            let n, u =
                                match s |> String.split " " with
                                | [n; u] -> n |> String.trim, u |> String.trim
                                | _ -> failwith $"cannot parse substance {s}"
                            {
                                Name = n
                                Quantity = q
                                Unit = u
                                MultipleQuantity = None
                                MultipleUnit = ""
                            }
                        )
                }
            )


    let getParenteral : unit -> Product [] =
        Memoization.memoize parenteral


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
                    Apotheek = get "UMCU"
                    ICC = get "ICC"
                    NEO = get "NEO"
                    ICK = get "ICK"
                    HCK = get "HCK"                
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
                            r.DiluentVol |> Option.isSome
                        )
                        |> Array.map (fun r ->
                            {
                                Route = r.Route
                                DoseType = r.DoseType
                                Department = r.Dep
                                Location = r.Location
                                DiluentVolume = r.DiluentVol.Value
                                ExpansionVolume = r.ExpansionVol
                                Diluents =
                                    r.Diluents
                                    |> String.splitAt ';'
                                    |> Array.map String.trim
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


    let getProducts : unit -> Product [] =
        Memoization.memoize products


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


