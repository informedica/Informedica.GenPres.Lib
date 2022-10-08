namespace Informedica.GenProduct.Lib

module RuleFinder =

    open Informedica.Utils.Lib.BCL

    // ARTI/LESION
    // * AURICULAIR
    // * CUTAAN
    // DENTAAL
    // ENDOCERVIC
    // * ENDOTR.PULM
    // * ENDOTRACHEOPULMONAIR
    // * EPIDURAAL
    // EPIDURAAL, INTRATHECAAL, PERINEURAAL
    // EXTRACORPORAAL
    // * GASTR-ENTER
    // * IM
    // * INHALATIE
    // INTRA-ART.
    // INTRA-ARTERIEEL
    // INTRA-ARTICULAIR
    // INTRA-OCUL.
    // INTRA-UTERIEN
    // INTRABURSAAL
    // INTRACARDIAAL
    // INTRACAVERNEUS
    // INTRACORONAIR
    // * INTRADERMAAL
    // INTRALAESIONAAL
    // INTRALYMFATISCH
    // * INTRAMUSCULAIR
    // INTRAMUSCULAIR, INTRAVENEUS
    // INTRAMUSCULAIR, SUBCUTAAN
    // INTRAOSSAAL
    // INTRAPERITONEAAL
    // INTRAPLEURAAL
    // INTRATHECAAL
    // * INTRAVENEUS
    // INTRAVENEUS, SUBCUTAAN
    // * INTRAVESIC.
    // * INTRAVESICAAL
    // INTRAVITR.
    // INTRAVITREAAL
    // * IV
    // * LOKAAL
    // * NASAAL
    // * NEUS
    // NIET GESPEC
    // NVT
    // OOG
    // * OOR
    // * ORAAL
    // ORAAL/RECT
    // * OROMUCOSAAL
    // PAR./ORAAL
    // PAR/UTERIEN
    // PAR/VESICAL
    // PARABULBAIR
    // PARENT/RECT
    // PARENTERAAL
    // PERI-ARTICULAIR
    // PERIBULBAIR
    // PERINEURAAL
    // PERITONEAAL
    // * RECTAAL
    // RETROBULBAIR
    // SUBCONJUNCTIVAAL
    // * SUBCUTAAN
    // SUBLINGUAAL
    // * TRANSDERMAAL
    // TRANSDERML
    // URETHRAAL
    // UTERIEN
    // VAGINAAL

    type Route = 
        | AUR // AURICULAIR OOR
        | CUT // CUTAAN TRANSDERMAAL TRANSDERML LOKAAL
        | ENDOTR // ENDOTR.PULM ENDOTRACHEOPULMONAIR
        | EPIDUR // EPIDURAAL
        | IM // INTRAMUSCULAIR IM
        | INH // INHALATIE
        | INTRAVESIC // INTRAVESIC. INTRAVESICAAL
        | IV // INTRAVENEUS IV
        | NASAL // NASAAL NEUS
        | ORAL // ORAAL GASTR-ENTER OROMUCOSAAL
        | OROMUCOSAL //OROMUCOSAAL
        | RECTAL // RECTAAL
        | SUBCUT // INTRADERMAAL SUBCUTAAN
        | NoRoute

    let routeMapping =
        [ 
            (AUR, ["AURICULAIR"; "OOR"])
            (CUT, [ "CUTAAN"; "TRANSDERMAAL"; "TRANSDERML"; "LOKAAL"])
            (ENDOTR, ["ENDOTR.PULM"; "ENDOTRACHEOPULMONAIR"])
            (EPIDUR, ["EPIDURAAL"])
            (IM, ["INTRAMUSCULAIR"; "IM"])
            (INH, ["INHALATIE"; "INH"])
            (INTRAVESIC, ["INTRAVESIC."; "INTRAVESICAAL"])
            (IV, ["INTRAVENEUS"; "IV"])
            (NASAL, ["NASAAL"; "NEUS"])
            (ORAL, ["ORAAL"; "GASTR-ENTER"; "OR"])
            (OROMUCOSAL, ["OROMUCOSAAL"])
            (RECTAL, ["RECTAAL"; "RECT"])
            (SUBCUT, ["SUBCUTAAN"; "SC"])
        ]

    let createRoute s = 
        let m = 
            routeMapping 
            |> List.tryFind (fun (_, rs) -> 
                rs 
                |> List.exists (String.equalsCapInsens s)
            )
        match m with
        | Some (r, _) -> r
        | _ -> NoRoute

    let eqsRoute r rs = 
        if r = NoRoute then true
        else
            rs 
            |> Array.map createRoute
            |> Array.exists ((=) r)

    type AgeInMo = float Option

    type WeightInKg = float Option
    
    type BSAInM2 = float Option

    let inRange n { DoseRule.Min = min; DoseRule.Max = max } =
        if n |> Option.isNone then true
        else
            let n = n |> Option.get
            match min, max with
            | None, None -> true
            | Some min, None -> n >= min
            | None, Some max -> n <= max
            | Some min, Some max -> n >= min && n <= max

    type PatientFilter =
        {
            Age: AgeInMo
            Weight: WeightInKg
            BSA: BSAInM2
        }

    type ProductFilter =
        | GPKRoute of (int * string)
        | GenericShapeRoute of GenericShapeRoute
    and GenericShapeRoute = 
        {
            Generic: string
            Shape: string
            Route: string
        }
    

    type Filter =
        {
            Patient: PatientFilter
            Product: ProductFilter
        }

    let createFilter age wght bsa gpk gen shp rte =
        let pat = { Age = age; Weight = wght; BSA = bsa }
        let prod = 
            if gpk |> Option.isSome then
                (gpk |> Option.get, rte)
                |> GPKRoute
            else
                {
                    Generic = gen
                    Shape = shp
                    Route = rte
                } |> GenericShapeRoute
        {
            Patient = pat
            Product = prod
        }

    let createGPKRouteFilter gpk rte = createFilter None None None gpk "" "" rte

    let find all { Patient = pat; Product = prod } =
        let r = 
            match prod with
            | GPKRoute (_, route)   -> route
            | GenericShapeRoute gsr -> gsr.Route 
            |> createRoute 

        match prod with
        | GPKRoute (gpk, _) -> [| gpk |]
        | GenericShapeRoute gsr ->
            GenPresProduct.filter all gsr.Generic gsr.Shape gsr.Route
            |> Array.collect (fun gpp -> 
                gpp.GenericProducts 
                |> Array.map (fun gp -> gp.Id)
            )
        |> Array.collect (fun gpk ->
            DoseRule.get ()
            |> Array.filter (fun dr -> 
                (dr.CareGroup = DoseRule.Constants.intensive || dr.CareGroup = DoseRule.Constants.all)
                && dr.GenericProduct |> Array.exists (fun gp -> gp.Id = gpk)
                && dr.Routes |> eqsRoute r
                && dr.Age    |> inRange pat.Age
                && dr.Weight |> inRange pat.Weight
                && dr.BSA    |> inRange pat.BSA
            )
            |> Array.distinct
        )

    // stuk
    // ml
    // dosis
    // g
    // mg
    // ug
    // milj. IE
    // IE
    // E
    type RuleResult =
        {
            Product: GenPresProduct.GenPresProduct
            DoseRules: string []
            Doses: FreqDose []
        }
    and FreqDose =
        {
            /// The frequency of the dose rule
            Freq: DoseRule.Frequency
            /// The optional min/max values of a 'normal dose range'
            NormDose: DoseRule.MinMax
            /// The optional min/max values of the 'absolute dose range'
            AbsDose: DoseRule.MinMax
            /// The optional min/max values of a 'normal dose range' per kg
            NormKg: DoseRule.MinMax
            /// The optional min/max values of the 'absolute dose range' per kg
            AbsKg: DoseRule.MinMax
            /// The optional min/max values of a 'normal dose range' per m2
            NormM2: DoseRule.MinMax
            /// The optional min/max values of the 'absolute dose range' per m2
            AbsM2: DoseRule.MinMax
            /// The unit in which the doses are measured
            Unit: string
        }

    let createResult gpp drs ds =
        {
            Product = gpp
            DoseRules = drs
            Doses = ds
        }

    let createFreqDose freq norm abs normKg absKg normM2 absM2 un =
        {
            Freq = freq
            NormDose = norm
            AbsDose = abs
            NormKg = normKg
            AbsKg = absKg
            NormM2 = normM2
            AbsM2 = absM2
            Unit = un
        }
     

    let convertToResult (drs : DoseRule.DoseRule  []) =

        // Get the min max weight if there is one min weight or max weight
        let wghtMinMax (drs : DoseRule.DoseRule []) =

            match drs |> Array.toList with
            | [] -> DoseRule.minmax
            | [h] -> h.Weight
            | h::tail ->
                if tail |> List.forall (fun mm -> mm.Weight = h.Weight) then h.Weight
                else DoseRule.minmax

        // Get the min max weight if there is one min weight or max weight
        let bsaMinMax (drs : DoseRule.DoseRule []) =

            match drs |> Array.toList with
            | [] -> DoseRule.minmax
            | [h] -> h.BSA
            | h::tail ->
                if tail |> List.forall (fun mm -> mm.BSA = h.Weight) then h.BSA
                else DoseRule.minmax

        // Alle dose rules should apply to the same 
        // GenPresProduct
        let gpp =
            drs
            |> Array.collect (fun dr ->
                dr.GenericProduct
                |> Array.map (fun gp -> gp.Id)
            )
            |> Array.distinct
            |> Array.collect GenPresProduct.findByGPK 
            |> (fun gpps ->
                if gpps |> Array.isEmpty then None
                else
                    gpps
                    |> Array.fold (fun acc gpp -> 
                        match acc with
                        | Some gpp' -> if gpp' = gpp then acc else None
                        | None -> None
                    ) (Some gpps.[0])
            ) 

        match gpp with
        | Some gpp' ->
            let multMinMax f n { DoseRule.Min = min; DoseRule.Max = max } =
                let m = f * n

                let mn, mx = 
                    match min, max with
                    | None, None           -> (0., 0.)
                    | Some min', None      -> (min' * m, 0.)
                    | None, Some max'      -> (0., max' * m )
                    | Some min', Some max' -> (min' * m, max' * m)

                DoseRule.createMinMax mn mx

            let gpks (dr : DoseRule.DoseRule) = 
                dr.GenericProduct 
                |> Array.map (fun gp -> gp.Id) 
                |> Array.toList
                |> GenericProduct.get


            // Calculate the normal min max dose
            let calcDose get drs = 
                drs
                |> Array.collect (fun dr -> 
                    dr
                    |> gpks
                    |> Array.map (fun gp ->
                        let n = 
                            (gp.Substances
                            |> Array.head).SubstanceQuantity
                        dr |> get |> multMinMax dr.Freq.Frequency n)
                    )
                |> DoseRule.foldMinMax


            // Calculate the normal min max dose
            let norm = calcDose DoseRule.Optics.getNorm

            // Calculate the absolute min max dose
            let abs = calcDose DoseRule.Optics.getAbs

            // Calculate the normal min max dose per kg
            let normKg = calcDose DoseRule.Optics.getNormKg 

            // Calculate the absolute min max dose per kg
            let absKg = calcDose DoseRule.Optics.getAbsKg 

            // Calculate the normal min max dose per m2
            let normM2 = calcDose DoseRule.Optics.getNormM2 

            // Calculate the absolute min max dose per m2
            let absM2 = calcDose DoseRule.Optics.getAbsM2 

            let calcNoneAndAdjusted 
                (calcAdj   : DoseRule.DoseRule [] -> DoseRule.MinMax) 
                (calcNorm  : DoseRule.DoseRule [] -> DoseRule.MinMax)
                (calcPerKg : DoseRule.DoseRule [] -> DoseRule.MinMax) drs =

                let wght  = drs |> calcAdj
                let norm  = drs |> calcNorm
                let perKg = drs |> calcPerKg

                let calc op x1 x2 y =
                    match y with
                    | Some _ -> y
                    | None -> 
                        match x1, x2 with
                        | Some x1_, Some x2_ -> (x1_ |> op <| x2_) |> Some
                        | _ -> y

                // Norm.min = PerKg.min * Wght.min
                // Norm.max = PerKg.max * Wght.max
                { norm with 
                    Min = norm.Min |> calc (*) perKg.Min wght.Min
                    Max = norm.Max |> calc (*) perKg.Max wght.Max } ,
                // PerKg.min = Norm.min / Wght.max
                // PerKg.max = norm.max / Wght.min
                { perKg with
                    Min = perKg.Min |> calc (/) norm.Min wght.Max
                    Max = perKg.Max |> calc (/) norm.Max wght.Min }

            let calcNormPerKg = calcNoneAndAdjusted wghtMinMax norm normKg
            let calcAbsPerKg  = calcNoneAndAdjusted wghtMinMax abs  absKg
            let calcNormPerM2 = calcNoneAndAdjusted bsaMinMax  norm normM2
            let calcAbsPerM2  = calcNoneAndAdjusted bsaMinMax  abs  absM2

            let un drs' =
                drs'
                |> Array.fold (fun acc dr ->
                    dr
                    |> gpks
                    |> Array.fold (fun acc' gp ->
                        gp.Substances
                        |> Array.fold (fun acc'' s ->
                            if acc'' = "" then s.SubstanceUnit
                            else
                                if acc'' <> s.SubstanceUnit then "_"
                                else s.SubstanceUnit
                        ) acc'
                    ) acc
                ) ""
                |> (fun u -> if u = "_" then "" else u)

            let freqs =
                drs
                |> Array.map (fun dr -> dr.Freq)
                |> Array.distinct
                |> Array.map (fun fr ->
                    let drs' =
                        drs
                        |> Array.filter (fun dr -> dr.Freq = fr)
                    createFreqDose 
                        fr 
                        ([| drs' |> calcNormPerKg |> fst; drs' |> calcNormPerM2 |> fst |] |> DoseRule.foldMinMax ) 
                        ([| drs' |> calcAbsPerKg  |> fst; drs' |> calcAbsPerM2  |> fst |] |> DoseRule.foldMinMax ) 
                        (drs' |> calcNormPerKg |> snd) 
                        (drs' |> calcAbsPerKg  |> snd) 
                        (drs' |> calcNormPerM2 |> snd) 
                        (drs' |> calcAbsPerM2  |> snd) 
                        (drs' |> un)
                )

            createResult gpp' (drs |> Array.map (DoseRule.toString ", ")) freqs 
            |> Some
        
        | None -> None

