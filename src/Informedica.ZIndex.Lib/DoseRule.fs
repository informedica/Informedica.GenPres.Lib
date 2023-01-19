namespace Informedica.ZIndex.Lib


module DoseRule =

    open System
    open System.Text

    open Aether

    open Informedica.Utils.Lib.BCL
    open Informedica.Utils.Lib


    module Constants =

        [<Literal>]
        let intensive = "intensieve"

        [<Literal>]
        let nonIntensive = "niet-intensieve"

        [<Literal>]
        let all = "alle"

        [<Literal>]
        let profylactic = "profylactisch"

        [<Literal>]
        let therapeutic = "therapeutisch"

        [<Literal>]
        let standard = "Standaard"

        [<Literal>]
        let special = "Verbyzondering"

        [<Literal>]
        let male = "man"

        [<Literal>]
        let female = "vrouw"


    type DoseRule =
        {
            /// The id of the doserule
            Id : int
            /// The caregroup the doserule applies to
            /// this is either 'intensieve' or 'niet-intensieve' or 'all'
            CareGroup : string
            /// This is the usage of the dose rule, can be therapeutic or
            /// profylactic
            Usage : string
            /// The dose type, 'standaard' means that the dose rule applies without
            /// a specific indication, 'verbyzondering' means the dose rule needs
            /// an indication other than 'Algemeen'.
            DoseType : string
            /// The list of generic products for which the dose rule applies
            GenericProduct : GenericProduct[]
            /// The list of prescription products for which the dose rule applies
            PrescriptionProduct : Product[]
            /// The list of trade products for which the dose rule applies
            TradeProduct : Product[]
            /// The route for which the dose rule applies
            Routes : string []
            /// The indication id for which the dose rule applies.
            /// The indications are coded by ICPC/ICD-10
            IndicationId : int
            /// The indication text for which the dose rule applies.
            /// The indications are coded by ICPC/ICD-10
            Indication : string
            /// If high risk, than the dose margins are smaller
            HighRisk : bool
            /// Gender is either 'man', 'vrouw' or an empty string.
            /// When gender is empty the dose rule can apply to either
            /// gender.
            Gender : string
            /// The optional minimum or maximum age limits for the dose rule
            Age : MinMax
            /// The optional minimum or maximum weight limits for which the dose
            /// rule applies
            Weight : MinMax
            /// The optional BSA min/max for which the dose rule applies
            BSA : MinMax
            /// The frequency of the dose rule. The total dose can be calculated
            /// by multiplying the dose by the frequency.
            Freq : Frequency
            /// The normal optional min/max of the unadjusted dose
            Norm : MinMax
            /// The absolute optional min/max of the unadjusted dose
            Abs : MinMax
            /// The normal optional min/max of the dose adjusted by weight
            NormKg : MinMax
            /// The absolute optional min/max of the dose adjusted by weight
            AbsKg : MinMax
            /// The absolute optional min/max of the dose adjusted by BSA
            NormM2 : MinMax
            /// The absolute optional min/max of the dose adjusted by BSA
            AbsM2 : MinMax
            /// The unit in which the dose is measured
            Unit : string
        }
    and Product = { Id: int; Name: string }
    and GenericProduct =
        {
            Id: int
            Name: string
            Route: string []
            Unit: string
            Substances : Substance []
        }
    and Substance = { Name: string; Quantity: decimal; Unit: string }
    and Frequency = { Frequency: decimal; Time: string }
    and MinMax = { Min: decimal Option; Max: decimal Option }


    let foldMinMax xs =
        xs |> Array.fold (fun { Min = min; Max = max} (acc: MinMax) ->
            { Min = Option.min acc.Min min; Max = Option.max acc.Max max }
        ) { Min = None; Max = None }


    let freqToString (freq : Frequency) =
        (string freq.Frequency) + " " + freq.Time


    let toString del (r: DoseRule)  =
        let minMaxToString n u p (mm: MinMax) s =
            let mms =
                match mm.Min, mm.Max with
                | Some min, Some max ->
                    let min = Decimal.fixPrecision p min |> string
                    let max = Decimal.fixPrecision p max |> string
                    sprintf "%s - %s" min max
                | Some min, None ->
                    let min = Decimal.fixPrecision p min |> string
                    sprintf "vanaf %s" min
                | None, Some max ->
                    if max = 0m then ""
                    else
                        let max = Decimal.fixPrecision p max |> string
                        sprintf "tot %s" max
                | None, None -> ""
            if mms = "" then s
            else
                s + n + ": " + mms + " " + u + del

        let adds s1 s2 s3 =
            if s2 |> String.IsNullOrWhiteSpace then s3
            else
                let s3 = if s1 = "" then s3 else s3 + s1 + ": "
                s3 + s2 + del

        let gp =
            r.GenericProduct |> Seq.fold(fun a gp ->
                let s' = if a |> String.IsNullOrWhiteSpace then "" else ", "
                s' + gp.Name) ""

        let pp =
            r.PrescriptionProduct |> Seq.fold(fun a gp ->
                let s' = if a |> String.IsNullOrWhiteSpace then "" else ", "
                s' + gp.Name) ""

        let tp =
            r.TradeProduct |> Seq.fold(fun a gp ->
                let s' = if a |> String.IsNullOrWhiteSpace then "" else ", "
                s' + gp.Name) ""

        let s = "" + (string r.Id) + del
        let s = s |> adds "" gp
        let s = s |> adds "" pp
        let s = s |> adds "" tp
        let s = s |> adds "Gebruik" r.Usage
        let s = s |> adds "Groep" r.CareGroup
        let s = s |> adds "Type" r.DoseType
        let s = s |> adds "Route" (r.Routes |> String.concat "/")
        let s = s |> adds "Indicatie" r.Indication

        let s = if r.HighRisk then s + "Hig Risk " else s

        let s = s |> adds "" r.Gender

        let s = s |> minMaxToString "Leeftijd" "maanden" 1 r.Age
        let s = s |> minMaxToString "Gewicht" "kg" 3 r.Weight
        let s = s |> minMaxToString "BSA" "m2" 3 r.BSA

        let s =
            if r.Freq.Frequency <= 0m then s
            else
                s + "Freq: " + (r.Freq |> freqToString) + " " + del

        let s = s |> minMaxToString "Norm" r.Unit 3 r.Norm
        let s = s |> minMaxToString "Norm/Kg" r.Unit 3 r.NormKg
        let s = s |> minMaxToString "Abs" r.Unit 3 r.Abs
        let s = s |> minMaxToString "Abs/Kg" r.Unit 3 r.AbsKg

        let s = s |> minMaxToString "Norm/m2" r.Unit 3 r.NormM2
        let s = s |> minMaxToString "Abs/m2" r.Unit 3 r.AbsM2

        let s = s |> String.subString 0 ((s |> String.length) - (del |> String.length))
        s

    let minmax = { Min = None; Max = None }

    let createProduct id nm : Product = { Id = id; Name = nm }

    let createGenericProduct id nm rt un sl = { Id = id; Name = nm; Route = rt; Unit = un; Substances = sl }

    let createFrequency fr tm = { Frequency = fr; Time = tm }

    let createMinMax mn mx =

        let chkmx =
            mx
            |> string
            |> String.forall (fun c -> c = '9' || c = '.')

        if mx < mn then minmax
        else
            let mn = if mn = 0m then None else Some mn
            let mx = if mx = 0m || chkmx then None else Some mx

            { Min = mn; Max = mx }

    let create id gr us dt gp pr tr rt ci ic hr sx ag wt bs fr no ab nk ak nm am un =
        {
            Id = id
            CareGroup = gr
            Usage = us
            DoseType = dt
            GenericProduct = gp
            PrescriptionProduct = pr
            TradeProduct = tr
            Routes = rt
            IndicationId = ci
            Indication = ic
            HighRisk = hr
            Gender = sx
            Age = ag
            Weight = wt
            BSA = bs
            Freq = fr
            Norm = no
            Abs = ab
            NormKg = nk
            AbsKg = ak
            NormM2 = nm
            AbsM2 = am
            Unit = un
        }


    let empty =
        {
            Id = 0
            CareGroup = ""
            Usage = ""
            DoseType = ""
            GenericProduct = Array.empty
            PrescriptionProduct = Array.empty
            TradeProduct = Array.empty
            Routes = [||]
            IndicationId = 0
            Indication = ""
            HighRisk = false
            Gender = ""
            Age = minmax
            Weight = minmax
            BSA = minmax
            Freq = createFrequency 0m ""
            Norm = minmax
            Abs = minmax
            NormKg = minmax
            AbsKg = minmax
            NormM2 = minmax
            AbsM2 = minmax
            Unit = ""
        }


    let _getGenericProducts () =
        GenPresProduct.get true
        |> Array.collect (fun gpp ->
            gpp.GenericProducts
            |> Array.map (fun gp -> gp.Id)
        )
        |> Array.distinct
        |> Array.toList
        |> GenericProduct.get
        |> Array.map (fun gp ->
            let unt =
                gp.Substances
                |> Array.fold (fun acc s ->
                    if acc = "" then s.ShapeUnit else acc
                ) ""
            {
                Id = gp.Id
                Name = gp.Name
                Route = gp.Route
                Unit = unt
                Substances =
                    gp.Substances
                    |> Array.map (fun s ->
                        {
                            Name = s.SubstanceName
                            Quantity = s.SubstanceQuantity
                            Unit = s.SubstanceUnit
                        }
                    )
            }
        )

    let getGenericProducts : unit -> GenericProduct[] =
        Memoization.memoize _getGenericProducts


    let _getPrescriptionProducts _ =
        query {
            for p in Zindex.BST050T.records () do
            join nm in Zindex.BST020T.records ()
                on (p.PRNMNR = nm.NMNR)
            where (p.MUTKOD <> 1)
            select
                (
                    createProduct p.PRKODE
                                  (nm.NMNAAM.Trim())
                )
        } |> Seq.toArray


    let getPresciptionProducts : unit -> Product[] =
        Memoization.memoize _getPrescriptionProducts


    let _getTradeProducts _ =
        query {
            for p in Zindex.BST031T.records () do
            join nm in Zindex.BST020T.records ()
                on (p.HPNAMN = nm.NMNR)
            where (p.MUTKOD <> 1)
            select
                (
                    createProduct p.HPKODE
                                  (nm.NMNAAM.Trim())
                )
        } |> Seq.toArray


    let getTradeProducts : unit -> Product[] =
        Memoization.memoize _getTradeProducts


    let getICPCRoute (icp : Zindex.BST642T.BST642T) =
        let r = Names.getThes icp.GPKTWG Names.Route Names.Fifty
        if r = "TOEDIENINGSWEG NIET INGEVULD" ||
           r = "PARENTERAAL" then Array.empty
        else [|r|]
        |> Array.collect (fun r ->
            r
            |> String.splitAt ','
            |> Array.map String.trim
        )

    let getDoseType (bas : Zindex.BST641T.BST641T) =
        Zindex.BST902T.records ()
        |> Array.tryFind (fun tx ->
            tx.MUTKOD <> 1 &&
            tx.TSNR = bas.GPDCTH &&
            tx.TSITNR = bas.GPDCOD
        )
        |> (fun r ->
            if r |> Option.isNone then ""
            else r.Value.THNM50.Trim())


    let getICPCText (icp : Zindex.BST642T.BST642T) =
        Zindex.BST380T.records ()
        |> Array.tryFind (fun i ->
            i.ICPCNR1 = icp.ICPCNR1
        )
        |> (fun r ->
            if r.IsNone then ""
            else r.Value.ICPCTXT.Trim()
        )


    let getFrequency (cat: Zindex.BST643T.BST643T) =
        Zindex.BST360T.records ()
        |> Array.tryFind (fun tx ->
            tx.MUTKOD <> 1 &&
            tx.TTEHNR = cat.GPDFEE
        )
        |> (fun r ->
            if r |> Option.isNone then ""
            else r.Value.TTEHOM.Trim())

        |> String.replace "om de dag" "per 2 dagen"
        |> String.replace "per half uur" "per 30 minuten"
        |> createFrequency cat.GPDFAA


    let parse gpks =

        query {
            // get all dose records
            for dos in  Zindex.BST649T.records () do
            // get 1 to 1 all category records
            join cat in Zindex.BST643T.records ()
                on (dos.GPDDNR = cat.GPDDNR)
            // get many to 1 all dose indications
            join icp in Zindex.BST642T.records ()
                on (cat.GPDCAT = icp.GPDCAT)
            // get many to 1 all prescription and trade products
            join bas in Zindex.BST641T.records ()
                on (icp.GPDBAS = bas.GPDBAS)
            // get many to 1 all generic products
            join vas in Zindex.BST640T.records ()
                on (bas.GPKODE = vas.GPKODE)

            where (dos.MUTKOD <> 1 &&
                   cat.MUTKOD <> 1 &&
                   icp.MUTKOD <> 1 &&
                   bas.MUTKOD <> 1 &&
                   vas.MUTKOD <> 1 &&
                   gpks |> Array.exists ((=) bas.GPKODE))

            select
                ((bas.GPKODE, bas.PRKODE, bas.HPKODE),
                {
                    empty with
                        Id           = dos.GPDDNR
                        Routes       = getICPCRoute icp
                        DoseType     = getDoseType bas
                        IndicationId = icp.GPDID1
                        Indication   = getICPCText icp
                        HighRisk     = vas.GPRISC = "*"

                        Gender =
                            if   vas.GPDGST = 1 then Constants.male
                            elif vas.GPDGST = 2 then Constants.female
                            else ""

                        CareGroup =
                            if   icp.GPDZCO = 1 then Constants.nonIntensive
                            elif icp.GPDZCO = 2 then Constants.intensive
                            else Constants.all

                        Usage =
                            if   icp.ICPCTO = 0 then ""
                            elif icp.ICPCTO = 1 then Constants.profylactic
                            else Constants.therapeutic

                        Age    = createMinMax cat.GPDLFM cat.GPDLFX
                        Weight = createMinMax cat.GPDKGM cat.GPDKGX
                        BSA    = createMinMax cat.GPDM2M cat.GPDM2X
                        Freq   = getFrequency cat
                        Norm   = createMinMax dos.GPNRMMIN dos.GPNRMMAX
                        Abs    = createMinMax dos.GPABSMIN dos.GPABSMAX
                        NormKg = createMinMax dos.GPNRMMINK dos.GPNRMMAXK
                        AbsKg  = createMinMax dos.GPABSMINK dos.GPABSMAXK
                        NormM2 = createMinMax dos.GPNRMMINM dos.GPNRMMAXM
                        AbsM2  = createMinMax dos.GPABSMINM dos.GPABSMAXM
                })
        }
        |> Seq.toArray
        // Get Generic products
        |> Array.map ((fun (bas, r) ->
            let gpk, _, _ = bas
            let gpks =
                getGenericProducts ()
                |> Array.filter (fun gp ->
                        gp.Id = gpk
                    )
            let rt =
                if (r.Routes |> Array.isEmpty)
                then
                    gpks
                    |> Array.collect (fun gp ->
                        gp.Route
                    )
                else r.Routes
            let un =
                match gpks |> Array.tryHead with
                | Some gp -> gp.Unit
                | None -> ""
            let r =
                {
                    r with
                        GenericProduct = gpks
                        Routes = rt
                        Unit = un
                }
            (bas, r)
        // Get prescription products
         ) >> (fun (bas, r) ->
            let _, prk, _ = bas
            let prks =
                getPresciptionProducts ()
                |> Array.filter (fun pp ->
                    pp.Id = prk
                )
            (bas, { r with PrescriptionProduct = prks })
        // Get trade products
        ) >> (fun (bas, r) ->
            let _, _, hpk = bas
            let hpks =
                getTradeProducts ()
                |> Array.filter (fun tp ->
                    tp.Id = hpk
                )
            { r with TradeProduct = hpks }
        ))


    let _get () =
        if FilePath.ruleCache |> File.exists then
            FilePath.ruleCache
            |> Json.getCache<DoseRule[]>
        else
            printfn "No cache creating DoseRule"
            let rules = GenPresProduct.getGPKS true |> parse
            rules |> Json.cache FilePath.ruleCache
            rules

    let get : unit -> DoseRule [] = Memoization.memoize _get

    let load () = get () |> ignore

    let toString2 (dr : DoseRule) =
        let addString lbl s =
            if s = "" then ""
            else
                lbl + ": " + s + ", "

        let freqToString (fr: Frequency) =
            (fr.Frequency |> string) + " " + (fr.Time |> string)

        let minMaxToString u (mm: MinMax) =
            let s =
                match mm.Min, mm.Max with
                | None, None -> ""
                | Some min, None -> "vanaf " + (min |> string)
                | None, Some max ->
                    if max = 0m then "" else "tot " + (max |> string)
                | Some min, Some max -> (min |> string) + " - " + (max |> string)
            if s = "" then "" else s + " " + u

        if dr.GenericProduct |> Array.length = 1 then
            dr.GenericProduct[0].Name + ": "
        else ""
        + (addString "Indicatie" dr.Indication)
        + (addString "Geslacht" dr.Gender)
        + (addString "Leeftijd" (dr.Age |> minMaxToString "maanden"))
        + (addString "Oppervlak" (dr.BSA |> minMaxToString "m2"))
        + (addString "Gewicht" (dr.Weight |> minMaxToString "kg"))
        + (addString "Frequentie" (dr.Freq |> freqToString))
        + (addString "Dosering" (dr.Norm |> minMaxToString dr.Unit))
        + (addString "Dose Per kg" (dr.NormKg |> minMaxToString dr.Unit))
        + (addString "Dose Per m2" (dr.NormM2 |> minMaxToString dr.Unit))
        + (addString "Grens Per kg" (dr.AbsKg |> minMaxToString dr.Unit))
        + (addString "Grens Per m2" (dr.AbsM2 |> minMaxToString dr.Unit))
        + (addString "Abs grens" (dr.Abs |> minMaxToString dr.Unit))
        |> String.remove 1

    let indications_ () =
        // Get all distinct indciations
        Zindex.BST642T.records ()
        |> Array.map getICPCText
        |> Array.distinct
        |> Array.sort

    let indications = Memoization.memoize indications_

    let routes_ () =
        Zindex.BST642T.records ()
        |> Array.collect getICPCRoute
        |> Array.distinct
        |> Array.sort

    let routes = Memoization.memoize routes_

    let frequencies_ () =
        Zindex.BST643T.records ()
        |> Array.map getFrequency
        |> Array.distinct
        |> Array.sortBy (fun f -> (f.Time, f.Frequency))

    let frequencies = Memoization.memoize frequencies_


    type DoseRule with

        static member Weight_ :
            (DoseRule -> MinMax) * (MinMax -> DoseRule -> DoseRule) =
            (fun dr -> dr.Weight) ,
            (fun mm dr -> { dr with Weight = mm })

        static member BSA_ :
            (DoseRule -> MinMax) * (MinMax -> DoseRule -> DoseRule) =
            (fun dr -> dr.BSA) ,
            (fun mm dr -> { dr with BSA = mm })

        static member Norm_ :
            (DoseRule -> MinMax) * (MinMax -> DoseRule -> DoseRule) =
            (fun dr -> dr.Norm) ,
            (fun mm dr -> { dr with Norm = mm })

        static member Abs_ :
            (DoseRule -> MinMax) * (MinMax -> DoseRule -> DoseRule) =
            (fun dr -> dr.Abs) ,
            (fun mm dr -> { dr with Abs = mm })

        static member NormKg_ :
            (DoseRule -> MinMax) * (MinMax -> DoseRule -> DoseRule) =
            (fun dr -> dr.NormKg) ,
            (fun mm dr -> { dr with NormKg = mm })

        static member AbsKg_ :
            (DoseRule -> MinMax) * (MinMax -> DoseRule -> DoseRule) =
            (fun dr -> dr.AbsKg) ,
            (fun mm dr -> { dr with AbsKg = mm })

        static member NormM2_ :
            (DoseRule -> MinMax) * (MinMax -> DoseRule -> DoseRule) =
            (fun dr -> dr.NormM2) ,
            (fun mm dr -> { dr with NormM2 = mm })

        static member AbsM2_ :
            (DoseRule -> MinMax) * (MinMax -> DoseRule -> DoseRule) =
            (fun dr -> dr.AbsM2) ,
            (fun mm dr -> { dr with AbsM2 = mm })



    module Optics =


        let getWeight = Optic.get DoseRule.Weight_


        let setWeight = Optic.set DoseRule.Weight_


        let getBSA = Optic.get DoseRule.BSA_


        let setBSA = Optic.set DoseRule.BSA_


        let getNorm = Optic.get DoseRule.Norm_


        let setNorm = Optic.set DoseRule.Norm_


        let getAbs = Optic.get DoseRule.Abs_


        let setAbs = Optic.set DoseRule.Abs_


        let getNormKg = Optic.get DoseRule.NormKg_


        let setNormKg = Optic.set DoseRule.NormKg_


        let getAbsKg = Optic.get DoseRule.AbsKg_


        let setAbsKg = Optic.set DoseRule.AbsKg_


        let getNormM2 = Optic.get DoseRule.NormM2_


        let setNormM2 = Optic.set DoseRule.NormM2_


        let getAbsM2 = Optic.get DoseRule.AbsM2_


        let setAbsM2 = Optic.set DoseRule.AbsM2_
