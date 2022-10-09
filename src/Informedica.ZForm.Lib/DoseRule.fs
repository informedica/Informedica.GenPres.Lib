namespace Informedica.ZForm.Lib

module DoseRule =

    open System
    open MathNet.Numerics

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL

    open Aether
    open Aether.Operators


    /// Models a medication dose range with lower and upper limits
    /// * Norm : the 'normal' non adjusted upper and lower limits
    /// * NormWeight : the 'normal' by weight adjusted upper and lower limits
    /// * NormBSA : the 'normal' by bsa adjusted upper and lower limits
    /// * Abs : the 'absolute' non adjusted upper and lower limits
    /// * AbsWeight : the 'absolute' by weight adjusted upper and lower limits
    /// * AbsBSA : the 'absolute' by bsa adjusted upper and lower limits
    module DoseRange =


        module ValueUnit = Informedica.GenUnits.Lib.ValueUnit


        type MinMax = MinMax.MinMax


        /// Dose limits
        type DoseRange =
            {
                // Normal limits
                Norm : MinMax
                // Normal limits adjusted by weight
                NormWeight : MinMax * WeightUnit
                // Normal limits adjusted by BSA
                NormBSA : MinMax * BSAUnit
                // Absolute limits
                Abs : MinMax
                // Absolute limits adjusted by weight
                AbsWeight : MinMax * WeightUnit
                // Absolute limits adjusted by BSA
                AbsBSA : MinMax * BSAUnit
            }
        and WeightUnit = ValueUnit.Unit
        and BSAUnit = ValueUnit.Unit

        let create norm normWght normBSA abs absWght absBSA =
            {
                Norm = norm
                NormWeight = normWght
                NormBSA = normBSA
                Abs = abs
                AbsWeight = absWght
                AbsBSA = absBSA
            }

        let emptyWeight = MinMax.empty, ValueUnit.NoUnit


        let emptyBSA = MinMax.empty, ValueUnit.NoUnit


        let empty = create MinMax.empty emptyWeight emptyBSA MinMax.empty emptyWeight emptyBSA


        let count n =
            let setMinIncl = Optic.set MinMax.Optics.inclMinLens
            let setMaxIncl = Optic.set MinMax.Optics.inclMaxLens

            let mm =
                MinMax.empty
                |> setMinIncl (Some n)
                |> setMaxIncl (Some n)

            let wmm = (mm, ValueUnit.Units.Weight.kiloGram)

            let bmm = (mm, ValueUnit.Units.BSA.M2)

            create mm wmm bmm mm wmm bmm


        let calc op (dr1 : DoseRange) (dr2 : DoseRange) =
            {
                empty with
                    Norm = dr1.Norm |> op <| dr2.Norm
                    NormWeight =
                        (dr1.NormWeight |> fst) |> op <| (dr2.NormWeight |> fst),
                        (dr1.NormWeight |> snd)
                    NormBSA =
                        (dr1.NormBSA |> fst)  |> op <|  (dr2.NormBSA |> fst),
                        (dr1.NormBSA |> snd)
                    Abs = dr1.Abs |> op <| dr2.Abs
                    AbsWeight =
                        (dr1.AbsWeight |> fst)  |> op <|  (dr2.AbsWeight |> fst),
                        (dr1.AbsWeight |> snd)
                    AbsBSA =
                        (dr1.AbsBSA |> fst)  |> op <|  (dr2.AbsBSA |> fst),
                        (dr1.AbsBSA |> snd)
            }


        let convertTo u (dr: DoseRange) =

            {
                dr with
                    Norm = dr.Norm |> MinMax.convertTo u
                    NormWeight =
                        dr.NormWeight |> fst |> MinMax.convertTo u ,
                        dr.NormWeight |> snd
                    NormBSA =
                        dr.NormBSA |> fst |> MinMax.convertTo u ,
                        dr.NormBSA |> snd
                    Abs = dr.Abs |> MinMax.convertTo u
                    AbsWeight =
                        dr.AbsWeight |> fst |> MinMax.convertTo u ,
                        dr.AbsWeight |> snd
                    AbsBSA =
                        dr.AbsBSA |> fst |> MinMax.convertTo u ,
                        dr.AbsBSA |> snd
            }


        type DoseRange with

            static member Norm_ :
                (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
                (fun dr -> dr.Norm),
                (fun mm dr -> { dr with Norm = mm })

            static member NormWeight_ :
                (DoseRange -> (MinMax * WeightUnit)) * ((MinMax * WeightUnit) -> DoseRange -> DoseRange) =
                (fun dr -> dr.NormWeight),
                (fun mm dr -> { dr with NormWeight = mm })

            static member NormBSA_ :
                (DoseRange -> (MinMax * BSAUnit)) * ((MinMax * BSAUnit) -> DoseRange -> DoseRange) =
                (fun dr -> dr.NormBSA),
                (fun mm dr -> { dr with NormBSA = mm })

            static member Abs_ :
                (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
                (fun dr -> dr.Abs),
                (fun mm dr -> { dr with Abs = mm })

            static member AbsWeight_ :
                (DoseRange -> (MinMax * WeightUnit)) * ((MinMax * WeightUnit) -> DoseRange -> DoseRange) =
                (fun dr -> dr.AbsWeight),
                (fun mm dr -> { dr with AbsWeight = mm })

            static member AbsBSA_ :
                (DoseRange -> (MinMax * BSAUnit)) * ((MinMax * BSAUnit) -> DoseRange -> DoseRange) =
                (fun dr -> dr.AbsBSA),
                (fun mm dr -> { dr with AbsBSA = mm })

            static member (*) (dr1, dr2) = calc (*) dr1 dr2

            static member (/) (dr1, dr2) = calc (/) dr1 dr2



        module Optics =

            module MinMax = MinMax.Optics


            let inclMinNormLens =
                DoseRange.Norm_ >-> MinMax.inclMinLens


            let exclMinNormLens =
                DoseRange.Norm_ >-> MinMax.exclMinLens


            let inclMaxNormLens =
                DoseRange.Norm_ >-> (MinMax.inclMaxLens)


            let exclMaxNormLens =
                DoseRange.Norm_ >-> (MinMax.exclMaxLens)


            let normWeightUnitLens = DoseRange.NormWeight_ >-> snd_


            let inclMinNormWeightLens =
                DoseRange.NormWeight_ >-> fst_ >-> MinMax.inclMinLens


            let exclMinNormWeightLens =
                DoseRange.NormWeight_ >-> fst_ >-> MinMax.exclMinLens


            let inclMaxNormWeightLens =
                DoseRange.NormWeight_ >-> fst_ >-> MinMax.inclMaxLens


            let exclMaxNormWeightLens =
                DoseRange.NormWeight_ >-> fst_ >-> MinMax.exclMaxLens


            let normBSAUnitLens = DoseRange.NormBSA_ >-> snd_


            let inclMinNormBSALens =
                DoseRange.NormBSA_ >-> fst_ >-> MinMax.inclMinLens


            let exclMinNormBSALens =
                DoseRange.NormBSA_ >-> fst_ >-> MinMax.exclMinLens


            let inclMaxNormBSALens =
                DoseRange.NormBSA_ >-> fst_ >-> MinMax.inclMaxLens


            let exclMaxNormBSALens =
                DoseRange.NormBSA_ >-> fst_ >-> MinMax.exclMaxLens


            let minAbsLens = DoseRange.Abs_ >-> MinMax.Min_


            let inclMinAbsLens =
                DoseRange.Abs_ >-> (MinMax.inclMinLens)


            let exclMinAbsLens =
                DoseRange.Abs_ >-> (MinMax.exclMinLens)


            let inclMaxAbsLens =
                DoseRange.Abs_ >-> (MinMax.inclMaxLens)


            let exclMaxAbsLens =
                DoseRange.Abs_ >-> (MinMax.exclMaxLens)


            let absWeightUnitLens = DoseRange.AbsWeight_ >-> snd_


            let inclMinAbsWeightLens =
                DoseRange.AbsWeight_ >-> fst_ >-> MinMax.inclMinLens


            let exclMinAbsWeightLens =
                DoseRange.AbsWeight_ >-> fst_ >-> MinMax.exclMinLens


            let inclMaxAbsWeightLens =
                DoseRange.AbsWeight_ >-> fst_ >-> MinMax.inclMaxLens


            let exclMaxAbsWeightLens =
                DoseRange.AbsWeight_ >-> fst_ >-> MinMax.exclMaxLens


            let absBSAUnitLens = DoseRange.AbsBSA_ >-> snd_


            let inclMinAbsBSALens =
                DoseRange.AbsBSA_ >-> fst_ >-> MinMax.inclMinLens


            let exclMinAbsBSALens =
                DoseRange.AbsBSA_ >-> fst_ >-> MinMax.exclMinLens


            let inclMaxAbsBSALens =
                DoseRange.AbsBSA_ >-> fst_ >-> MinMax.inclMaxLens


            let exclMaxAbsBSALens =
                DoseRange.AbsBSA_ >-> fst_ >-> MinMax.exclMaxLens


        let toString ru ({ Norm = norm; NormWeight = normwght; NormBSA = normbsa; Abs = abs; AbsWeight = abswght; AbsBSA = absbsa}) =
            let (>+) sl sr =
                let sl = sl |> String.trim
                let sr = sr |> String.trim

                if sl |> String.isNullOrWhiteSpace then sr
                else
                    let sr = if sr |> String.isNullOrWhiteSpace then sr else " of " + sr
                    sl + sr


            let optRate mm =
                match ru with
                | Some u ->
                    mm / (u |> MinMax.one)
                | None -> mm

            let norm, abs =
                if norm = abs then
                    MinMax.empty, abs |> optRate
                else
                    norm |> optRate, abs |> optRate

            let nw, nuw = normwght
            let nb, nub = normbsa
            let aw, auw = abswght
            let ab, aub = absbsa

            let nw, aw =
                if nw = aw then
                    MinMax.empty ,
                    aw / (auw |> MinMax.one) |> optRate
                else
                    nw / (nuw |> MinMax.one) |> optRate ,
                    aw / (auw |> MinMax.one) |> optRate

            let nb, ab =
                if nb = ab then
                    MinMax.empty ,
                    ab / (aub |> MinMax.one) |> optRate
                else
                    nb / (nub |> MinMax.one) |> optRate ,
                    ab / (aub |> MinMax.one) |> optRate

            let mmToStr = MinMax.toString "van" "tot"

            norm
            |> mmToStr
            >+ (nw |> mmToStr)
            >+ (nb |> mmToStr)
            |> (fun s ->
                let s = s |> String.trim

                if s |> String.isNullOrWhiteSpace then s
                else " " + s
            )
            |> (fun sn ->
                let sn = sn |> String.trim

                let sa =
                    abs |> mmToStr
                    >+ (aw |> mmToStr)
                    >+ (ab |> mmToStr)

                if sa |> String.isNullOrWhiteSpace then sn
                else
                    let sn = if sn |> String.isNullOrWhiteSpace then sn else sn + " "
                    sn + "maximaal " + sa
            )


        module Dto =

            type Dto () =
                member val Norm = MinMax.Dto.dto () with get, set
                member val NormWeight = MinMax.Dto.dto () with get, set
                member val NormWeightUnit = "" with get, set
                member val NormBSA = MinMax.Dto.dto () with get, set
                member val NormBSAUnit = ""
                member val Abs = MinMax.Dto.dto () with get, set
                member val AbsWeight = MinMax.Dto.dto () with get, set
                member val AbsWeightUnit = "" with get, set
                member val AbsBSA = MinMax.Dto.dto () with get, set
                member val AbsBSAUnit = "" with get, set

            let dto () = Dto ()

            let toDto (dr: DoseRange) =
                let dto = dto ()
                let unstr = snd >> ValueUnit.unitToReadableString
                let grstr = snd >> ValueUnit.Group.unitToGroup >> ValueUnit.Group.toString

                dto.Norm <- dr.Norm |> MinMax.Dto.toDto
                dto.NormWeight <- dr.NormWeight |> fst |> MinMax.Dto.toDto
                dto.NormWeightUnit <- dr.NormWeight |> unstr
                dto.NormBSA <- dr.NormBSA |> fst  |> MinMax.Dto.toDto
                dto.Abs <- dr.Abs |> MinMax.Dto.toDto
                dto.AbsWeight <- dr.AbsWeight |> fst  |> MinMax.Dto.toDto
                dto.AbsWeightUnit <- dr.AbsWeight |> unstr
                dto.AbsBSA <- dr.AbsBSA |> fst |> MinMax.Dto.toDto
                dto.AbsBSAUnit <- dr.AbsBSA |> unstr

                dto

            let fromDto (dto: Dto) =
                let set f o x =
                    match x |> f with | Some x -> x | None -> o

                let setug u g o mm =
                    if u |> String.isNullOrWhiteSpace || g |> String.isNullOrWhiteSpace then o
                    else
                        match
                            sprintf "%s[%s]" u g
                            |> ValueUnit.Units.fromString with
                        | None -> o
                        | Some u ->
                            match mm |> MinMax.Dto.fromDto with
                            | Some mm -> (mm, u)
                            | None -> o

                {   empty with
                        Norm = dto.Norm |> set MinMax.Dto.fromDto empty.Norm
                        NormWeight =
                            dto.NormWeight
                            |> setug dto.NormWeightUnit "weight" empty.NormWeight
                        NormBSA =
                            dto.NormBSA
                            |> setug dto.NormBSAUnit "bsa" empty.NormBSA
                        Abs = dto.Abs |> set MinMax.Dto.fromDto empty.Abs
                        AbsWeight =
                            dto.AbsWeight
                            |> setug dto.AbsWeightUnit "weight" empty.AbsWeight
                        AbsBSA =
                            dto.AbsBSA
                            |> setug dto.AbsBSAUnit "bsa" empty.AbsBSA
                }


        module DoseRangeTests =

            let (|>!) x f =
                printfn "%A" x
                x |> f

            let tests () =
                let dto = Dto.dto ()

                dto.Norm.HasMin <- true
                dto.Norm.Min.Value <- 1.
                dto.Norm.Min.Unit <- "mg"
                dto.Norm.Min.Group <- "mass"

                dto.Norm.HasMax <- true
                dto.Norm.Max.Value <- 10.
                dto.Norm.Max.Unit <- "mg"
                dto.Norm.Max.Group <- "mass"

                dto.NormWeight.HasMin <- true
                dto.NormWeight.Min.Value <- 0.01
                dto.NormWeight.Min.Unit <- "mg"
                dto.NormWeight.Min.Group <- "mass"
                dto.NormWeightUnit <- "kg"

                dto.NormWeight.HasMax <- true
                dto.NormWeight.Max.Value <- 1.
                dto.NormWeight.Max.Unit <- "mg"
                dto.NormWeight.Max.Group <- "mass"
                dto.NormWeightUnit <- "kg"

                dto
                |>! Dto.fromDto
                |>! Dto.toDto
                |>! Dto.fromDto
                |>! ignore


    /// Models a drug dosage. For each combination
    /// of a drug, indication there is one dosage.
    /// The indication is identified by the name of
    /// the dosage. Per dosage the following `DoseRange`
    /// items can be defined:
    /// * StartDosage: dosage at the start
    /// * SingleDosage: dosage per administration
    /// * RateDosage: dosage rate, has a rate unit
    /// * TotalDosage: dosage per time period, has a `Frequency`
    /// The frequency is defined by a list of possible frequencies
    /// per time period and/or a minimal interval
    module Dosage =

        module ValueUnit = Informedica.GenUnits.Lib.ValueUnit


        type DoseRange = DoseRange.DoseRange
        type ValueUnit = ValueUnit.ValueUnit
        type Unit = ValueUnit.Unit

        /// Dosage
        type Dosage =
            {
                /// Indentifies the indication
                Name : string
                /// Dosage at the start
                StartDosage : DoseRange
                /// Dosage per administration
                SingleDosage : DoseRange
                /// Dosage rate
                RateDosage : DoseRange * RateUnit
                /// Total dosage per time period
                TotalDosage : DoseRange * Frequency
                /// List of original doserules
                Rules : Rule list
            }
        and Frequency =
            {
                Frequencies : Frequencies
                TimeUnit : TimeUnit
                MinimalInterval : ValueUnit Option
            }
        and Frequencies = ValueUnit.Value list
        and TimeUnit = Unit
        and RateUnit = Unit
        and Rule = GStandRule of string | PedFormRule of string

        let createFrequency frs tu mi =
            {
                Frequencies = frs
                TimeUnit = tu
                MinimalInterval = mi
            }


        let create nm start single rate total rls =
            {
                Name = nm
                StartDosage = start
                SingleDosage = single
                RateDosage = rate
                TotalDosage = total
                Rules = rls
            }


        let emptyFrequency = { Frequencies = []; TimeUnit = ValueUnit.NoUnit; MinimalInterval = None }


        let empty =
            create
                ""
                DoseRange.empty
                DoseRange.empty
                (DoseRange.empty, ValueUnit.NoUnit)
                (DoseRange.empty, emptyFrequency)
                []


        let convertSubstanceUnitTo u (ds : Dosage) =
            let convert = DoseRange.convertTo u

            {
                ds with
                    StartDosage = ds.StartDosage |> convert
                    SingleDosage = ds.SingleDosage |> convert
                    RateDosage =
                        ds.RateDosage |> fst |> convert ,
                        ds.RateDosage |> snd
                    TotalDosage =
                        ds.TotalDosage |> fst |> convert ,
                        ds.TotalDosage |> snd
            }


        let convertRateUnitTo u (ds : Dosage) =
            let getCount u1 u2 =
                1N
                |> ValueUnit.create u2
                |> ValueUnit.convertTo u1
                |> ValueUnit.get
                |> fst
                |> ValueUnit.create ValueUnit.Units.Count.times
                |> DoseRange.count

            {
                ds with
                    RateDosage =
                        let factor =
                            ds.RateDosage
                            |> snd
                            |> getCount u

                        (ds.RateDosage |> fst) / factor,
                        u
            }


        type Frequency with

            static member Frequencies_ :
                (Frequency -> Frequencies) * (Frequencies -> Frequency -> Frequency) =
                (fun fr -> fr.Frequencies) ,
                (fun frs fr -> { fr with Frequencies = frs })

            static member TimeUnit_ :
                (Frequency -> TimeUnit) * (TimeUnit -> Frequency -> Frequency) =
                (fun fr -> fr.TimeUnit) ,
                (fun tu fr -> { fr with TimeUnit = tu })

            static member MinimalInterval_ :
                (Frequency -> ValueUnit Option) * (ValueUnit Option -> Frequency -> Frequency) =
                (fun fr -> fr.MinimalInterval) ,
                (fun mi fr -> { fr with MinimalInterval = mi })


        type Dosage with

            static member Name_ :
                (Dosage -> string) * (string -> Dosage -> Dosage) =
                (fun d -> d.Name),
                (fun s d -> { d with Name = s })

            static member StartDosage_ :
                (Dosage -> DoseRange) * (DoseRange -> Dosage -> Dosage) =
                (fun d -> d.StartDosage),
                (fun dr d -> { d with StartDosage = dr })

            static member SingleDosage_ :
                (Dosage -> DoseRange) * (DoseRange -> Dosage -> Dosage) =
                (fun d -> d.SingleDosage),
                (fun dr d -> { d with SingleDosage = dr })

            static member RateDosage_ :
                (Dosage -> (DoseRange * RateUnit)) * ((DoseRange * RateUnit) -> Dosage -> Dosage) =
                (fun d -> d.RateDosage),
                (fun dr d -> { d with RateDosage = dr })

            static member TotalDosage_ :
                (Dosage -> (DoseRange * Frequency)) * ((DoseRange * Frequency) -> Dosage -> Dosage) =
                (fun d -> d.TotalDosage),
                (fun dt d -> { d with TotalDosage = dt })

            static member Rules_ :
                (Dosage -> Rule list) * (Rule list -> Dosage -> Dosage) =
                (fun d -> d.Rules) ,
                (fun rs d -> { d with Rules = rs })


        module Optics =

            module DoseRange = DoseRange.Optics


            let getName = Optic.get Dosage.Name_


            let setName = Optic.set Dosage.Name_


            let freqsFrequencyLens =
                Dosage.TotalDosage_ >-> snd_ >-> Frequency.Frequencies_


            let getFrequencyValues = Optic.get freqsFrequencyLens


            let setFrequencyValues = Optic.set freqsFrequencyLens


            let timeUnitFrequencyLens =
                Dosage.TotalDosage_ >-> snd_ >-> Frequency.TimeUnit_


            let getFrequencyTimeUnit = Optic.get timeUnitFrequencyLens


            let setFrequencyTimeUnit = Optic.set timeUnitFrequencyLens


            let minIntervalValueFrequencyLens =
                Dosage.TotalDosage_ >-> snd_ >-> Frequency.MinimalInterval_


            let inclMinNormStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.inclMinNormLens


            let exclMinNormStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.exclMinNormLens


            let inclMaxNormStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.inclMaxNormLens


            let exclMaxNormStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.exclMaxNormLens


            let normWeightUnitStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.normWeightUnitLens


            let inclMinNormWeightStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.inclMinNormWeightLens


            let exclMinNormWeightStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.exclMinNormWeightLens


            let inclMaxNormWeightStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.inclMaxNormWeightLens


            let exclMaxNormWeightStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.exclMaxNormWeightLens


            let normBSAUnitStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.normBSAUnitLens


            let inclMinNormBSAStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.inclMinNormBSALens


            let exclMinNormBSAStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.exclMinNormBSALens


            let inclMaxNormBSAStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.inclMaxNormBSALens


            let exclMaxNormBSAStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.exclMaxNormBSALens


            let inclMinNormSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.inclMinNormLens


            let exclMinNormSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.exclMinNormLens


            let inclMaxNormSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.inclMaxNormLens


            let exclMaxNormSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.exclMaxNormLens


            let normWeightUnitSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.normWeightUnitLens


            let inclMinNormWeightSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.inclMinNormWeightLens


            let exclMinNormWeightSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.exclMinNormWeightLens


            let inclMaxNormWeightSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.inclMaxNormWeightLens


            let exclMaxNormWeightSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.exclMaxNormWeightLens


            let normBSAUnitSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.normBSAUnitLens


            let inclMinNormBSASingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.inclMinNormBSALens


            let exclMinNormBSASingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.exclMinNormBSALens


            let inclMaxNormBSASingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.inclMaxNormBSALens


            let exclMaxNormBSASingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.exclMaxNormBSALens


            let rateUnitRateDosagePrism =
                Dosage.RateDosage_ >-> snd_


            let normWeightUnitRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.normWeightUnitLens


            let inclMinNormRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinNormLens


            let exclMinNormRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinNormLens


            let inclMaxNormRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxNormLens


            let exclMaxNormRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxNormLens


            let inclMinNormWeightRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinNormWeightLens


            let exclMinNormWeightRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinNormWeightLens


            let inclMaxNormWeightRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxNormWeightLens


            let exclMaxNormWeightRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxNormWeightLens


            let normBSAUnitRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.normBSAUnitLens


            let inclMinNormBSARateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinNormBSALens


            let exclMinNormBSARateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinNormBSALens


            let inclMaxNormBSARateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxNormBSALens


            let exclMaxNormBSARateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxNormBSALens


            let timeUnitTotalDosagePrism =
                Dosage.TotalDosage_ >-> snd_


            let normWeightUnitTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.normWeightUnitLens


            let inclMinNormTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinNormLens


            let exclMinNormTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinNormLens


            let inclMaxNormTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxNormLens


            let exclMaxNormTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxNormLens


            let inclMinNormWeightTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinNormWeightLens


            let exclMinNormWeightTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinNormWeightLens


            let inclMaxNormWeightTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxNormWeightLens


            let exclMaxNormWeightTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxNormWeightLens


            let normBSAUnitTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.normBSAUnitLens


            let inclMinNormBSATotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinNormBSALens


            let exclMinNormBSATotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinNormBSALens


            let inclMaxNormBSATotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxNormBSALens


            let exclMaxNormBSATotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxNormBSALens


            let inclMinAbsStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.inclMinAbsLens


            let exclMinAbsStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.exclMinAbsLens


            let inclMaxAbsStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.inclMaxAbsLens


            let exclMaxAbsStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.exclMaxAbsLens


            let absWeightUnitStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.absWeightUnitLens


            let inclMinAbsWeightStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.inclMinAbsWeightLens


            let exclMinAbsWeightStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.exclMinAbsWeightLens


            let inclMaxAbsWeightStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.inclMaxAbsWeightLens


            let exclMaxAbsWeightStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.exclMaxAbsWeightLens


            let absBSAUnitStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.absBSAUnitLens


            let inclMinAbsBSAStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.inclMinAbsBSALens


            let exclMinAbsBSAStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.exclMinAbsBSALens


            let inclMaxAbsBSAStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.inclMaxAbsBSALens


            let exclMaxAbsBSAStartDosagePrism =
                Dosage.StartDosage_ >-> DoseRange.exclMaxAbsBSALens


            let inclMinAbsSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.inclMinAbsLens


            let exclMinAbsSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.exclMinAbsLens


            let inclMaxAbsSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.inclMaxAbsLens


            let exclMaxAbsSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.exclMaxAbsLens


            let absWeightUnitSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.absWeightUnitLens


            let inclMinAbsWeightSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.inclMinAbsWeightLens


            let exclMinAbsWeightSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.exclMinAbsWeightLens


            let inclMaxAbsWeightSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.inclMaxAbsWeightLens


            let exclMaxAbsWeightSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.exclMaxAbsWeightLens


            let absBSAUnitSingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.absBSAUnitLens


            let inclMinAbsBSASingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.inclMinAbsBSALens


            let exclMinAbsBSASingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.exclMinAbsBSALens


            let inclMaxAbsBSASingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.inclMaxAbsBSALens


            let exclMaxAbsBSASingleDosagePrism =
                Dosage.SingleDosage_ >-> DoseRange.exclMaxAbsBSALens


            let inclMinAbsRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinAbsLens


            let exclMinAbsRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinAbsLens


            let inclMaxAbsRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxAbsLens


            let exclMaxAbsRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxAbsLens


            let absWeightUnitRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.absWeightUnitLens


            let inclMinAbsWeightRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinAbsWeightLens


            let exclMinAbsWeightRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinAbsWeightLens


            let inclMaxAbsWeightRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxAbsWeightLens


            let exclMaxAbsWeightRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxAbsWeightLens


            let absBSAUnitRateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.absBSAUnitLens


            let inclMinAbsBSARateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinAbsBSALens


            let exclMinAbsBSARateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinAbsBSALens


            let inclMaxAbsBSARateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxAbsBSALens


            let exclMaxAbsBSARateDosagePrism =
                Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxAbsBSALens


            let inclMinAbsTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinAbsLens


            let exclMinAbsTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinAbsLens


            let inclMaxAbsTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxAbsLens


            let exclMaxAbsTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxAbsLens


            let absWeightUnitTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.absWeightUnitLens


            let inclMinAbsWeightTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinAbsWeightLens


            let exclMinAbsWeightTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinAbsWeightLens


            let inclMaxAbsWeightTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxAbsWeightLens


            let exclMaxAbsWeightTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxAbsWeightLens


            let absBSAUnitTotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.absBSAUnitLens


            let inclMinAbsBSATotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinAbsBSALens


            let exclMinAbsBSATotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinAbsBSALens


            let inclMaxAbsBSATotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxAbsBSALens


            let exclMaxAbsBSATotalDosagePrism =
                Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxAbsBSALens



        let freqsToStr (freqs : Frequency) =
            let fu =
                freqs.TimeUnit
                |> ValueUnit.unitToReadableString
                |> String.replace "x/" ""


            if freqs.Frequencies |> List.isConsecutive 0N 1N |> not then
                freqs.Frequencies |> List.toString
            else
                match freqs.Frequencies |> List.headTail with
                | Some h, Some t -> sprintf "%s - %s" (h.ToString ()) (t.ToString ())
                | _ -> freqs.Frequencies |> List.toString
            |> (fun s ->
                if s |> String.isNullOrWhiteSpace ||
                   s |> String.isNullOrWhiteSpace then ""
                else
                    sprintf "%s x/%s" s fu
            )


        let toString rules ({ Name = n; StartDosage = start; SingleDosage = single; RateDosage = rate; TotalDosage = total; Rules = rs }) =
            let vuToStr = ValueUnit.toStringPrec 2

            let (>+) sl sr =
                let l, s = sr

                if s |> String.isNullOrWhiteSpace then sl
                else
                    let sl = sl |> String.trim
                    (if sl |> String.isNullOrWhiteSpace then sl else sl + ", ") +
                    (if l |> String.isNullOrWhiteSpace then "" else  l + " ") + s

            let rt, ru = rate
            let tt, _ = total

            let frqs = total |> snd
            let fu =
                frqs.TimeUnit
                |> ValueUnit.unitToReadableString
                |> String.replace "x/" ""

            let drToStr = DoseRange.toString None

            ""
            >+ ("oplaad:", start |> drToStr)
            >+ ("per keer:", single |> drToStr)
            >+ ("snelheid:", rt |> DoseRange.toString (Some ru))
            >+ ("dosering:", tt |> drToStr)
            |> (fun s ->
                let  s = s |> String.trim
                if frqs.Frequencies |> List.isEmpty ||
                   fu |> String.isNullOrWhiteSpace then s
                else
                    sprintf "%s in %s" s (frqs |> freqsToStr)
                    |> (fun s ->
                        match frqs.MinimalInterval with
                        | Some mi ->
                            s + " " + (sprintf "minimaal interval: %s" (mi |> vuToStr))
                        | None -> s

                    )
            )
            |> String.removeTrailing ["\n"]
            |> (fun s -> (n |> String.toLower) + " " + (s |> String.trim))
            |> (fun s ->
                if not rules then s
                else
                    s + "\n" +
                    (rs
                     |> List.map (fun r -> match r with | GStandRule r | PedFormRule r -> r)
                     |> String.concat "\n")
            )


        module Dto =

            open MathNet.Numerics

            type Dto () =
                member val Name = "" with get, set
                member val StartDosage = DoseRange.Dto.dto () with get, set
                member val SingleDosage = DoseRange.Dto.dto () with get, set
                member val DoseRate = DoseRange.Dto.dto () with get, set
                member val DoseRateUnit = "" with get, set
                member val TotalDosage = DoseRange.Dto.dto () with get, set
                member val TotalDosageFrequencyValues : float list = [] with get, set
                member val TotalDosageFrequencyUnit = "" with get, set
                member val TotalDosageHasMinimalPeriod = false with get, set
                member val TotalDosageMinimalPeriod = ValueUnit.Dto.dto () with get, set
                member val GStandRules : string list = [] with get, set
                member val PedFormRules : string list = [] with get, set


            let dto () = Dto ()

            let toDto (ds : Dosage) =
                let dto = dto ()

                dto.Name <- ds.Name
                dto.StartDosage <- ds.StartDosage |> DoseRange.Dto.toDto
                dto.SingleDosage <- ds.SingleDosage |> DoseRange.Dto.toDto
                dto.DoseRate <- ds.RateDosage |> fst |> DoseRange.Dto.toDto
                dto.DoseRateUnit <- ds.RateDosage |> snd |> ValueUnit.unitToReadableString
                dto.TotalDosage <- ds.TotalDosage |> fst |> DoseRange.Dto.toDto
                match ds.TotalDosage |> snd  with
                | { Frequencies = freqs; TimeUnit = u; MinimalInterval = vu } ->
                    dto.TotalDosageFrequencyValues <-
                        freqs
                        |> List.map BigRational.toFloat
                    dto.TotalDosageFrequencyUnit <-
                        u
                        |> ValueUnit.unitToReadableString
                    match vu with
                    | None -> ()
                    | Some vu ->
                        dto.TotalDosageHasMinimalPeriod <- true
                        dto.TotalDosageMinimalPeriod <- vu |> ValueUnit.Dto.toDtoDutchShort
                ds.Rules
                |> List.iter (fun r ->
                    match r with
                    | GStandRule s  -> dto.GStandRules  <- [s] |> List.append dto.GStandRules
                    | PedFormRule s -> dto.PedFormRules <- [s] |> List.append dto.PedFormRules
                )

                dto


            let fromDto (dto: Dto) =
                {   empty with
                        Name = dto.Name
                        StartDosage  = dto.StartDosage  |> DoseRange.Dto.fromDto
                        SingleDosage = dto.SingleDosage |> DoseRange.Dto.fromDto
                        RateDosage =
                            match dto.DoseRateUnit |> ValueUnit.readableStringToTimeUnit with
                            | None -> empty.RateDosage
                            | Some u ->
                                dto.DoseRate |> DoseRange.Dto.fromDto, u
                        TotalDosage =
                            match dto.TotalDosageFrequencyUnit |> ValueUnit.readableStringToTimeUnit with
                            | None -> empty.TotalDosage
                            | Some u ->
                                dto.TotalDosage |> DoseRange.Dto.fromDto,
                                {   Frequencies =
                                        dto.TotalDosageFrequencyValues
                                        |> List.map BigRational.fromFloat
                                        |> List.filter Option.isSome
                                        |> List.map Option.get
                                    TimeUnit = u
                                    MinimalInterval =
                                        if dto.TotalDosageHasMinimalPeriod |> not then None
                                        else
                                            dto.TotalDosageMinimalPeriod
                                            |> ValueUnit.Dto.fromDto
                                }
                        Rules =
                            dto.GStandRules
                            |> List.map GStandRule
                            |> List.append (dto.PedFormRules |> List.map PedFormRule)
                }



    module PatientDosage =

        type Dosage = Dosage.Dosage
        type Patient = Patient.Patient

        type PatientDosage =
            {
                // The patient group the doserules applies
                Patient : Patient
                // List of shapes that have a dosage
                ShapeDosage : Dosage
                // List of substances that have a dosage
                SubstanceDosages : Dosage list
            }

        let create pat =
            { Patient = pat; ShapeDosage = Dosage.empty; SubstanceDosages = [] }


        type PatientDosage with

            static member Patient_ :
                (PatientDosage -> Patient) * (Patient -> PatientDosage -> PatientDosage) =
                (fun pd -> pd.Patient) ,
                (fun pat pd -> { pd with Patient = pat })

            static member ShapeDosage_ :
                (PatientDosage -> Dosage) * (Dosage -> PatientDosage -> PatientDosage) =
                (fun pd -> pd.ShapeDosage) ,
                (fun sd pd -> { pd with ShapeDosage = sd })

            static member SubstanceDosages_ :
                (PatientDosage -> Dosage list) * (Dosage list -> PatientDosage -> PatientDosage) =
                (fun sd -> sd.SubstanceDosages) ,
                (fun d sd -> { sd with SubstanceDosages = d })



        module Optics =


            let setPatient = Optic.set PatientDosage.Patient_

            let getPatient = Optic.get PatientDosage.Patient_

            let setShapeDosage = Optic.set PatientDosage.ShapeDosage_

            let getShapeDosage = Optic.get PatientDosage.ShapeDosage_

            let setSubstanceDosages = Optic.set PatientDosage.SubstanceDosages_

            let getSubstanceDosages = Optic.get PatientDosage.SubstanceDosages_



        module Dto =


            type Dto () =
                    // The patient group the doserules applies
                    member val Patient = Patient.Dto.dto () with get, set
                    // List of shapes that have a dosage
                    member val ShapeDosage = Dosage.Dto.dto () with get, set
                    // List of substances that have a dosage
                    member val SubstanceDosages : Dosage.Dto.Dto list = [] with get, set

            let dto () = Dto ()

            let fromDto (dto : Dto) =
                match
                    dto.Patient
                    |> Patient.Dto.fromDto with
                | None -> None
                | Some p ->
                    p
                    |> create
                    |> Optics.setShapeDosage (dto.ShapeDosage |> Dosage.Dto.fromDto)
                    |> Optics.setSubstanceDosages (dto.SubstanceDosages |> List.map Dosage.Dto.fromDto)
                    |> Some


            let toDto (pd : PatientDosage) =
                let dto = dto ()

                dto.Patient <- pd.Patient |> Patient.Dto.toDto
                dto.ShapeDosage <- pd.ShapeDosage |> Dosage.Dto.toDto
                dto.SubstanceDosages <-
                    pd.SubstanceDosages
                    |> List.map Dosage.Dto.toDto

                dto



    module ShapeDosage =


        module TradeProduct =


            type TradeProduct = { HPK : int; Label : string }

            let create hpk label = { HPK = hpk; Label = label }

            let apply f (x: TradeProduct) = x |> f

            let get = apply id

            let label tp = (tp |> get).Label

            let hpk tp = (tp |> get).HPK


            type TradeProduct with

                static member HPK_ :
                    (TradeProduct -> int) * (int -> TradeProduct -> TradeProduct) =
                    (fun tp -> tp.HPK) ,
                    (fun hpk tp -> { tp with HPK = hpk })


                static member Label_ :
                    (TradeProduct -> string) * (string -> TradeProduct -> TradeProduct) =
                    (fun tp -> tp.Label) ,
                    (fun lbl tp -> { tp with Label = lbl })


            module Optics =

                let setHPK = Optic.set TradeProduct.HPK_

                let getHPK = Optic.get TradeProduct.HPK_

                let setLabel = Optic.set TradeProduct.Label_

                let getLabel = Optic.get TradeProduct.Label_


            module Dto =


                type Dto () =
                    member val HPK = 0 with get, set
                    member val Label = "" with get, set


                let dto () = Dto ()


                let toDto (tp: TradeProduct) =
                    let dto = dto ()

                    dto.HPK <- tp.HPK
                    dto.Label <- tp.Label

                    dto


                let fromDto (dto: Dto) =
                    {   HPK = dto.HPK
                        Label = dto.Label
                    }



        module GenericProduct =


            type GenericProduct = { GPK : int; Label : string }

            let create gpk label = { GPK = gpk; Label = label }

            let apply f (x : GenericProduct) = x |> f

            let get = apply id

            let lablel gp = (gp |> get).Label

            let gpk gp = (gp |> get).GPK


            type GenericProduct with

                static member GPK_ :
                    (GenericProduct -> int) * (int -> GenericProduct -> GenericProduct) =
                    (fun tp -> tp.GPK) ,
                    (fun hpk tp -> { tp with GPK = hpk })


                static member Label_ :
                    (GenericProduct -> string) * (string -> GenericProduct -> GenericProduct) =
                    (fun tp -> tp.Label) ,
                    (fun lbl tp -> { tp with Label = lbl })


            module Optics =

                let setGPK = Optic.set GenericProduct.GPK_

                let getGPK = Optic.get GenericProduct.GPK_

                let setLabel = Optic.set GenericProduct.Label_

                let getLabel = Optic.get GenericProduct.Label_


            module Dto =


                type Dto () =
                    member val GPK = 0 with get, set
                    member val Label = "" with get, set


                let dto () = Dto ()


                let toDto (gp: GenericProduct) =
                    let dto = dto ()

                    dto.GPK <- gp.GPK
                    dto.Label <- gp.Label

                    dto


                let fromDto (dto: Dto) =
                    {   GPK = dto.GPK
                        Label = dto.Label
                    }



        type PatientDosage = PatientDosage.PatientDosage
        type TradeProduct = TradeProduct.TradeProduct
        type GenericProduct = GenericProduct.GenericProduct


        type ShapeDosage =
            {
                // Name of the shape the doserule applies to
                Shape : String list
                // TradeProducts the doserule applies to
                TradeProducts : TradeProduct list
                // GenericProducts the doserule applies to
                GenericProducts : GenericProduct list
                // Patients to wich the doserule applies to
                PatientDosages : PatientDosage list
            }

        let create shp gps tps =
            if shp |> List.exists String.isNullOrWhiteSpace then None
            else
                { Shape = shp; GenericProducts = gps; TradeProducts = tps; PatientDosages = [] }
                |> Some


        let genericProductLabel = GenericProduct.lablel


        let tradeProductLabel = TradeProduct.label


        type ShapeDosage with

            static member Shape_ :
                (ShapeDosage -> string list) * (string list -> ShapeDosage -> ShapeDosage) =
                (fun rd -> rd.Shape) ,
                (fun s rd -> { rd with Shape = s })

            static member TradeProducts_ :
                (ShapeDosage -> TradeProduct list) * (TradeProduct list -> ShapeDosage -> ShapeDosage) =
                (fun sd -> sd.TradeProducts) ,
                (fun tps sd -> { sd with TradeProducts = tps |> List.distinct })

            static member GenericProducts_ :
                (ShapeDosage -> GenericProduct list) * (GenericProduct list -> ShapeDosage -> ShapeDosage) =
                (fun sd -> sd.GenericProducts) ,
                (fun tps sd -> { sd with GenericProducts = tps |> List.distinct })

            static member PatientDosages_ :
                (ShapeDosage -> PatientDosage list) * (PatientDosage list -> ShapeDosage -> ShapeDosage) =
                (fun rd -> rd.PatientDosages) ,
                (fun pdl rd -> { rd with PatientDosages = pdl })


        module Optics =

            let setShape = Optic.set ShapeDosage.Shape_

            let getShape = Optic.get ShapeDosage.Shape_

            let setTradeProducts = Optic.set ShapeDosage.TradeProducts_

            let getTradeProducts = Optic.get ShapeDosage.TradeProducts_

            let setGenericProducts = Optic.set ShapeDosage.GenericProducts_

            let getGenericProducts = Optic.get ShapeDosage.GenericProducts_

            let setPatientDosages = Optic.set ShapeDosage.PatientDosages_

            let getPatientDosages = Optic.get ShapeDosage.PatientDosages_


        module Dto =


            type Dto () =
                member val Shape : string list = [] with get, set
                member val TradeProducts : TradeProduct.Dto.Dto list = [] with get, set
                member val GenericProducts : GenericProduct.Dto.Dto list = [] with get, set
                member val PatientDosages : PatientDosage.Dto.Dto list = [] with get, set


            let dto () = Dto ()

            let toDto (sd : ShapeDosage) =
                let dto = dto ()

                dto.Shape <- sd.Shape
                dto.GenericProducts <-
                    sd.GenericProducts
                    |> List.map GenericProduct.Dto.toDto
                dto.TradeProducts <-
                    sd.TradeProducts
                    |> List.map TradeProduct.Dto.toDto

                dto


            let fromDto (dto : Dto) =
                create [] [] []
                |> Option.bind ((Optics.setShape dto.Shape) >> Some)
                |> Option.bind
                    ((Optics.setGenericProducts (dto.GenericProducts
                                                 |> List.map GenericProduct.Dto.fromDto)) >> Some)
                |> Option.bind
                    ((Optics.setTradeProducts (dto.TradeProducts
                                                 |> List.map TradeProduct.Dto.fromDto)) >> Some)
                |> Option.bind
                    ((Optics.setPatientDosages (dto.PatientDosages
                                                |> List.map PatientDosage.Dto.fromDto
                                                |> List.filter Option.isSome
                                                |> List.map Option.get)) >> Some)



    module RouteDosage =

        type private ShapeDosage = ShapeDosage.ShapeDosage

        type RouteDosage =
            {
                // Administration route
                Route : string
                // The dosage rules per shape
                ShapeDosages : ShapeDosage list
            }

        let create rt =
            if rt |> String.isNullOrWhiteSpace then None
            else
                { Route = rt; ShapeDosages = [] }
                |> Some


        type RouteDosage with

            static member Route_ :
                (RouteDosage -> string) * (string -> RouteDosage -> RouteDosage) =
                (fun rd -> rd.Route) ,
                (fun s rd -> { rd with Route = s })

            static member ShapeDosages_ :
                (RouteDosage -> ShapeDosage list) * (ShapeDosage list -> RouteDosage -> RouteDosage) =
                (fun rd -> rd.ShapeDosages) ,
                (fun pdl rd -> { rd with ShapeDosages = pdl })


        module Optics =

            let setRoute = Optic.set RouteDosage.Route_

            let getRoute = Optic.get RouteDosage.Route_

            let setShapeDosages = Optic.set RouteDosage.ShapeDosages_

            let getShapeDosages = Optic.get RouteDosage.ShapeDosages_


        module Dto =

            type Dto () =
                member val Route = "" with get, set
                member val ShapeDosages : ShapeDosage.Dto.Dto list = [] with get, set


            let dto () = Dto ()


            let toDto (rd : RouteDosage) =
                let dto = dto ()

                dto.Route <- rd.Route
                dto.ShapeDosages <-
                    rd.ShapeDosages
                    |> List.map ShapeDosage.Dto.toDto

                dto


            let fromDto (dto : Dto) =
                create dto.Route
                |> Option.bind (Optics.setShapeDosages (dto.ShapeDosages
                                                        |> List.map ShapeDosage.Dto.fromDto
                                                        |> List.filter Option.isSome
                                                        |> List.map Option.get) >> Some)



    module IndicationDosage =

        type RouteDosage = RouteDosage.RouteDosage

        type IndicationDosage =
            {
                // The indication(-s) the dose rule applies to
                Indications : string list
                // The dosage rules per administration route
                RouteDosages : RouteDosage list
            }

        let create inds =
            { Indications = inds; RouteDosages = [] }


        type IndicationDosage with

            static member Indications_ :
                (IndicationDosage -> string list) * (string list -> IndicationDosage -> IndicationDosage) =
                (fun inds -> inds.Indications) ,
                (fun sl inds -> { inds with Indications = sl })

            static member RouteDosages_ :
                (IndicationDosage -> RouteDosage list) * (RouteDosage list -> IndicationDosage -> IndicationDosage) =
                (fun inds -> inds.RouteDosages) ,
                (fun rdl inds -> { inds with RouteDosages = rdl })


        module Optics =

            let setIndications = Optic.set IndicationDosage.Indications_

            let getIndications = Optic.get IndicationDosage.Indications_

            let setRouteDosages = Optic.set IndicationDosage.RouteDosages_

            let getRouteDosages = Optic.get IndicationDosage.RouteDosages_


        module Dto =

            type Dto () =
                member val Indications : string list = [] with get, set
                member val RouteDosages : RouteDosage.Dto.Dto list = [] with get, set


            let dto () = Dto ()


            let toDto (id : IndicationDosage) =
                let dto = dto ()

                dto.Indications <- id.Indications
                dto.RouteDosages <-
                    id.RouteDosages
                    |> List.map RouteDosage.Dto.toDto

                dto


            let fromDto (dto : Dto) =
                create []
                |> Optics.setRouteDosages (dto.RouteDosages
                                           |> List.map RouteDosage.Dto.fromDto
                                           |> List.filter Option.isSome
                                           |> List.map Option.get)


    type Dosage = Dosage.Dosage
    type MinMax = MinMax.MinMax
    type Patient = Patient.Patient
    type IndicationDosage = IndicationDosage.IndicationDosage


    /// Doserule
    type DoseRule =
        {
            // Generic the doserule applies to
            Generic : string
            // List of synonyms for the generic
            Synonyms : string list
            // The ATC code
            ATC : string
            // ATCTherapyGroup the doserule applies to
            ATCTherapyGroup : string
            // ATCTherapySubGroup the doserule applies to
            ATCTherapySubGroup : string
            // The generic group the doserule applies to
            GenericGroup : string
            // The generic subgroup the doserule applies to
            GenericSubGroup : string
            // The doserules per indication(-s)
            IndicationsDosages : IndicationDosage list
        }


    let apply f (dr : DoseRule) = f dr


    let get = apply id


    let create gen syn atc thg sub ggp gsg idl =
        {
            Generic = gen
            Synonyms = syn
            ATC = atc
            ATCTherapyGroup = thg
            ATCTherapySubGroup = sub
            GenericGroup = ggp
            GenericSubGroup = gsg
            IndicationsDosages = idl
        }


    let createIndicationDosage = IndicationDosage.create


    let createRouteDosage = RouteDosage.create


    let createShapeDosage = ShapeDosage.create


    let createPatientDosage = PatientDosage.create


    let createDosage n = Dosage.empty |> (Optic.set Dosage.Name_) n


    let createSubstanceDosage sn =
        if sn |> String.isNullOrWhiteSpace then None
        else sn |> createDosage |> Some


    let indxIndications inds (dr : DoseRule) =
        dr.IndicationsDosages
        |> List.tryFindIndex (fun id -> id.Indications = inds)


    let indxRoute inds rt dr =
        dr
        |> indxIndications inds
        |> Option.bind (fun ni ->
            match
                dr.IndicationsDosages.[ni].RouteDosages
                |> List.tryFindIndex (fun rd -> rd.Route = rt) with
            | None -> None
            | Some nr -> (ni, nr) |> Some
        )


    let indxShape inds rt shp dr =
        match dr |> indxRoute inds rt with
        | Some (ni, nr) ->
            match dr.IndicationsDosages.[ni].RouteDosages.[nr].ShapeDosages
                  |> List.tryFindIndex (fun sd -> sd.Shape = shp) with
            | Some ns -> (ni, nr, ns) |> Some
            | None -> None
        | None -> None


    let indxPatient inds rt shp pat dr =
        match dr |> indxShape inds rt shp with
        | Some (ni, nr, ns) ->
            match
                dr.IndicationsDosages.[ni].RouteDosages.[nr].ShapeDosages.[ns].PatientDosages
                |> List.tryFindIndex (fun rd -> rd.Patient = pat) with
            | Some np ->  (ni, nr, ns, np) |> Some
            | None -> None
        | None -> None


    let indxSubstance inds rt shp pat n dr =
        match dr |> indxPatient inds rt shp pat with
        | Some (ni, nr, ns, np) ->
            match dr.IndicationsDosages.[ni].RouteDosages.[nr].ShapeDosages.[np].PatientDosages.[ns].SubstanceDosages
                  |> List.tryFindIndex (fun sd -> sd.Name = n) with
            | Some n -> (ni, nr, np, ns, n) |> Some
            | None -> None
        | None -> None


    let addIndications inds (dr : DoseRule) =
        let indd = createIndicationDosage inds

        match dr |> indxIndications inds with
        | Some _ -> dr
        | None ->
            {
                dr with
                    IndicationsDosages =
                        dr.IndicationsDosages
                        |> List.prepend [ indd ]
            }


    let genericProductLabel = ShapeDosage.genericProductLabel


    let tradeProductLabel = ShapeDosage.tradeProductLabel


    type DoseRule with

        static member Generic_ :
            (DoseRule -> string) * (string -> DoseRule -> DoseRule) =
            (fun dr -> dr.Generic),
            (fun s dr -> { dr with Generic = s })

        static member Synonyms_ :
            (DoseRule -> string list) * (string list -> DoseRule -> DoseRule) =
            (fun dr -> dr.Synonyms) ,
            (fun sns dr -> { dr with Synonyms = sns |> List.distinct })


        static member IndicationDosages_ :
            (DoseRule -> IndicationDosage list) * (IndicationDosage list -> DoseRule -> DoseRule) =
            (fun dr -> dr.IndicationsDosages) ,
            (fun inds dr -> { dr with IndicationsDosages = inds })



    module Optics =

        module Patient = Patient.Optics
        module Dosage = Dosage.Optics

        type ShapeDosage = ShapeDosage.ShapeDosage
        type RouteDosage = RouteDosage.RouteDosage
        type PatientDosage = PatientDosage.PatientDosage


        let getGeneric = Optic.get DoseRule.Generic_


        let setGeneric = Optic.set DoseRule.Generic_


        let getSynonyms = Optic.get DoseRule.Synonyms_


        let setSynonyms = Optic.set DoseRule.Synonyms_


        let indDosDosagesLens n =
            DoseRule.IndicationDosages_ >-> List.pos_ n >?> IndicationDosage.RouteDosages_


        let getRouteDosages indd dr =
            match dr |> indxIndications indd with
            | Some n ->
                match dr |> Optic.get (indDosDosagesLens n) with
                | Some rtds -> rtds
                | None -> []
            | None -> []


        let addRoute inds rt dr =
            match rt |> createRouteDosage with
            | None -> dr
            | Some rtd ->
                match
                    dr |> indxIndications inds with
                    | Some n ->
                        match dr |> indxRoute inds rt with
                        | Some _ -> dr
                        | None ->
                            dr
                            |> Optic.set (indDosDosagesLens n) (dr |> getRouteDosages inds |> List.prepend [rtd])
                    | None -> dr


        let shapeDosagesPrism n1 n2 =
            indDosDosagesLens n1 >?> List.pos_ n2 >?> RouteDosage.ShapeDosages_


        let getShapeDosages inds rt dr =

            match dr |> indxRoute inds rt with
            | Some (ni, nr) ->
                match dr |> Optic.get (shapeDosagesPrism ni nr) with
                | Some pds -> pds
                | None -> []
            | None -> []


        let setShapeDosages inds rt pds dr =

            match dr |> indxRoute inds rt with
            | Some (ni, nr) ->
                dr
                |> Optic.set (shapeDosagesPrism ni nr) pds
            | None -> dr


        let addShape inds rt shp dr =
            match createShapeDosage shp [] [] with
            | None -> dr
            | Some shpd ->

                match dr |> indxShape inds rt shp with
                | Some _ -> dr
                | None ->
                    let pds =
                        dr
                        |> getShapeDosages inds rt
                        |> List.prepend [ shpd ]

                    dr
                    |> setShapeDosages inds rt pds


        let shapeDosagePrism n1 n2 n3 =
            shapeDosagesPrism n1 n2 >?> List.pos_ n3


        let inline private shapeDosageProductsGetter prism inds rt shp dr =
            match dr |> indxShape inds rt shp with
            | Some (ni, nr, ns) ->
                dr |> Optic.get ((shapeDosagePrism ni nr ns) >?> prism)
            | None -> None


        let inline private shapeDosageProductsSetter prism inds rt shp ps dr =
            match dr |> indxShape inds rt shp with
            | Some (ni, nr, ns) ->
                dr |> Optic.set ((shapeDosagePrism ni nr ns) >?> prism) ps
            | None -> dr


        let getGenericProducts = shapeDosageProductsGetter ShapeDosage.GenericProducts_


        let setGenericProducts = shapeDosageProductsSetter ShapeDosage.GenericProducts_


        let getTradeProducts = shapeDosageProductsGetter ShapeDosage.TradeProducts_


        let setTradeProducts = shapeDosageProductsSetter ShapeDosage.TradeProducts_


        let patientDosagesPrism n1 n2 n3 =
            shapeDosagePrism n1 n2 n3 >?> ShapeDosage.PatientDosages_


        let getPatientDosages inds rt shp dr =
            match dr |> indxShape inds rt shp with
            | Some (ni, nr, ns) ->
                match dr
                      |> Optic.get (patientDosagesPrism ni nr ns) with
                | Some sds -> sds
                | None -> []
            | None -> []


        let setPatientDosages inds rt shp pds dr =
            match dr |> indxShape inds rt shp with
            | Some (ni, nr, ns) ->
                        dr
                        |> Optic.set (patientDosagesPrism ni nr ns) pds
            | None -> dr


        let addPatient inds rt shp pat dr =
            match dr |> indxPatient inds rt shp pat with
            | Some _ -> dr
            | None ->
                let pds =
                    dr
                    |> getPatientDosages inds rt shp
                    |> List.prepend [ createPatientDosage pat ]

                dr
                |> setPatientDosages inds rt shp pds


        let patientDosagePrism n1 n2 n3 n4 =
            patientDosagesPrism n1 n2 n3 >?> List.pos_ n4


        let patientPrism n1 n2 n3 n4 =
            patientDosagePrism n1 n2 n3 n4 >?> PatientDosage.Patient_


        let private patientGetter prism inds rt shp pat dr =
            match dr |> indxPatient inds rt shp pat with
            | Some (ni, nr, ns, np) ->
                dr |> Optic.get ((patientPrism ni nr ns np) >?> prism)
            | None -> None


        let private patientSetter prism inds rt shp vu pat dr =
            match dr |> indxPatient inds rt shp pat with
            | Some (ni, nr, ns, np) ->
                let pat =
                    pat |> prism vu
                dr |> Optic.set ((patientPrism ni nr ns np)) pat, pat
            | None -> dr, pat


        let getPatientInclMinGestAge = patientGetter Patient.inclMinGestAge


        let setPatientInclMinGestAge = patientSetter Patient.setInclMinGestAge


        let getPatientExclMinGestAge = patientGetter Patient.exclMinGestAge


        let setPatientExclMinGestAge = patientSetter Patient.setExclMinGestAge


        let getPatientInclMaxGestAge = patientGetter Patient.inclMaxGestAge


        let setPatientInclMaxGestAge = patientSetter Patient.setInclMaxGestAge


        let getPatientExclMaxGestAge = patientGetter Patient.exclMaxGestAge


        let setPatientExclMaxGestAge = patientSetter Patient.setExclMaxGestAge


        let getPatientInclMinAge = patientGetter Patient.inclMinAge


        let setPatientInclMinAge = patientSetter Patient.setInclMinAge


        let getPatientExclMinAge = patientGetter Patient.exclMinAge


        let setPatientExclMinAge = patientSetter Patient.setExclMinAge


        let getPatientInclMaxAge = patientGetter Patient.inclMaxAge


        let setPatientInclMaxAge = patientSetter Patient.setInclMaxAge


        let getPatientExclMaxAge = patientGetter Patient.exclMaxAge


        let setPatientExclMaxAge = patientSetter Patient.setExclMaxAge


        let getPatientInclMinWeight = patientGetter Patient.inclMinWeight


        let setPatientInclMinWeight = patientSetter Patient.setInclMinWeight


        let getPatientExclMinWeight = patientGetter Patient.exclMinWeight


        let setPatientExclMinWeight = patientSetter Patient.setExclMinWeight


        let getPatientInclMaxWeight = patientGetter Patient.inclMaxWeight


        let setPatientInclMaxWeight = patientSetter Patient.setInclMaxWeight


        let getPatientExclMaxWeight = patientGetter Patient.exclMaxWeight


        let setPatientExclMaxWeight = patientSetter Patient.setExclMaxWeight


        let getPatientInclMinBSA = patientGetter Patient.inclMinBSA


        let setPatientInclMinBSA = patientSetter Patient.setInclMinBSA


        let getPatientExclMinBSA = patientGetter Patient.exclMinBSA


        let setPatientExclMinBSA = patientSetter Patient.setExclMinBSA


        let getPatientInclMaxBSA = patientGetter Patient.inclMaxBSA


        let setPatientInclMaxBSA = patientSetter Patient.setInclMaxBSA


        let getPatientExclMaxBSA = patientGetter Patient.exclMaxBSA


        let setPatientExclMaxBSA = patientSetter Patient.setExclMaxBSA

        // TODO : add gender setting


        let patientShapeDosagePrism n1 n2 n3 n4 =
            patientDosagePrism n1 n2 n3 n4 >?> PatientDosage.ShapeDosage_


        let inline private shapeDosageGetter prism inds rt shp pat dr =
            match dr |> indxPatient inds rt shp pat with
            | Some (ni, nr, ns, np) ->
                dr |> Optic.get ((patientShapeDosagePrism ni nr ns np) >?> prism)
            | None -> None


        let inline private shapeDosageSetter prism inds rt shp vu pat dr =
            match dr |> indxPatient inds rt shp pat with
            | Some (ni, nr, ns, np) ->
                dr |> Optic.set ((patientShapeDosagePrism ni nr ns np) >?> prism) vu
            | None -> dr


        let getFrequenciesShapeDosage = shapeDosageGetter Dosage.Optics.freqsFrequencyLens


        let setFrequenciesShapeDosage = shapeDosageSetter Dosage.Optics.freqsFrequencyLens


        let getInclMinNormStartShapeDosage = shapeDosageGetter Dosage.inclMinNormStartDosagePrism


        let setInclMinNormStartShapeDosage = shapeDosageSetter Dosage.inclMinNormStartDosagePrism


        let getExclMinNormStartShapeDosage = shapeDosageGetter Dosage.exclMinNormStartDosagePrism


        let setExclMinNormStartShapeDosage = shapeDosageSetter Dosage.exclMinNormStartDosagePrism


        let getInclMaxNormStartShapeDosage = shapeDosageGetter Dosage.inclMaxNormStartDosagePrism


        let setInclMaxNormStartShapeDosage = shapeDosageSetter Dosage.inclMaxNormStartDosagePrism


        let getExclMaxNormStartShapeDosage = shapeDosageGetter Dosage.exclMaxNormStartDosagePrism


        let setExclMaxNormStartShapeDosage = shapeDosageSetter Dosage.exclMaxNormStartDosagePrism


        let getInclMinNormWeightStartShapeDosage = shapeDosageGetter Dosage.inclMinNormWeightStartDosagePrism


        let setInclMinNormWeightStartShapeDosage = shapeDosageSetter Dosage.inclMinNormWeightStartDosagePrism


        let getExclMinNormWeightStartShapeDosage = shapeDosageGetter Dosage.exclMinNormWeightStartDosagePrism


        let setExclMinNormWeightStartShapeDosage = shapeDosageSetter Dosage.exclMinNormWeightStartDosagePrism


        let getInclMaxNormWeightStartShapeDosage = shapeDosageGetter Dosage.inclMaxNormWeightStartDosagePrism


        let setInclMaxNormWeightStartShapeDosage = shapeDosageSetter Dosage.inclMaxNormWeightStartDosagePrism


        let getExclMaxNormWeightStartShapeDosage = shapeDosageGetter Dosage.exclMaxNormWeightStartDosagePrism


        let setExclMaxNormWeightStartShapeDosage = shapeDosageSetter Dosage.exclMaxNormWeightStartDosagePrism


        let getInclMinNormBSAStartShapeDosage = shapeDosageGetter Dosage.inclMinNormBSAStartDosagePrism


        let setInclMinNormBSAStartShapeDosage = shapeDosageSetter Dosage.inclMinNormBSAStartDosagePrism


        let getExclMinNormBSAStartShapeDosage = shapeDosageGetter Dosage.exclMinNormBSAStartDosagePrism


        let setExclMinNormBSAStartShapeDosage = shapeDosageSetter Dosage.exclMinNormBSAStartDosagePrism


        let getInclMaxNormBSAStartShapeDosage = shapeDosageGetter Dosage.inclMaxNormBSAStartDosagePrism


        let setInclMaxNormBSAStartShapeDosage = shapeDosageSetter Dosage.inclMaxNormBSAStartDosagePrism


        let getExclMaxNormBSAStartShapeDosage = shapeDosageGetter Dosage.exclMaxNormBSAStartDosagePrism


        let setExclMaxNormBSAStartShapeDosage = shapeDosageSetter Dosage.exclMaxNormBSAStartDosagePrism


        let getInclMinAbsStartShapeDosage = shapeDosageGetter Dosage.inclMinAbsStartDosagePrism


        let setInclMinAbsStartShapeDosage = shapeDosageSetter Dosage.inclMinAbsStartDosagePrism


        let getExclMinAbsStartShapeDosage = shapeDosageGetter Dosage.exclMinAbsStartDosagePrism


        let setExclMinAbsStartShapeDosage = shapeDosageSetter Dosage.exclMinAbsStartDosagePrism


        let getInclMaxAbsStartShapeDosage = shapeDosageGetter Dosage.inclMaxAbsStartDosagePrism


        let setInclMaxAbsStartShapeDosage = shapeDosageSetter Dosage.inclMaxAbsStartDosagePrism


        let getExclMaxAbsStartShapeDosage = shapeDosageGetter Dosage.exclMaxAbsStartDosagePrism


        let setExclMaxAbsStartShapeDosage = shapeDosageSetter Dosage.exclMaxAbsStartDosagePrism


        let getInclMinAbsWeightStartShapeDosage = shapeDosageGetter Dosage.inclMinAbsWeightStartDosagePrism


        let setInclMinAbsWeightStartShapeDosage = shapeDosageSetter Dosage.inclMinAbsWeightStartDosagePrism


        let getExclMinAbsWeightStartShapeDosage = shapeDosageGetter Dosage.exclMinAbsWeightStartDosagePrism


        let setExclMinAbsWeightStartShapeDosage = shapeDosageSetter Dosage.exclMinAbsWeightStartDosagePrism


        let getInclMaxAbsWeightStartShapeDosage = shapeDosageGetter Dosage.inclMaxAbsWeightStartDosagePrism


        let setInclMaxAbsWeightStartShapeDosage = shapeDosageSetter Dosage.inclMaxAbsWeightStartDosagePrism


        let getExclMaxAbsWeightStartShapeDosage = shapeDosageGetter Dosage.exclMaxAbsWeightStartDosagePrism


        let setExclMaxAbsWeightStartShapeDosage = shapeDosageSetter Dosage.exclMaxAbsWeightStartDosagePrism


        let getInclMinAbsBSAStartShapeDosage = shapeDosageGetter Dosage.inclMinAbsBSAStartDosagePrism


        let setInclMinAbsBSAStartShapeDosage = shapeDosageSetter Dosage.inclMinAbsBSAStartDosagePrism


        let getExclMinAbsBSAStartShapeDosage = shapeDosageGetter Dosage.exclMinAbsBSAStartDosagePrism


        let setExclMinAbsBSAStartShapeDosage = shapeDosageSetter Dosage.exclMinAbsBSAStartDosagePrism


        let getInclMaxAbsBSAStartShapeDosage = shapeDosageGetter Dosage.inclMaxAbsBSAStartDosagePrism


        let setInclMaxAbsBSAStartShapeDosage = shapeDosageSetter Dosage.inclMaxAbsBSAStartDosagePrism


        let getExclMaxAbsBSAStartShapeDosage = shapeDosageGetter Dosage.exclMaxAbsBSAStartDosagePrism


        let setExclMaxAbsBSAStartShapeDosage = shapeDosageSetter Dosage.exclMaxAbsBSAStartDosagePrism


        let getInclMinNormSingleShapeDosage = shapeDosageGetter Dosage.inclMinNormSingleDosagePrism


        let setInclMinNormSingleShapeDosage = shapeDosageSetter Dosage.inclMinNormSingleDosagePrism


        let getExclMinNormSingleShapeDosage = shapeDosageGetter Dosage.exclMinNormSingleDosagePrism


        let setExclMinNormSingleShapeDosage = shapeDosageSetter Dosage.exclMinNormSingleDosagePrism


        let getInclMaxNormSingleShapeDosage = shapeDosageGetter Dosage.inclMaxNormSingleDosagePrism


        let setInclMaxNormSingleShapeDosage = shapeDosageSetter Dosage.inclMaxNormSingleDosagePrism


        let getExclMaxNormSingleShapeDosage = shapeDosageGetter Dosage.exclMaxNormSingleDosagePrism


        let setExclMaxNormSingleShapeDosage = shapeDosageSetter Dosage.exclMaxNormSingleDosagePrism


        let getInclMinNormWeightSingleShapeDosage = shapeDosageGetter Dosage.inclMinNormWeightSingleDosagePrism


        let setInclMinNormWeightSingleShapeDosage = shapeDosageSetter Dosage.inclMinNormWeightSingleDosagePrism


        let getExclMinNormWeightSingleShapeDosage = shapeDosageGetter Dosage.exclMinNormWeightSingleDosagePrism


        let setExclMinNormWeightSingleShapeDosage = shapeDosageSetter Dosage.exclMinNormWeightSingleDosagePrism


        let getInclMaxNormWeightSingleShapeDosage = shapeDosageGetter Dosage.inclMaxNormWeightSingleDosagePrism


        let setInclMaxNormWeightSingleShapeDosage = shapeDosageSetter Dosage.inclMaxNormWeightSingleDosagePrism


        let getExclMaxNormWeightSingleShapeDosage = shapeDosageGetter Dosage.exclMaxNormWeightSingleDosagePrism


        let setExclMaxNormWeightSingleShapeDosage = shapeDosageSetter Dosage.exclMaxNormWeightSingleDosagePrism


        let getInclMinNormBSASingleShapeDosage = shapeDosageGetter Dosage.inclMinNormBSASingleDosagePrism


        let setInclMinNormBSASingleShapeDosage = shapeDosageSetter Dosage.inclMinNormBSASingleDosagePrism


        let getExclMinNormBSASingleShapeDosage = shapeDosageGetter Dosage.exclMinNormBSASingleDosagePrism


        let setExclMinNormBSASingleShapeDosage = shapeDosageSetter Dosage.exclMinNormBSASingleDosagePrism


        let getInclMaxNormBSASingleShapeDosage = shapeDosageGetter Dosage.inclMaxNormBSASingleDosagePrism


        let setInclMaxNormBSASingleShapeDosage = shapeDosageSetter Dosage.inclMaxNormBSASingleDosagePrism


        let getExclMaxNormBSASingleShapeDosage = shapeDosageGetter Dosage.exclMaxNormBSASingleDosagePrism


        let setExclMaxNormBSASingleShapeDosage = shapeDosageSetter Dosage.exclMaxNormBSASingleDosagePrism


        let getInclMinAbsSingleShapeDosage = shapeDosageGetter Dosage.inclMinAbsSingleDosagePrism


        let setInclMinAbsSingleShapeDosage = shapeDosageSetter Dosage.inclMinAbsSingleDosagePrism


        let getExclMinAbsSingleShapeDosage = shapeDosageGetter Dosage.exclMinAbsSingleDosagePrism


        let setExclMinAbsSingleShapeDosage = shapeDosageSetter Dosage.exclMinAbsSingleDosagePrism


        let getInclMaxAbsSingleShapeDosage = shapeDosageGetter Dosage.inclMaxAbsSingleDosagePrism


        let setInclMaxAbsSingleShapeDosage = shapeDosageSetter Dosage.inclMaxAbsSingleDosagePrism


        let getExclMaxAbsSingleShapeDosage = shapeDosageGetter Dosage.exclMaxAbsSingleDosagePrism


        let setExclMaxAbsSingleShapeDosage = shapeDosageSetter Dosage.exclMaxAbsSingleDosagePrism


        let getInclMinAbsWeightSingleShapeDosage = shapeDosageGetter Dosage.inclMinAbsWeightSingleDosagePrism


        let setInclMinAbsWeightSingleShapeDosage = shapeDosageSetter Dosage.inclMinAbsWeightSingleDosagePrism


        let getExclMinAbsWeightSingleShapeDosage = shapeDosageGetter Dosage.exclMinAbsWeightSingleDosagePrism


        let setExclMinAbsWeightSingleShapeDosage = shapeDosageSetter Dosage.exclMinAbsWeightSingleDosagePrism


        let getInclMaxAbsWeightSingleShapeDosage = shapeDosageGetter Dosage.inclMaxAbsWeightSingleDosagePrism


        let setInclMaxAbsWeightSingleShapeDosage = shapeDosageSetter Dosage.inclMaxAbsWeightSingleDosagePrism


        let getExclMaxAbsWeightSingleShapeDosage = shapeDosageGetter Dosage.exclMaxAbsWeightSingleDosagePrism


        let setExclMaxAbsWeightSingleShapeDosage = shapeDosageSetter Dosage.exclMaxAbsWeightSingleDosagePrism


        let getInclMinAbsBSASingleShapeDosage = shapeDosageGetter Dosage.inclMinAbsBSASingleDosagePrism


        let setInclMinAbsBSASingleShapeDosage = shapeDosageSetter Dosage.inclMinAbsBSASingleDosagePrism


        let getExclMinAbsBSASingleShapeDosage = shapeDosageGetter Dosage.exclMinAbsBSASingleDosagePrism


        let setExclMinAbsBSASingleShapeDosage = shapeDosageSetter Dosage.exclMinAbsBSASingleDosagePrism


        let getInclMaxAbsBSASingleShapeDosage = shapeDosageGetter Dosage.inclMaxAbsBSASingleDosagePrism


        let setInclMaxAbsBSASingleShapeDosage = shapeDosageSetter Dosage.inclMaxAbsBSASingleDosagePrism


        let getExclMaxAbsBSASingleShapeDosage = shapeDosageGetter Dosage.exclMaxAbsBSASingleDosagePrism


        let setExclMaxAbsBSASingleShapeDosage = shapeDosageSetter Dosage.exclMaxAbsBSASingleDosagePrism


        let getInclMinNormRateShapeDosage = shapeDosageGetter Dosage.inclMinNormRateDosagePrism


        let setInclMinNormRateShapeDosage = shapeDosageSetter Dosage.inclMinNormRateDosagePrism


        let getExclMinNormRateShapeDosage = shapeDosageGetter Dosage.exclMinNormRateDosagePrism


        let setExclMinNormRateShapeDosage = shapeDosageSetter Dosage.exclMinNormRateDosagePrism


        let getInclMaxNormRateShapeDosage = shapeDosageGetter Dosage.inclMaxNormRateDosagePrism


        let setInclMaxNormRateShapeDosage = shapeDosageSetter Dosage.inclMaxNormRateDosagePrism


        let getExclMaxNormRateShapeDosage = shapeDosageGetter Dosage.exclMaxNormRateDosagePrism


        let setExclMaxNormRateShapeDosage = shapeDosageSetter Dosage.exclMaxNormRateDosagePrism


        let getInclMinNormWeightRateShapeDosage = shapeDosageGetter Dosage.inclMinNormWeightRateDosagePrism


        let setInclMinNormWeightRateShapeDosage = shapeDosageSetter Dosage.inclMinNormWeightRateDosagePrism


        let getExclMinNormWeightRateShapeDosage = shapeDosageGetter Dosage.exclMinNormWeightRateDosagePrism


        let setExclMinNormWeightRateShapeDosage = shapeDosageSetter Dosage.exclMinNormWeightRateDosagePrism


        let getInclMaxNormWeightRateShapeDosage = shapeDosageGetter Dosage.inclMaxNormWeightRateDosagePrism


        let setInclMaxNormWeightRateShapeDosage = shapeDosageSetter Dosage.inclMaxNormWeightRateDosagePrism


        let getExclMaxNormWeightRateShapeDosage = shapeDosageGetter Dosage.exclMaxNormWeightRateDosagePrism


        let setExclMaxNormWeightRateShapeDosage = shapeDosageSetter Dosage.exclMaxNormWeightRateDosagePrism


        let getInclMinNormBSARateShapeDosage = shapeDosageGetter Dosage.inclMinNormBSARateDosagePrism


        let setInclMinNormBSARateShapeDosage = shapeDosageSetter Dosage.inclMinNormBSARateDosagePrism


        let getExclMinNormBSARateShapeDosage = shapeDosageGetter Dosage.exclMinNormBSARateDosagePrism


        let setExclMinNormBSARateShapeDosage = shapeDosageSetter Dosage.exclMinNormBSARateDosagePrism


        let getInclMaxNormBSARateShapeDosage = shapeDosageGetter Dosage.inclMaxNormBSARateDosagePrism


        let setInclMaxNormBSARateShapeDosage = shapeDosageSetter Dosage.inclMaxNormBSARateDosagePrism


        let getExclMaxNormBSARateShapeDosage = shapeDosageGetter Dosage.exclMaxNormBSARateDosagePrism


        let setExclMaxNormBSARateShapeDosage = shapeDosageSetter Dosage.exclMaxNormBSARateDosagePrism


        let getInclMinAbsRateShapeDosage = shapeDosageGetter Dosage.inclMinAbsRateDosagePrism


        let setInclMinAbsRateShapeDosage = shapeDosageSetter Dosage.inclMinAbsRateDosagePrism


        let getExclMinAbsRateShapeDosage = shapeDosageGetter Dosage.exclMinAbsRateDosagePrism


        let setExclMinAbsRateShapeDosage = shapeDosageSetter Dosage.exclMinAbsRateDosagePrism


        let getInclMaxAbsRateShapeDosage = shapeDosageGetter Dosage.inclMaxAbsRateDosagePrism


        let setInclMaxAbsRateShapeDosage = shapeDosageSetter Dosage.inclMaxAbsRateDosagePrism


        let getExclMaxAbsRateShapeDosage = shapeDosageGetter Dosage.exclMaxAbsRateDosagePrism


        let setExclMaxAbsRateShapeDosage = shapeDosageSetter Dosage.exclMaxAbsRateDosagePrism


        let getInclMinAbsWeightRateShapeDosage = shapeDosageGetter Dosage.inclMinAbsWeightRateDosagePrism


        let setInclMinAbsWeightRateShapeDosage = shapeDosageSetter Dosage.inclMinAbsWeightRateDosagePrism


        let getExclMinAbsWeightRateShapeDosage = shapeDosageGetter Dosage.exclMinAbsWeightRateDosagePrism


        let setExclMinAbsWeightRateShapeDosage = shapeDosageSetter Dosage.exclMinAbsWeightRateDosagePrism


        let getInclMaxAbsWeightRateShapeDosage = shapeDosageGetter Dosage.inclMaxAbsWeightRateDosagePrism


        let setInclMaxAbsWeightRateShapeDosage = shapeDosageSetter Dosage.inclMaxAbsWeightRateDosagePrism


        let getExclMaxAbsWeightRateShapeDosage = shapeDosageGetter Dosage.exclMaxAbsWeightRateDosagePrism


        let setExclMaxAbsWeightRateShapeDosage = shapeDosageSetter Dosage.exclMaxAbsWeightRateDosagePrism


        let getInclMinAbsBSARateShapeDosage = shapeDosageGetter Dosage.inclMinAbsBSARateDosagePrism


        let setInclMinAbsBSARateShapeDosage = shapeDosageSetter Dosage.inclMinAbsBSARateDosagePrism


        let getExclMinAbsBSARateShapeDosage = shapeDosageGetter Dosage.exclMinAbsBSARateDosagePrism


        let setExclMinAbsBSARateShapeDosage = shapeDosageSetter Dosage.exclMinAbsBSARateDosagePrism


        let getInclMaxAbsBSARateShapeDosage = shapeDosageGetter Dosage.inclMaxAbsBSARateDosagePrism


        let setInclMaxAbsBSARateShapeDosage = shapeDosageSetter Dosage.inclMaxAbsBSARateDosagePrism


        let getExclMaxAbsBSARateShapeDosage = shapeDosageGetter Dosage.exclMaxAbsBSARateDosagePrism


        let setExclMaxAbsBSARateShapeDosage = shapeDosageSetter Dosage.exclMaxAbsBSARateDosagePrism


        let getInclMinNormTotalShapeDosage = shapeDosageGetter Dosage.inclMinNormTotalDosagePrism


        let setInclMinNormTotalShapeDosage = shapeDosageSetter Dosage.inclMinNormTotalDosagePrism


        let getExclMinNormTotalShapeDosage = shapeDosageGetter Dosage.exclMinNormTotalDosagePrism


        let setExclMinNormTotalShapeDosage = shapeDosageSetter Dosage.exclMinNormTotalDosagePrism


        let getInclMaxNormTotalShapeDosage = shapeDosageGetter Dosage.inclMaxNormTotalDosagePrism


        let setInclMaxNormTotalShapeDosage = shapeDosageSetter Dosage.inclMaxNormTotalDosagePrism


        let getExclMaxNormTotalShapeDosage = shapeDosageGetter Dosage.exclMaxNormTotalDosagePrism


        let setExclMaxNormTotalShapeDosage = shapeDosageSetter Dosage.exclMaxNormTotalDosagePrism


        let getInclMinNormWeightTotalShapeDosage = shapeDosageGetter Dosage.inclMinNormWeightTotalDosagePrism


        let setInclMinNormWeightTotalShapeDosage = shapeDosageSetter Dosage.inclMinNormWeightTotalDosagePrism


        let getExclMinNormWeightTotalShapeDosage = shapeDosageGetter Dosage.exclMinNormWeightTotalDosagePrism


        let setExclMinNormWeightTotalShapeDosage = shapeDosageSetter Dosage.exclMinNormWeightTotalDosagePrism


        let getInclMaxNormWeightTotalShapeDosage = shapeDosageGetter Dosage.inclMaxNormWeightTotalDosagePrism


        let setInclMaxNormWeightTotalShapeDosage = shapeDosageSetter Dosage.inclMaxNormWeightTotalDosagePrism


        let getExclMaxNormWeightTotalShapeDosage = shapeDosageGetter Dosage.exclMaxNormWeightTotalDosagePrism


        let setExclMaxNormWeightTotalShapeDosage = shapeDosageSetter Dosage.exclMaxNormWeightTotalDosagePrism


        let getInclMinNormBSATotalShapeDosage = shapeDosageGetter Dosage.inclMinNormBSATotalDosagePrism


        let setInclMinNormBSATotalShapeDosage = shapeDosageSetter Dosage.inclMinNormBSATotalDosagePrism


        let getExclMinNormBSATotalShapeDosage = shapeDosageGetter Dosage.exclMinNormBSATotalDosagePrism


        let setExclMinNormBSATotalShapeDosage = shapeDosageSetter Dosage.exclMinNormBSATotalDosagePrism


        let getInclMaxNormBSATotalShapeDosage = shapeDosageGetter Dosage.inclMaxNormBSATotalDosagePrism


        let setInclMaxNormBSATotalShapeDosage = shapeDosageSetter Dosage.inclMaxNormBSATotalDosagePrism


        let getExclMaxNormBSATotalShapeDosage = shapeDosageGetter Dosage.exclMaxNormBSATotalDosagePrism


        let setExclMaxNormBSATotalShapeDosage = shapeDosageSetter Dosage.exclMaxNormBSATotalDosagePrism


        let getInclMinAbsTotalShapeDosage = shapeDosageGetter Dosage.inclMinAbsTotalDosagePrism


        let setInclMinAbsTotalShapeDosage = shapeDosageSetter Dosage.inclMinAbsTotalDosagePrism


        let getExclMinAbsTotalShapeDosage = shapeDosageGetter Dosage.exclMinAbsTotalDosagePrism


        let setExclMinAbsTotalShapeDosage = shapeDosageSetter Dosage.exclMinAbsTotalDosagePrism


        let getInclMaxAbsTotalShapeDosage = shapeDosageGetter Dosage.inclMaxAbsTotalDosagePrism


        let setInclMaxAbsTotalShapeDosage = shapeDosageSetter Dosage.inclMaxAbsTotalDosagePrism


        let getExclMaxAbsTotalShapeDosage = shapeDosageGetter Dosage.exclMaxAbsTotalDosagePrism


        let setExclMaxAbsTotalShapeDosage = shapeDosageSetter Dosage.exclMaxAbsTotalDosagePrism


        let getInclMinAbsWeightTotalShapeDosage = shapeDosageGetter Dosage.inclMinAbsWeightTotalDosagePrism


        let setInclMinAbsWeightTotalShapeDosage = shapeDosageSetter Dosage.inclMinAbsWeightTotalDosagePrism


        let getExclMinAbsWeightTotalShapeDosage = shapeDosageGetter Dosage.exclMinAbsWeightTotalDosagePrism


        let setExclMinAbsWeightTotalShapeDosage = shapeDosageSetter Dosage.exclMinAbsWeightTotalDosagePrism


        let getInclMaxAbsWeightTotalShapeDosage = shapeDosageGetter Dosage.inclMaxAbsWeightTotalDosagePrism


        let setInclMaxAbsWeightTotalShapeDosage = shapeDosageSetter Dosage.inclMaxAbsWeightTotalDosagePrism


        let getExclMaxAbsWeightTotalShapeDosage = shapeDosageGetter Dosage.exclMaxAbsWeightTotalDosagePrism


        let setExclMaxAbsWeightTotalShapeDosage = shapeDosageSetter Dosage.exclMaxAbsWeightTotalDosagePrism


        let getInclMinAbsBSATotalShapeDosage = shapeDosageGetter Dosage.inclMinAbsBSATotalDosagePrism


        let setInclMinAbsBSATotalShapeDosage = shapeDosageSetter Dosage.inclMinAbsBSATotalDosagePrism


        let getExclMinAbsBSATotalShapeDosage = shapeDosageGetter Dosage.exclMinAbsBSATotalDosagePrism


        let setExclMinAbsBSATotalShapeDosage = shapeDosageSetter Dosage.exclMinAbsBSATotalDosagePrism


        let getInclMaxAbsBSATotalShapeDosage = shapeDosageGetter Dosage.inclMaxAbsBSATotalDosagePrism


        let setInclMaxAbsBSATotalShapeDosage = shapeDosageSetter Dosage.inclMaxAbsBSATotalDosagePrism


        let getExclMaxAbsBSATotalShapeDosage = shapeDosageGetter Dosage.exclMaxAbsBSATotalDosagePrism


        let setExclMaxAbsBSATotalShapeDosage = shapeDosageSetter Dosage.exclMaxAbsBSATotalDosagePrism


        let substanceDosagesPrism n1 n2 n3 n4 =
            patientDosagePrism n1 n2 n3 n4 >?> PatientDosage.SubstanceDosages_


        let getSubstanceDosages inds rt shp pat dr =
            match dr |> indxPatient inds rt shp pat with
            | Some (ni, nr, np, ns) ->
                match dr |> Optic.get (substanceDosagesPrism ni nr np ns) with
                | Some sds -> sds
                | None -> []
            | None -> []


        let setSubstanceDosages inds rt shp pat sds dr =
            match dr |> indxPatient inds rt shp pat with
            | Some (ni, nr, np, ns) ->
                dr
                |> Optic.set (substanceDosagesPrism ni nr np ns) sds
            | None -> dr


        let addSubstance inds rt shp pat sn dr  =
            match sn |> createSubstanceDosage with
            | None -> dr
            | Some sds ->
                match dr |> indxSubstance inds rt shp pat sn with
                | Some _ -> dr
                | None ->
                    let sds =
                        dr
                        |> getSubstanceDosages inds rt shp pat
                        |> List.prepend [sds]

                    dr
                    |> setSubstanceDosages inds rt shp pat sds


        let substanceDosagePrism n1 n2 n3 n4 n5 =
            substanceDosagesPrism n1 n2 n3 n4  >?> List.pos_ n5


        let inline private substanceDosageGetter prism inds rt shp pat sn dr =
            match dr |> indxSubstance inds rt shp pat sn with
            | Some (ni, nr, np, ns, n) ->
                dr |> Optic.get ((substanceDosagePrism ni nr np ns n) >?> prism)
            | None -> None


        let inline private substanceDosageSetter prism inds rt shp pat sn vu dr =
            match dr |> indxSubstance inds rt shp pat sn with
            | Some (ni, nr, np, ns, n) ->
                dr |> Optic.set ((substanceDosagePrism ni nr np ns n) >?> prism) vu
            | None -> dr


        let getRateUnitSubstanceDosage = substanceDosageGetter Dosage.rateUnitRateDosagePrism


        let setRateUnitSubstanceDosage = substanceDosageSetter Dosage.rateUnitRateDosagePrism


        let getTimeUnitSubstanceDosage = substanceDosageGetter Dosage.timeUnitTotalDosagePrism


        let setTimeUnitSubstanceDosage = substanceDosageSetter Dosage.timeUnitTotalDosagePrism


        let getNormWeightUnitStartSubstanceDosage = substanceDosageGetter Dosage.normWeightUnitStartDosagePrism


        let setNormWeightUnitStartSubstanceDosage = substanceDosageSetter Dosage.normWeightUnitStartDosagePrism


        let getNormBSAUnitStartSubstanceDosage = substanceDosageGetter Dosage.normBSAUnitStartDosagePrism


        let setNormBSAUnitStartSubstanceDosage = substanceDosageSetter Dosage.normBSAUnitStartDosagePrism


        let getNormWeightUnitSingleSubstanceDosage = substanceDosageGetter Dosage.normWeightUnitSingleDosagePrism


        let setNormWeightUnitSingleSubstanceDosage = substanceDosageSetter Dosage.normWeightUnitSingleDosagePrism


        let getNormBSAUnitSingleSubstanceDosage = substanceDosageGetter Dosage.normBSAUnitSingleDosagePrism


        let setNormBSAUnitSingleSubstanceDosage = substanceDosageSetter Dosage.normBSAUnitSingleDosagePrism


        let getNormWeightUnitRateSubstanceDosage = substanceDosageGetter Dosage.normWeightUnitRateDosagePrism


        let setNormWeightUnitRateSubstanceDosage = substanceDosageSetter Dosage.normWeightUnitRateDosagePrism


        let getNormBSAUnitRateSubstanceDosage = substanceDosageGetter Dosage.normBSAUnitRateDosagePrism


        let setNormBSAUnitRateSubstanceDosage = substanceDosageSetter Dosage.normBSAUnitRateDosagePrism


        let getNormWeightUnitTotalSubstanceDosage = substanceDosageGetter Dosage.normWeightUnitTotalDosagePrism


        let setNormWeightUnitTotalSubstanceDosage = substanceDosageSetter Dosage.normWeightUnitTotalDosagePrism


        let getNormBSAUnitTotalSubstanceDosage = substanceDosageGetter Dosage.normBSAUnitTotalDosagePrism


        let setNormBSAUnitTotalSubstanceDosage = substanceDosageSetter Dosage.normBSAUnitTotalDosagePrism


        let getAbsWeightUnitStartSubstanceDosage = substanceDosageGetter Dosage.absWeightUnitStartDosagePrism


        let setAbsWeightUnitStartSubstanceDosage = substanceDosageSetter Dosage.absWeightUnitStartDosagePrism


        let getAbsBSAUnitStartSubstanceDosage = substanceDosageGetter Dosage.absBSAUnitStartDosagePrism


        let setAbsBSAUnitStartSubstanceDosage = substanceDosageSetter Dosage.absBSAUnitStartDosagePrism


        let getAbsWeightUnitSingleSubstanceDosage = substanceDosageGetter Dosage.absWeightUnitSingleDosagePrism


        let setAbsWeightUnitSingleSubstanceDosage = substanceDosageSetter Dosage.absWeightUnitSingleDosagePrism


        let getAbsBSAUnitSingleSubstanceDosage = substanceDosageGetter Dosage.absBSAUnitSingleDosagePrism


        let setAbsBSAUnitSingleSubstanceDosage = substanceDosageSetter Dosage.absBSAUnitSingleDosagePrism


        let getAbsWeightUnitRateSubstanceDosage = substanceDosageGetter Dosage.absWeightUnitRateDosagePrism


        let setAbsWeightUnitRateSubstanceDosage = substanceDosageSetter Dosage.absWeightUnitRateDosagePrism


        let getAbsBSAUnitRateSubstanceDosage = substanceDosageGetter Dosage.absBSAUnitRateDosagePrism


        let setAbsBSAUnitRateSubstanceDosage = substanceDosageSetter Dosage.absBSAUnitRateDosagePrism


        let getAbsWeightUnitTotalSubstanceDosage = substanceDosageGetter Dosage.absWeightUnitTotalDosagePrism


        let setAbsWeightUnitTotalSubstanceDosage = substanceDosageSetter Dosage.absWeightUnitTotalDosagePrism


        let getAbsBSAUnitTotalSubstanceDosage = substanceDosageGetter Dosage.absBSAUnitTotalDosagePrism


        let setAbsBSAUnitTotalSubstanceDosage = substanceDosageSetter Dosage.absBSAUnitTotalDosagePrism


        let getFreqsFrequencySubstanceDosage = substanceDosageGetter Dosage.freqsFrequencyLens


        let setFreqsFrequencySubstanceDosage = substanceDosageSetter Dosage.freqsFrequencyLens


        let getTimeUnitFrequencySubstanceDosage = substanceDosageGetter Dosage.timeUnitFrequencyLens


        let setTimeUnitFrequencySubstanceDosage = substanceDosageSetter Dosage.timeUnitFrequencyLens


        let getMinIntervalFrequencySubstanceDosage = substanceDosageGetter Dosage.minIntervalValueFrequencyLens


        let setMinIntervalFrequencySubstanceDosage = substanceDosageSetter Dosage.minIntervalValueFrequencyLens


        let getInclMinNormStartSubstanceDosage = substanceDosageGetter Dosage.inclMinNormStartDosagePrism


        let setInclMinNormStartSubstanceDosage = substanceDosageSetter Dosage.inclMinNormStartDosagePrism


        let getExclMinNormStartSubstanceDosage = substanceDosageGetter Dosage.exclMinNormStartDosagePrism


        let setExclMinNormStartSubstanceDosage = substanceDosageSetter Dosage.exclMinNormStartDosagePrism


        let getInclMaxNormStartSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormStartDosagePrism


        let setInclMaxNormStartSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormStartDosagePrism


        let getExclMaxNormStartSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormStartDosagePrism


        let setExclMaxNormStartSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormStartDosagePrism


        let getInclMinNormWeightStartSubstanceDosage = substanceDosageGetter Dosage.inclMinNormWeightStartDosagePrism


        let setInclMinNormWeightStartSubstanceDosage = substanceDosageSetter Dosage.inclMinNormWeightStartDosagePrism


        let getExclMinNormWeightStartSubstanceDosage = substanceDosageGetter Dosage.exclMinNormWeightStartDosagePrism


        let setExclMinNormWeightStartSubstanceDosage = substanceDosageSetter Dosage.exclMinNormWeightStartDosagePrism


        let getInclMaxNormWeightStartSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormWeightStartDosagePrism


        let setInclMaxNormWeightStartSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormWeightStartDosagePrism


        let getExclMaxNormWeightStartSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormWeightStartDosagePrism


        let setExclMaxNormWeightStartSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormWeightStartDosagePrism


        let getInclMinNormBSAStartSubstanceDosage = substanceDosageGetter Dosage.inclMinNormBSAStartDosagePrism


        let setInclMinNormBSAStartSubstanceDosage = substanceDosageSetter Dosage.inclMinNormBSAStartDosagePrism


        let getExclMinNormBSAStartSubstanceDosage = substanceDosageGetter Dosage.exclMinNormBSAStartDosagePrism


        let setExclMinNormBSAStartSubstanceDosage = substanceDosageSetter Dosage.exclMinNormBSAStartDosagePrism


        let getInclMaxNormBSAStartSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormBSAStartDosagePrism


        let setInclMaxNormBSAStartSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormBSAStartDosagePrism


        let getExclMaxNormBSAStartSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormBSAStartDosagePrism


        let setExclMaxNormBSAStartSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormBSAStartDosagePrism


        let getInclMinAbsStartSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsStartDosagePrism


        let setInclMinAbsStartSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsStartDosagePrism


        let getExclMinAbsStartSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsStartDosagePrism


        let setExclMinAbsStartSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsStartDosagePrism


        let getInclMaxAbsStartSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsStartDosagePrism


        let setInclMaxAbsStartSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsStartDosagePrism


        let getExclMaxAbsStartSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsStartDosagePrism


        let setExclMaxAbsStartSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsStartDosagePrism


        let getInclMinAbsWeightStartSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsWeightStartDosagePrism


        let setInclMinAbsWeightStartSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsWeightStartDosagePrism


        let getExclMinAbsWeightStartSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsWeightStartDosagePrism


        let setExclMinAbsWeightStartSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsWeightStartDosagePrism


        let getInclMaxAbsWeightStartSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsWeightStartDosagePrism


        let setInclMaxAbsWeightStartSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsWeightStartDosagePrism


        let getExclMaxAbsWeightStartSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsWeightStartDosagePrism


        let setExclMaxAbsWeightStartSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsWeightStartDosagePrism


        let getInclMinAbsBSAStartSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsBSAStartDosagePrism


        let setInclMinAbsBSAStartSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsBSAStartDosagePrism


        let getExclMinAbsBSAStartSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsBSAStartDosagePrism


        let setExclMinAbsBSAStartSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsBSAStartDosagePrism


        let getInclMaxAbsBSAStartSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsBSAStartDosagePrism


        let setInclMaxAbsBSAStartSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsBSAStartDosagePrism


        let getExclMaxAbsBSAStartSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsBSAStartDosagePrism


        let setExclMaxAbsBSAStartSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsBSAStartDosagePrism


        let getInclMinNormSingleSubstanceDosage = substanceDosageGetter Dosage.inclMinNormSingleDosagePrism


        let setInclMinNormSingleSubstanceDosage = substanceDosageSetter Dosage.inclMinNormSingleDosagePrism


        let getExclMinNormSingleSubstanceDosage = substanceDosageGetter Dosage.exclMinNormSingleDosagePrism


        let setExclMinNormSingleSubstanceDosage = substanceDosageSetter Dosage.exclMinNormSingleDosagePrism


        let getInclMaxNormSingleSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormSingleDosagePrism


        let setInclMaxNormSingleSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormSingleDosagePrism


        let getExclMaxNormSingleSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormSingleDosagePrism


        let setExclMaxNormSingleSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormSingleDosagePrism


        let getInclMinNormWeightSingleSubstanceDosage = substanceDosageGetter Dosage.inclMinNormWeightSingleDosagePrism


        let setInclMinNormWeightSingleSubstanceDosage = substanceDosageSetter Dosage.inclMinNormWeightSingleDosagePrism


        let getExclMinNormWeightSingleSubstanceDosage = substanceDosageGetter Dosage.exclMinNormWeightSingleDosagePrism


        let setExclMinNormWeightSingleSubstanceDosage = substanceDosageSetter Dosage.exclMinNormWeightSingleDosagePrism


        let getInclMaxNormWeightSingleSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormWeightSingleDosagePrism


        let setInclMaxNormWeightSingleSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormWeightSingleDosagePrism


        let getExclMaxNormWeightSingleSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormWeightSingleDosagePrism


        let setExclMaxNormWeightSingleSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormWeightSingleDosagePrism


        let getInclMinNormBSASingleSubstanceDosage = substanceDosageGetter Dosage.inclMinNormBSASingleDosagePrism


        let setInclMinNormBSASingleSubstanceDosage = substanceDosageSetter Dosage.inclMinNormBSASingleDosagePrism


        let getExclMinNormBSASingleSubstanceDosage = substanceDosageGetter Dosage.exclMinNormBSASingleDosagePrism


        let setExclMinNormBSASingleSubstanceDosage = substanceDosageSetter Dosage.exclMinNormBSASingleDosagePrism


        let getInclMaxNormBSASingleSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormBSASingleDosagePrism


        let setInclMaxNormBSASingleSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormBSASingleDosagePrism


        let getExclMaxNormBSASingleSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormBSASingleDosagePrism


        let setExclMaxNormBSASingleSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormBSASingleDosagePrism


        let getInclMinAbsSingleSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsSingleDosagePrism


        let setInclMinAbsSingleSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsSingleDosagePrism


        let getExclMinAbsSingleSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsSingleDosagePrism


        let setExclMinAbsSingleSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsSingleDosagePrism


        let getInclMaxAbsSingleSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsSingleDosagePrism


        let setInclMaxAbsSingleSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsSingleDosagePrism


        let getExclMaxAbsSingleSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsSingleDosagePrism


        let setExclMaxAbsSingleSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsSingleDosagePrism


        let getInclMinAbsWeightSingleSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsWeightSingleDosagePrism


        let setInclMinAbsWeightSingleSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsWeightSingleDosagePrism


        let getExclMinAbsWeightSingleSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsWeightSingleDosagePrism


        let setExclMinAbsWeightSingleSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsWeightSingleDosagePrism


        let getInclMaxAbsWeightSingleSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsWeightSingleDosagePrism


        let setInclMaxAbsWeightSingleSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsWeightSingleDosagePrism


        let getExclMaxAbsWeightSingleSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsWeightSingleDosagePrism


        let setExclMaxAbsWeightSingleSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsWeightSingleDosagePrism


        let getInclMinAbsBSASingleSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsBSASingleDosagePrism


        let setInclMinAbsBSASingleSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsBSASingleDosagePrism


        let getExclMinAbsBSASingleSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsBSASingleDosagePrism


        let setExclMinAbsBSASingleSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsBSASingleDosagePrism


        let getInclMaxAbsBSASingleSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsBSASingleDosagePrism


        let setInclMaxAbsBSASingleSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsBSASingleDosagePrism


        let getExclMaxAbsBSASingleSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsBSASingleDosagePrism


        let setExclMaxAbsBSASingleSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsBSASingleDosagePrism


        let getInclMinNormRateSubstanceDosage = substanceDosageGetter Dosage.inclMinNormRateDosagePrism


        let setInclMinNormRateSubstanceDosage = substanceDosageSetter Dosage.inclMinNormRateDosagePrism


        let getExclMinNormRateSubstanceDosage = substanceDosageGetter Dosage.exclMinNormRateDosagePrism


        let setExclMinNormRateSubstanceDosage = substanceDosageSetter Dosage.exclMinNormRateDosagePrism


        let getInclMaxNormRateSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormRateDosagePrism


        let setInclMaxNormRateSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormRateDosagePrism


        let getExclMaxNormRateSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormRateDosagePrism


        let setExclMaxNormRateSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormRateDosagePrism


        let getInclMinNormWeightRateSubstanceDosage = substanceDosageGetter Dosage.inclMinNormWeightRateDosagePrism


        let setInclMinNormWeightRateSubstanceDosage = substanceDosageSetter Dosage.inclMinNormWeightRateDosagePrism


        let getExclMinNormWeightRateSubstanceDosage = substanceDosageGetter Dosage.exclMinNormWeightRateDosagePrism


        let setExclMinNormWeightRateSubstanceDosage = substanceDosageSetter Dosage.exclMinNormWeightRateDosagePrism


        let getInclMaxNormWeightRateSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormWeightRateDosagePrism


        let setInclMaxNormWeightRateSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormWeightRateDosagePrism


        let getExclMaxNormWeightRateSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormWeightRateDosagePrism


        let setExclMaxNormWeightRateSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormWeightRateDosagePrism


        let getInclMinNormBSARateSubstanceDosage = substanceDosageGetter Dosage.inclMinNormBSARateDosagePrism


        let setInclMinNormBSARateSubstanceDosage = substanceDosageSetter Dosage.inclMinNormBSARateDosagePrism


        let getExclMinNormBSARateSubstanceDosage = substanceDosageGetter Dosage.exclMinNormBSARateDosagePrism


        let setExclMinNormBSARateSubstanceDosage = substanceDosageSetter Dosage.exclMinNormBSARateDosagePrism


        let getInclMaxNormBSARateSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormBSARateDosagePrism


        let setInclMaxNormBSARateSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormBSARateDosagePrism


        let getExclMaxNormBSARateSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormBSARateDosagePrism


        let setExclMaxNormBSARateSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormBSARateDosagePrism


        let getInclMinAbsRateSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsRateDosagePrism


        let setInclMinAbsRateSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsRateDosagePrism


        let getExclMinAbsRateSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsRateDosagePrism


        let setExclMinAbsRateSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsRateDosagePrism


        let getInclMaxAbsRateSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsRateDosagePrism


        let setInclMaxAbsRateSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsRateDosagePrism


        let getExclMaxAbsRateSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsRateDosagePrism


        let setExclMaxAbsRateSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsRateDosagePrism


        let getInclMinAbsWeightRateSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsWeightRateDosagePrism


        let setInclMinAbsWeightRateSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsWeightRateDosagePrism


        let getExclMinAbsWeightRateSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsWeightRateDosagePrism


        let setExclMinAbsWeightRateSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsWeightRateDosagePrism


        let getInclMaxAbsWeightRateSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsWeightRateDosagePrism


        let setInclMaxAbsWeightRateSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsWeightRateDosagePrism


        let getExclMaxAbsWeightRateSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsWeightRateDosagePrism


        let setExclMaxAbsWeightRateSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsWeightRateDosagePrism


        let getInclMinAbsBSARateSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsBSARateDosagePrism


        let setInclMinAbsBSARateSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsBSARateDosagePrism


        let getExclMinAbsBSARateSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsBSARateDosagePrism


        let setExclMinAbsBSARateSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsBSARateDosagePrism


        let getInclMaxAbsBSARateSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsBSARateDosagePrism


        let setInclMaxAbsBSARateSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsBSARateDosagePrism


        let getExclMaxAbsBSARateSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsBSARateDosagePrism


        let setExclMaxAbsBSARateSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsBSARateDosagePrism


        let getInclMinNormTotalSubstanceDosage = substanceDosageGetter Dosage.inclMinNormTotalDosagePrism


        let setInclMinNormTotalSubstanceDosage = substanceDosageSetter Dosage.inclMinNormTotalDosagePrism


        let getExclMinNormTotalSubstanceDosage = substanceDosageGetter Dosage.exclMinNormTotalDosagePrism


        let setExclMinNormTotalSubstanceDosage = substanceDosageSetter Dosage.exclMinNormTotalDosagePrism


        let getInclMaxNormTotalSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormTotalDosagePrism


        let setInclMaxNormTotalSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormTotalDosagePrism


        let getExclMaxNormTotalSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormTotalDosagePrism


        let setExclMaxNormTotalSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormTotalDosagePrism


        let getInclMinNormWeightTotalSubstanceDosage = substanceDosageGetter Dosage.inclMinNormWeightTotalDosagePrism


        let setInclMinNormWeightTotalSubstanceDosage = substanceDosageSetter Dosage.inclMinNormWeightTotalDosagePrism


        let getExclMinNormWeightTotalSubstanceDosage = substanceDosageGetter Dosage.exclMinNormWeightTotalDosagePrism


        let setExclMinNormWeightTotalSubstanceDosage = substanceDosageSetter Dosage.exclMinNormWeightTotalDosagePrism


        let getInclMaxNormWeightTotalSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormWeightTotalDosagePrism


        let setInclMaxNormWeightTotalSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormWeightTotalDosagePrism


        let getExclMaxNormWeightTotalSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormWeightTotalDosagePrism


        let setExclMaxNormWeightTotalSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormWeightTotalDosagePrism


        let getInclMinNormBSATotalSubstanceDosage = substanceDosageGetter Dosage.inclMinNormBSATotalDosagePrism


        let setInclMinNormBSATotalSubstanceDosage = substanceDosageSetter Dosage.inclMinNormBSATotalDosagePrism


        let getExclMinNormBSATotalSubstanceDosage = substanceDosageGetter Dosage.exclMinNormBSATotalDosagePrism


        let setExclMinNormBSATotalSubstanceDosage = substanceDosageSetter Dosage.exclMinNormBSATotalDosagePrism


        let getInclMaxNormBSATotalSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormBSATotalDosagePrism


        let setInclMaxNormBSATotalSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormBSATotalDosagePrism


        let getExclMaxNormBSATotalSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormBSATotalDosagePrism


        let setExclMaxNormBSATotalSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormBSATotalDosagePrism


        let getInclMinAbsTotalSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsTotalDosagePrism


        let setInclMinAbsTotalSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsTotalDosagePrism


        let getExclMinAbsTotalSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsTotalDosagePrism


        let setExclMinAbsTotalSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsTotalDosagePrism


        let getInclMaxAbsTotalSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsTotalDosagePrism


        let setInclMaxAbsTotalSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsTotalDosagePrism


        let getExclMaxAbsTotalSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsTotalDosagePrism


        let setExclMaxAbsTotalSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsTotalDosagePrism


        let getInclMinAbsWeightTotalSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsWeightTotalDosagePrism


        let setInclMinAbsWeightTotalSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsWeightTotalDosagePrism


        let getExclMinAbsWeightTotalSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsWeightTotalDosagePrism


        let setExclMinAbsWeightTotalSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsWeightTotalDosagePrism


        let getInclMaxAbsWeightTotalSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsWeightTotalDosagePrism


        let setInclMaxAbsWeightTotalSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsWeightTotalDosagePrism


        let getExclMaxAbsWeightTotalSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsWeightTotalDosagePrism


        let setExclMaxAbsWeightTotalSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsWeightTotalDosagePrism


        let getInclMinAbsBSATotalSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsBSATotalDosagePrism


        let setInclMinAbsBSATotalSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsBSATotalDosagePrism


        let getExclMinAbsBSATotalSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsBSATotalDosagePrism


        let setExclMinAbsBSATotalSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsBSATotalDosagePrism


        let getInclMaxAbsBSATotalSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsBSATotalDosagePrism


        let setInclMaxAbsBSATotalSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsBSATotalDosagePrism


        let getExclMaxAbsBSATotalSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsBSATotalDosagePrism


        let setExclMaxAbsBSATotalSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsBSATotalDosagePrism



    let private convertTo conv gen u (dr : DoseRule) =
        {
            dr with
                IndicationsDosages =
                    dr.IndicationsDosages
                    |> List.map (fun id ->
                        {
                            id with
                                RouteDosages =
                                    id.RouteDosages
                                    |> List.map (fun rd ->
                                        {
                                            rd with
                                                ShapeDosages =
                                                    rd.ShapeDosages
                                                    |> List.map (fun sd ->
                                                         {
                                                            sd with
                                                                PatientDosages =
                                                                    sd.PatientDosages
                                                                    |> List.map (fun pd ->
                                                                        {
                                                                            pd with
                                                                                SubstanceDosages =
                                                                                    pd.SubstanceDosages
                                                                                    |> List.map (fun sd ->
                                                                                        if sd.Name = gen then
                                                                                            sd
                                                                                            |> conv u
                                                                                        else sd
                                                                                    )
                                                                        }
                                                                    )
                                                         }
                                                    )
                                        }
                                    )
                        }
                    )

        }


    let convertSubstanceUnitTo = convertTo Dosage.convertSubstanceUnitTo


    let convertRateUnitTo = convertTo Dosage.convertRateUnitTo


    module Operators =

        let (|>>) (x1, x2) f = f x2 x1


        let (|>>>) (x1, x2) f = f x2 x1, x2



    let mdText = """
## _Stofnaam_: {generic}
Synoniemen: {synonym}

---

### _ATC code_: {atc}

### _Therapeutische groep_: {thergroup}

### _Therapeutische subgroep_: {thersub}

### _Generiek groep_: {gengroup}

### _Generiek subgroep_: {gensub}

"""

    let mdIndicationText = """

---

### _Indicatie_: {indication}
"""


    let mdRouteText = """
* _Route_: {route}
"""

    let mdShapeText = """
  * _Vorm_: {shape}
  * _Producten_:
  * {products}
"""

    let mdPatientText = """
    * _Patient_: __{patient}__
"""

    let mdDosageText = """
      * {dosage}

"""


    type TextConfig =
        {
            MainText: string
            IndicationText : string
            RouteText : string
            ShapeText : string
            PatientText : string
            DosageText : string
        }


    let mdConfig =
        {
            MainText = mdText
            IndicationText = mdIndicationText
            RouteText = mdRouteText
            ShapeText = mdShapeText
            PatientText = mdPatientText
            DosageText = mdDosageText
        }


    let toStringWithConfig (config : TextConfig) printRules (dr : DoseRule) =
        let gpsToString (gps : ShapeDosage.GenericProduct list) =
            gps
            |> List.map (fun gp -> gp.Label)
            |> String.concat ", "

        config.MainText
        |> String.replace "{generic}" dr.Generic
        |> String.replace "{synonym}" (dr.Synonyms |> String.concat ",")
        |> String.replace "{atc}" dr.ATC
        |> String.replace "{thergroup}" dr.ATCTherapyGroup
        |> String.replace "{thersub}" dr.ATCTherapySubGroup
        |> String.replace "{gengroup}" dr.GenericGroup
        |> String.replace "{gensub}" dr.GenericSubGroup
        |> (fun s ->
            dr.IndicationsDosages
            |> List.fold (fun acc id ->
                let ind =
                    id.Indications
                    |> String.concat ", "

                id.RouteDosages
                |> List.fold (fun acc rd ->

                    rd.ShapeDosages
                    |> List.fold (fun acc sd ->
                        let shapeStr =
                            config.ShapeText
                            |> String.replace "{shape}" (sd.Shape |> String.concat ",")
                            |> String.replace "{products}" (sd.GenericProducts |> gpsToString)

                        sd.PatientDosages
                        |> List.fold (fun acc pd ->

                            let s =
                                (config.PatientText
                                 |> String.replace "{patient}" (pd.Patient |> Patient.toString)) +
                                ("{dosage}"
                                 |> String.replace "{dosage}" (pd.ShapeDosage |> Dosage.toString printRules))

                            pd.SubstanceDosages
                            |> List.fold (fun acc sd ->

                                acc + (config.DosageText |> String.replace "{dosage}" (sd |> Dosage.toString printRules))

                            ) (acc + s)

                        ) (acc + shapeStr)

                    ) (acc + (config.RouteText |> String.replace "{route}" rd.Route))

                ) (acc + (config.IndicationText |> String.replace "{indication}" ind))
            ) s
        )


    let toString = toStringWithConfig mdConfig

    module Dto =


        type Dto () =
            // Generic the doserule applies to
            member val Generic = "" with get, set
            // List of synonyms for the generic
            member val Synomyms : string list = [] with get, set
            // The ATC code
            member val ATC = "" with get, set
            // ATCTherapyGroup the doserule applies to
            member val ATCTherapyGroup = "" with get, set
            // ATCTherapySubGroup the doserule applies to
            member val ATCTherapySubGroup = "" with get, set
            // The generic group the doserule applies to
            member val GenericGroup = "" with get, set
            // The generic subgroup the doserule applies to
            member val GenericSubGroup = "" with get, set
            // The doserules per indication(-s)
            member val Indications : IndicationDosage.Dto.Dto list = [] with get, set


        let dto () = Dto ()


        let toDto (dr : DoseRule) =
            let dto = dto ()

            dto.Generic <- dr.Generic
            dto.Synomyms <- dr.Synonyms
            dto.ATC <- dr.ATC
            dto.ATCTherapyGroup <- dr.ATCTherapyGroup
            dto.ATCTherapySubGroup <- dr.ATCTherapySubGroup
            dto.GenericGroup <- dr.GenericGroup
            dto.GenericSubGroup <- dr.GenericSubGroup
            dto.Indications <-
                dr.IndicationsDosages
                |> List.map IndicationDosage.Dto.toDto

            dto

        let fromDto (dto : Dto) =
            let gen = dto.Generic
            let syn = dto.Synomyms
            let atc = dto.ATC
            let thg = dto.ATCTherapyGroup
            let sub = dto.ATCTherapySubGroup
            let ggp = dto.GenericGroup
            let gsg = dto.GenericSubGroup

            dto.Indications
            |> List.map IndicationDosage.Dto.fromDto
            |> create gen syn atc thg sub ggp gsg


    module DoseRuleTests =

        let (|>!) x f =
            printfn "%A" x
            x |> f

        let test () =

            let dto = Dto.dto ()


            dto
            |>! Dto.fromDto
            |>! ignore
