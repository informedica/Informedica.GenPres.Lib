#I __SOURCE_DIRECTORY__

#load "./../../../.paket/load/netstandard2.0/main.group.fsx"

#r "./../../Informedica.GenUtils.Lib/bin/Debug/netstandard2.0/Informedica.GenUtils.Lib.dll"

#time


open Informedica.GenUtils.Lib.BCL
open Informedica.GenUtils.Lib

open Aether
open Aether.Operators



module List =

    let toStr = List.toString

    let tryFindRest pred xs =
        let rec find x xs notx =
            match xs with
            | [] -> x, notx
            | h::tail ->
                if h |> pred then find (Some h) tail notx
                else find x tail ([h] |> List.append notx)

        find None xs []


    let toString xs =
        xs
        |> toStr
        |> String.replace "[" ""
        |> String.replace "]" ""
        |> String.replace ";" ","
    


module ValueUnit =

    type Unit = string


    /// Value and unit
    type ValueUnit =
        {
            Value : float
            Unit : Unit
        }


    let empty = { Value = 0.; Unit = "" }


    let create v u = { Value = v; Unit = u }


    type ValueUnit with
    
        static member Value_ :
            (ValueUnit -> float) * (float -> ValueUnit -> ValueUnit) =
            (fun vu -> vu.Value), (fun v vu -> { vu with Value = v })
    
        static member Unit_ :
            (ValueUnit -> string) * (string -> ValueUnit -> ValueUnit) =
            (fun vu -> vu.Unit), (fun u vu -> { vu with Unit = u })
    

    module Props =    
    
        let getValue = Optic.get ValueUnit.Value_
    
    
        let setValue = Optic.set ValueUnit.Value_ 
    
    
        let getUnit = Optic.get ValueUnit.Unit_
    
    
        let setUnit = Optic.set ValueUnit.Unit_ 


    let toString vu = 
        sprintf "%A %s" (vu |> Props.getValue) (vu |> Props.getUnit)



module MinMax =


    type ValueUnit = ValueUnit.ValueUnit


    /// Range with min and/or max
    type MinMax =
        {
            Min : Value option
            Max : Value option
        }
    and Value = Inclusive of ValueUnit | Exclusive of ValueUnit


    let create min max = { Min = min; Max = max }
    

    let empty = create None None


    let inclusive v = v |> Inclusive


    let exclusive v = v |> Exclusive


    let min v = { empty with Min = v |> Some }


    let max v = { empty with Max = v |> Some }
    

    let minIncl v = min (v |> inclusive) 


    let minExcl v = min (v |> exclusive) 


    let maxIncl v = max (v |> inclusive) 


    let maxExcl v = max (v |> exclusive) 


    type Value with
    
        static member Inclusive_ =
            (fun v ->
                match v with
                | Inclusive v_ -> v_ |> Some 
                | Exclusive _  -> None
            ), 
            (fun x v ->
                match v with 
                | Inclusive _ -> x |> Inclusive
                | Exclusive _ -> v
            )
    
    
        static member Exclusive_ =
            (fun v ->
                match v with
                | Inclusive _  -> None
                | Exclusive v_ -> v_ |> Some
            ), 
            (fun x v ->
                match v with 
                | Inclusive _ -> v
                | Exclusive _ -> x |> Exclusive
            )

    
    type MinMax with
    
        static member Min_ :
            (MinMax -> Value Option) * (Value -> MinMax -> MinMax) =
            (fun mm -> mm.Min), 
            (fun v mm -> { mm with Min = Some v })
    
        static member Max_ :
            (MinMax -> Value Option) * (Value -> MinMax -> MinMax) =
            (fun mm -> mm.Max), 
            (fun v mm -> { mm with Max = Some v })


    
    module Props =

        module ValueUnit = ValueUnit.Props
    
    
        let getInclusive = Optic.get Value.Inclusive_
    
    
        let setInclusive = Optic.set Value.Inclusive_
    
    
        let getExclusive = Optic.get Value.Exclusive_
    
    
        let setExclusive = Optic.set Value.Exclusive_
    
    
        let inclusiveValueLens = Value.Inclusive_ >?> ValueUnit.Value_
    
    
        let getInclusiveValue = Optic.get inclusiveValueLens
    
    
        let setInclusiveValue = Optic.set inclusiveValueLens 
        
    
        let inclusiveUnitLens = Value.Inclusive_ >?> ValueUnit.Unit_
    
    
        let getInclusiveUnit = Optic.get inclusiveUnitLens
    
    
        let setInclusiveUnit = Optic.set inclusiveUnitLens 
        
    
        let exclusiveValueLens = Value.Exclusive_ >?> ValueUnit.Value_
    
    
        let getExclusiveValue = Optic.get exclusiveValueLens
    
    
        let setExclusiveValue = Optic.set exclusiveValueLens 
        
    
        let exclusiveUnitLens = Value.Exclusive_ >?> ValueUnit.Unit_
    
    
        let getExclusiveUnit = Optic.get exclusiveUnitLens
    
    
        let setExclusiveUnit = Optic.set exclusiveUnitLens 
    
    
        let getMin = Optic.get MinMax.Min_
    
    
        let setMin = Optic.set MinMax.Min_
    
    
        let inclMinLens = 
            (fun mm -> 
                match mm |> getMin with 
                | Some min -> 
                    match min with 
                    | Inclusive v -> Some v 
                    | _ -> None 
                | None -> None),
            (fun vu mm -> mm |> setMin (vu |> inclusive))
    
    
        let getInclMin = Optic.get inclMinLens
    
    
        let setInclMin = Optic.set inclMinLens
    
    
        let valueInclMinLens = inclMinLens >?> ValueUnit.Value_
    
    
        let getValueInclMin = Optic.get valueInclMinLens
    
    
        let setValueInclMin = Optic.set valueInclMinLens
    
    
        let unitInclMinLens = inclMinLens >?> ValueUnit.Unit_
    
    
        let getUnitInclMin = Optic.get unitInclMinLens
    
    
        let setUnitInclMin = Optic.set unitInclMinLens
    
    
        let exclMinLens = 
            (fun mm -> 
                match mm |> getMin with 
                | Some min -> 
                    match min with 
                    | Exclusive v -> Some v 
                    | _ -> None 
                | None -> None),
            (fun vu mm -> mm |> setMin (vu |> exclusive))
    
    
        let getExclMin = Optic.get exclMinLens
    
    
        let setExclMin = Optic.set exclMinLens
        
    
        let valueExclMinLens = exclMinLens >?> ValueUnit.Value_
    
    
        let getValueExclMin = Optic.get valueExclMinLens
    
    
        let setValueExclMin = Optic.set valueExclMinLens
    
    
        let unitExclMinLens = exclMinLens >?> ValueUnit.Unit_
    
    
        let getUnitExclMin = Optic.get unitExclMinLens
    
    
        let setUnitExclMin = Optic.set unitExclMinLens
    
    
        let getMax = Optic.get MinMax.Max_
    
    
        let setMax = Optic.set MinMax.Max_
    
    
        let inclMaxLens = 
            (fun mm -> 
                match mm |> getMax with 
                | Some max -> 
                    match max with 
                    | Inclusive v -> Some v 
                    | _ -> None 
                | None -> None),
            (fun vu mm -> mm |> setMax (vu |> inclusive))
    
    
        let getInclMax = Optic.get inclMaxLens
    
    
        let setInclMax = Optic.set inclMaxLens
    
    
        let valueInclMaxLens = inclMaxLens >?> ValueUnit.Value_
    
    
        let getValueInclMax = Optic.get valueInclMaxLens
    
    
        let setValueInclMax = Optic.set valueInclMaxLens
    
    
        let unitInclMaxLens = inclMaxLens >?> ValueUnit.Unit_
    
    
        let getUnitInclMax = Optic.get unitInclMaxLens
    
    
        let setUnitInclMax = Optic.set unitInclMaxLens
    
    
        let exclMaxLens = 
            (fun mm -> 
                match mm |> getMax with 
                | Some max -> 
                    match max with 
                    | Exclusive v -> Some v 
                    | _ -> None 
                | None -> None),
            (fun vu mm -> mm |> setMax (vu |> exclusive))
    
    
        let getExclMax = Optic.get exclMaxLens
    
    
        let setExclMax = Optic.set exclMaxLens
        
    
        let valueExclMaxLens = exclMaxLens >?> ValueUnit.Value_
    
    
        let getValueExclMax = Optic.get valueExclMaxLens
    
    
        let setValueExclMax = Optic.set valueExclMaxLens
    
    
        let unitExclMaxLens = exclMaxLens >?> ValueUnit.Unit_
    
    
        let getUnitExclMax = Optic.get unitExclMaxLens
    
    
        let setUnitExclMax = Optic.set unitExclMaxLens
        

    let toString { Min = min; Max = max } =
        let minToString min =
            match min with 
            | Inclusive vu ->
                vu |> ValueUnit.toString |> sprintf "meer dan en gelijk aan %s"
            | Exclusive vu ->
                vu |> ValueUnit.toString |> sprintf "meer dan %s"

        let maxToString min =
            match min with 
            | Inclusive vu ->
                vu |> ValueUnit.toString |> sprintf "tot en met %s"
            | Exclusive vu ->
                vu |> ValueUnit.toString |> sprintf "tot %s"

        match min, max with
        | None, None -> ""
        | Some min_, Some max_ -> 
            sprintf "%s - %s "(min_ |> minToString) (max_ |> maxToString)
        | Some min_, None -> 
            (min_ |> minToString) 
        | None, Some max_ -> 
            (max_ |> maxToString)



module DoseRange =


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
    and WeightUnit = string
    and BSAUnit = string

    let create norm normWght normBSA abs absWght absBSA =
        {
            Norm = norm
            NormWeight = normWght
            NormBSA = normBSA
            Abs = abs
            AbsWeight = absWght
            AbsBSA = absBSA
        }

    let emptyWeight = MinMax.empty, ""
    
    
    let emptyBSA = MinMax.empty, ""


    let empty = create MinMax.empty emptyWeight emptyBSA MinMax.empty emptyWeight emptyBSA


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

    module Props =
        
        module MinMax = MinMax.Props


        let getNorm = Optic.get DoseRange.Norm_


        let setNorm = Optic.set DoseRange.Norm_


        let minNormLens = DoseRange.Norm_ >-> MinMax.Min_


        let getMinNorm = Optic.get minNormLens


        let setMinNorm = Optic.set minNormLens


        let inclMinNormLens =
            DoseRange.Norm_ >-> MinMax.inclMinLens 


        let getInclMinNorm = Optic.get inclMinNormLens 


        let setInclMinNorm = Optic.set inclMinNormLens


        let exclMinNormLens =
            DoseRange.Norm_ >-> MinMax.exclMinLens


        let getExclMinNorm = Optic.get exclMinNormLens 


        let setExclMinNorm = Optic.set exclMinNormLens


        let maxNormLens = DoseRange.Norm_ >-> MinMax.Max_


        let getMaxNorm = Optic.get maxNormLens


        let setMaxNorm = Optic.set maxNormLens


        let inclMaxNormLens =
            DoseRange.Norm_ >-> (MinMax.inclMaxLens) 


        let getInclMaxNorm = Optic.get inclMaxNormLens 


        let setInclMaxNorm = Optic.set inclMaxNormLens


        let exclMaxNormLens =
            DoseRange.Norm_ >-> (MinMax.exclMaxLens) 


        let getExclMaxNorm = Optic.get exclMaxNormLens 


        let setExclMaxNorm = Optic.set exclMaxNormLens


        let getNormWeight = Optic.get DoseRange.NormWeight_


        let setNormWeight = Optic.set DoseRange.NormWeight_


        let normWeightUnitLens = DoseRange.NormWeight_ >-> snd_


        let getNormWeightUnit = Optic.get normWeightUnitLens


        let setNormWeightUnit = Optic.set normWeightUnitLens


        let minNormWeightLens = DoseRange.NormWeight_ >-> fst_ >-> MinMax.Min_


        let getMinNormWeight = Optic.get minNormWeightLens


        let setMinNormWeight = Optic.set minNormWeightLens


        let inclMinNormWeightLens =
            DoseRange.NormWeight_ >-> fst_ >-> MinMax.inclMinLens


        let getInclMinNormWeight = Optic.get inclMinNormWeightLens 


        let setInclMinNormWeight = Optic.set inclMinNormWeightLens


        let exclMinNormWeightLens =
            DoseRange.NormWeight_ >-> fst_ >-> MinMax.exclMinLens


        let getExclMinNormWeight = Optic.get exclMinNormWeightLens 


        let setExclMinNormWeight = Optic.set exclMinNormWeightLens


        let maxNormWeightLens = DoseRange.NormWeight_ >-> fst_ >-> MinMax.Max_


        let getMaxNormWeight = Optic.get maxNormWeightLens


        let setMaxNormWeight = Optic.set maxNormWeightLens


        let inclMaxNormWeightLens =
            DoseRange.NormWeight_ >-> fst_ >-> MinMax.inclMaxLens


        let getInclMaxNormWeight = Optic.get inclMaxNormWeightLens 


        let setInclMaxNormWeight = Optic.set inclMaxNormWeightLens


        let exclMaxNormWeightLens =
            DoseRange.NormWeight_ >-> fst_ >-> MinMax.exclMaxLens 


        let getExclMaxNormWeight = Optic.get exclMaxNormWeightLens 


        let setExclMaxNormWeight = Optic.set exclMaxNormWeightLens


        let getNormBSA = Optic.get DoseRange.NormBSA_


        let setNormBSA = Optic.set DoseRange.NormBSA_


        let normBSAUnitLens = DoseRange.NormBSA_ >-> snd_


        let getNormBSAUnit = Optic.get normBSAUnitLens


        let setNormBSAUnit = Optic.set normBSAUnitLens


        let minNormBSALens = DoseRange.NormBSA_ >-> fst_ >-> MinMax.Min_


        let getMinNormBSA = Optic.get minNormBSALens


        let setMinNormBSA = Optic.set minNormBSALens


        let inclMinNormBSALens =
            DoseRange.NormBSA_ >-> fst_ >-> MinMax.inclMinLens


        let getInclMinNormBSA = Optic.get inclMinNormBSALens 


        let setInclMinNormBSA = Optic.set inclMinNormBSALens


        let exclMinNormBSALens =
            DoseRange.NormBSA_ >-> fst_ >-> MinMax.exclMinLens


        let getExclMinNormBSA = Optic.get exclMinNormBSALens 


        let setExclMinNormBSA = Optic.set exclMinNormBSALens


        let maxNormBSALens = DoseRange.NormBSA_ >-> fst_ >-> MinMax.Max_


        let getMaxNormBSA = Optic.get maxNormBSALens


        let setMaxNormBSA = Optic.set maxNormBSALens


        let inclMaxNormBSALens =
            DoseRange.NormBSA_ >-> fst_ >-> MinMax.inclMaxLens


        let getInclMaxNormBSA = Optic.get inclMaxNormBSALens 


        let setInclMaxNormBSA = Optic.set inclMaxNormBSALens


        let exclMaxNormBSALens =
            DoseRange.NormBSA_ >-> fst_ >-> MinMax.exclMaxLens


        let getExclMaxNormBSA = Optic.get exclMaxNormBSALens 


        let setExclMaxNormBSA = Optic.set exclMaxNormBSALens


        let getAbs = Optic.get DoseRange.Abs_


        let setAbs = Optic.set DoseRange.Abs_


        let minAbsLens = DoseRange.Abs_ >-> MinMax.Min_


        let getMinAbs = Optic.get minAbsLens


        let setMinAbs = Optic.set minAbsLens


        let inclMinAbsLens =
            DoseRange.Abs_ >-> (MinMax.inclMinLens) 


        let getInclMinAbs = Optic.get inclMinAbsLens 


        let setInclMinAbs = Optic.set inclMinAbsLens


        let exclMinAbsLens =
            DoseRange.Abs_ >-> (MinMax.exclMinLens) 


        let getExclMinAbs = Optic.get exclMinAbsLens 


        let setExclMinAbs = Optic.set exclMinAbsLens


        let maxAbsLens = DoseRange.Abs_ >-> MinMax.Max_


        let getMaxAbs = Optic.get maxAbsLens


        let setMaxAbs = Optic.set maxAbsLens


        let inclMaxAbsLens =
            DoseRange.Abs_ >-> (MinMax.inclMaxLens) 


        let getInclMaxAbs = Optic.get inclMaxAbsLens 


        let setInclMaxAbs = Optic.set inclMaxAbsLens


        let exclMaxAbsLens =
            DoseRange.Abs_ >-> (MinMax.exclMaxLens) 


        let getExclMaxAbs = Optic.get exclMaxAbsLens 


        let setExclMaxAbs = Optic.set exclMaxAbsLens


        let getAbsWeight = Optic.get DoseRange.AbsWeight_


        let setAbsWeight = Optic.set DoseRange.AbsWeight_


        let absWeightUnitLens = DoseRange.AbsWeight_ >-> snd_


        let getAbsWeightUnit = Optic.get absWeightUnitLens


        let setAbsWeightUnit = Optic.set absWeightUnitLens


        let minAbsWeightLens = DoseRange.AbsWeight_ >-> fst_ >-> MinMax.Min_


        let getMinAbsWeight = Optic.get minAbsWeightLens


        let setMinAbsWeight = Optic.set minAbsWeightLens


        let inclMinAbsWeightLens =
            DoseRange.AbsWeight_ >-> fst_ >-> MinMax.inclMinLens


        let getInclMinAbsWeight = Optic.get inclMinAbsWeightLens 


        let setInclMinAbsWeight = Optic.set inclMinAbsWeightLens


        let exclMinAbsWeightLens =
            DoseRange.AbsWeight_ >-> fst_ >-> MinMax.exclMinLens


        let getExclMinAbsWeight = Optic.get exclMinAbsWeightLens 


        let setExclMinAbsWeight = Optic.set exclMinAbsWeightLens


        let maxAbsWeightLens = DoseRange.AbsWeight_ >-> fst_ >-> MinMax.Max_


        let getMaxAbsWeight = Optic.get maxAbsWeightLens


        let setMaxAbsWeight = Optic.set maxAbsWeightLens


        let inclMaxAbsWeightLens =
            DoseRange.AbsWeight_ >-> fst_ >-> MinMax.inclMaxLens


        let getInclMaxAbsWeight = Optic.get inclMaxAbsWeightLens 


        let setInclMaxAbsWeight = Optic.set inclMaxAbsWeightLens


        let exclMaxAbsWeightLens =
            DoseRange.AbsWeight_ >-> fst_ >-> MinMax.exclMaxLens


        let getExclMaxAbsWeight = Optic.get exclMaxAbsWeightLens 


        let setExclMaxAbsWeight = Optic.set exclMaxAbsWeightLens


        let getAbsBSA = Optic.get DoseRange.AbsBSA_


        let setAbsBSA = Optic.set DoseRange.AbsBSA_


        let absBSAUnitLens = DoseRange.AbsBSA_ >-> snd_


        let getAbsBSAUnit = Optic.get absBSAUnitLens


        let setAbsBSAUnit = Optic.set absBSAUnitLens


        let minAbsBSALens = DoseRange.AbsBSA_ >-> fst_ >-> MinMax.Min_


        let getMinAbsBSA = Optic.get minAbsBSALens


        let setMinAbsBSA = Optic.set minAbsBSALens


        let inclMinAbsBSALens =
            DoseRange.AbsBSA_ >-> fst_ >-> MinMax.inclMinLens


        let getInclMinAbsBSA = Optic.get inclMinAbsBSALens 


        let setInclMinAbsBSA = Optic.set inclMinAbsBSALens


        let exclMinAbsBSALens =
            DoseRange.AbsBSA_ >-> fst_ >-> MinMax.exclMinLens


        let getExclMinAbsBSA = Optic.get exclMinAbsBSALens 


        let setExclMinAbsBSA = Optic.set exclMinAbsBSALens


        let maxAbsBSALens = DoseRange.AbsBSA_ >-> fst_ >-> MinMax.Max_


        let getMaxAbsBSA = Optic.get maxAbsBSALens


        let setMaxAbsBSA = Optic.set maxAbsBSALens


        let inclMaxAbsBSALens =
            DoseRange.AbsBSA_ >-> fst_ >-> MinMax.inclMaxLens


        let getInclMaxAbsBSA = Optic.get inclMaxAbsBSALens 


        let setInclMaxAbsBSA = Optic.set inclMaxAbsBSALens


        let exclMaxAbsBSALens =
            DoseRange.AbsBSA_ >-> fst_ >-> MinMax.exclMaxLens


        let getExclMaxAbsBSA = Optic.get exclMaxAbsBSALens 


        let setExclMaxAbsBSA = Optic.set exclMaxAbsBSALens


    let toString ({ Norm = norm; NormWeight = normwght; NormBSA = normbsa; Abs = abs; AbsWeight = abswght; AbsBSA = absbsa}) =
        let (>+) sl sr = 
            if sl |> String.isNullOrWhiteSpace then sr
            else sl + "\n" + sr
        
        let nw, nwu = normwght
        let nb, nbu = normbsa
        let aw, nau = abswght
        let ab, nbu = absbsa
        
        let mmtoStr u mm = 
            mm 
            |> MinMax.toString 
            |> (fun s -> 
                if s |> String.isNullOrWhiteSpace || u |> String.isNullOrWhiteSpace then s 
                else s + " " + u
            )
            
        norm 
        |> MinMax.toString
        >+ (nw |> mmtoStr nwu)
        >+ (nb |> mmtoStr nbu)
        |> (fun s -> 
            if s |> String.isNullOrWhiteSpace then s
            else "Normaal grenzen:\n" + s
        )
        |> (fun sn ->
            let sa =
                abs |> MinMax.toString
                >+ (aw |> mmtoStr nau)
                >+ (ab |> mmtoStr nbu)
            if sa |> String.isNullOrWhiteSpace then sn
            else 
                sn +
                "Absolute grenzen:\n" + sa
        )
        
        
    
module Dosage =


    type DoseRange = DoseRange.DoseRange


    /// Dosage
    type Dosage =
        {
            Name : string
            /// Dosage at the start
            StartDosage : DoseRange
            /// Dosage per administration
            SingleDosage : DoseRange
            /// Dosage rate
            RateDosage : DoseRange * RateUnit
            /// Total dosage per time period
            TotalDosage : DoseRange * TimeUnit
            /// Allowed frequencies
            Frequencies : Frequencies
        }
    and Frequencies = int list
    and TimeUnit = string
    and RateUnit = string


    let create nm start single rate total freqs =
        {
            Name = nm
            StartDosage = start
            SingleDosage = single
            RateDosage = rate
            TotalDosage = total
            Frequencies = freqs
        }


    let empty = create "" DoseRange.empty DoseRange.empty (DoseRange.empty, "") (DoseRange.empty, "" ) []


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
            (Dosage -> (DoseRange * TimeUnit)) * ((DoseRange * TimeUnit) -> Dosage -> Dosage) =
            (fun d -> d.TotalDosage),
            (fun dt d -> { d with TotalDosage = dt })
            
        static member Frequencies_ :
            (Dosage -> Frequencies) * (Frequencies -> Dosage -> Dosage) =
            (fun d -> d.Frequencies) ,
            (fun frqs d -> { d with Frequencies = frqs })        

    
    module Props =

        module DoseRange = DoseRange.Props


        let getName = Optic.get Dosage.Name_
    
    
        let setName = Optic.set Dosage.Name_


        let getFrequencies = Optic.get Dosage.Frequencies_
    
    
        let setFrequencies = Optic.set Dosage.Frequencies_


        let getStartDosage = Optic.get Dosage.StartDosage_


        let setStartDosage = Optic.set Dosage.StartDosage_
    

        let getSingleDosage = Optic.get Dosage.SingleDosage_


        let setSingleDosage = Optic.set Dosage.SingleDosage_


        let getRateDosage = Optic.get Dosage.RateDosage_


        let setRateDosage = Optic.set Dosage.RateDosage_


        let getTotalDosage = Optic.get Dosage.TotalDosage_


        let setTotalDosage = Optic.set Dosage.TotalDosage_


        let inclMinNormStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMinNormLens


        let getInclMinNormStartDosage = Optic.get inclMinNormStartDosagePrism


        let setInclMinNormStartDosage = Optic.set inclMinNormStartDosagePrism


        let exclMinNormStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMinNormLens


        let getExclMinNormStartDosage = Optic.get exclMinNormStartDosagePrism


        let setExclMinNormStartDosage = Optic.set exclMinNormStartDosagePrism


        let inclMaxNormStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMaxNormLens


        let getInclMaxNormStartDosage = Optic.get inclMaxNormStartDosagePrism


        let setInclMaxNormStartDosage = Optic.set inclMaxNormStartDosagePrism


        let exclMaxNormStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMaxNormLens


        let getExclMaxNormStartDosage = Optic.get exclMaxNormStartDosagePrism


        let setExclMaxNormStartDosage = Optic.set exclMaxNormStartDosagePrism

        
        let normWeightUnitStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.normWeightUnitLens


        let getNormWeightUnitStartDosage = Optic.get normWeightUnitStartDosagePrism


        let setNormWeightUnitStartDosage = Optic.set normWeightUnitStartDosagePrism


        let inclMinNormWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMinNormWeightLens


        let getInclMinNormWeightStartDosage = Optic.get inclMinNormWeightStartDosagePrism


        let setInclMinNormWeightStartDosage = Optic.set inclMinNormWeightStartDosagePrism


        let exclMinNormWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMinNormWeightLens


        let getExclMinNormWeightStartDosage = Optic.get exclMinNormWeightStartDosagePrism


        let setExclMinNormWeightStartDosage = Optic.set exclMinNormWeightStartDosagePrism


        let inclMaxNormWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMaxNormWeightLens


        let getInclMaxNormWeightStartDosage = Optic.get inclMaxNormWeightStartDosagePrism


        let setInclMaxNormWeightStartDosage = Optic.set inclMaxNormWeightStartDosagePrism


        let exclMaxNormWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMaxNormWeightLens


        let getExclMaxNormWeightStartDosage = Optic.get exclMaxNormWeightStartDosagePrism


        let setExclMaxNormWeightStartDosage = Optic.set exclMaxNormWeightStartDosagePrism


        let normBSAUnitStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.normBSAUnitLens


        let getNormBSAUnitStartDosage = Optic.get normBSAUnitStartDosagePrism


        let setNormBSAUnitStartDosage = Optic.set normBSAUnitStartDosagePrism


        let inclMinNormBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMinNormBSALens


        let getInclMinNormBSAStartDosage = Optic.get inclMinNormBSAStartDosagePrism


        let setInclMinNormBSAStartDosage = Optic.set inclMinNormBSAStartDosagePrism


        let exclMinNormBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMinNormBSALens


        let getExclMinNormBSAStartDosage = Optic.get exclMinNormBSAStartDosagePrism


        let setExclMinNormBSAStartDosage = Optic.set exclMinNormBSAStartDosagePrism


        let inclMaxNormBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMaxNormBSALens


        let getInclMaxNormBSAStartDosage = Optic.get inclMaxNormBSAStartDosagePrism


        let setInclMaxNormBSAStartDosage = Optic.set inclMaxNormBSAStartDosagePrism


        let exclMaxNormBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMaxNormBSALens


        let getExclMaxNormBSAStartDosage = Optic.get exclMaxNormBSAStartDosagePrism


        let setExclMaxNormBSAStartDosage = Optic.set exclMaxNormBSAStartDosagePrism


        let inclMinNormSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMinNormLens


        let getInclMinNormSingleDosage = Optic.get inclMinNormSingleDosagePrism


        let setInclMinNormSingleDosage = Optic.set inclMinNormSingleDosagePrism


        let exclMinNormSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMinNormLens


        let getExclMinNormSingleDosage = Optic.get exclMinNormSingleDosagePrism


        let setExclMinNormSingleDosage = Optic.set exclMinNormSingleDosagePrism


        let inclMaxNormSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMaxNormLens


        let getInclMaxNormSingleDosage = Optic.get inclMaxNormSingleDosagePrism


        let setInclMaxNormSingleDosage = Optic.set inclMaxNormSingleDosagePrism


        let exclMaxNormSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMaxNormLens


        let getExclMaxNormSingleDosage = Optic.get exclMaxNormSingleDosagePrism


        let setExclMaxNormSingleDosage = Optic.set exclMaxNormSingleDosagePrism

        
        let normWeightUnitSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.normWeightUnitLens


        let getNormWeightUnitSingleDosage = Optic.get normWeightUnitSingleDosagePrism


        let setNormWeightUnitSingleDosage = Optic.set normWeightUnitSingleDosagePrism


        let inclMinNormWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMinNormWeightLens


        let getInclMinNormWeightSingleDosage = Optic.get inclMinNormWeightSingleDosagePrism


        let setInclMinNormWeightSingleDosage = Optic.set inclMinNormWeightSingleDosagePrism


        let exclMinNormWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMinNormWeightLens


        let getExclMinNormWeightSingleDosage = Optic.get exclMinNormWeightSingleDosagePrism


        let setExclMinNormWeightSingleDosage = Optic.set exclMinNormWeightSingleDosagePrism


        let inclMaxNormWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMaxNormWeightLens


        let getInclMaxNormWeightSingleDosage = Optic.get inclMaxNormWeightSingleDosagePrism


        let setInclMaxNormWeightSingleDosage = Optic.set inclMaxNormWeightSingleDosagePrism


        let exclMaxNormWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMaxNormWeightLens


        let getExclMaxNormWeightSingleDosage = Optic.get exclMaxNormWeightSingleDosagePrism


        let setExclMaxNormWeightSingleDosage = Optic.set exclMaxNormWeightSingleDosagePrism

        
        let normBSAUnitSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.normBSAUnitLens


        let getNormBSAUnitSingleDosage = Optic.get normBSAUnitSingleDosagePrism


        let setNormBSAUnitSingleDosage = Optic.set normBSAUnitSingleDosagePrism


        let inclMinNormBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMinNormBSALens


        let getInclMinNormBSASingleDosage = Optic.get inclMinNormBSASingleDosagePrism


        let setInclMinNormBSASingleDosage = Optic.set inclMinNormBSASingleDosagePrism


        let exclMinNormBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMinNormBSALens


        let getExclMinNormBSASingleDosage = Optic.get exclMinNormBSASingleDosagePrism


        let setExclMinNormBSASingleDosage = Optic.set exclMinNormBSASingleDosagePrism


        let inclMaxNormBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMaxNormBSALens


        let getInclMaxNormBSASingleDosage = Optic.get inclMaxNormBSASingleDosagePrism


        let setInclMaxNormBSASingleDosage = Optic.set inclMaxNormBSASingleDosagePrism


        let exclMaxNormBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMaxNormBSALens


        let getExclMaxNormBSASingleDosage = Optic.get exclMaxNormBSASingleDosagePrism


        let setExclMaxNormBSASingleDosage = Optic.set exclMaxNormBSASingleDosagePrism


        let rateUnitRageDosagePrism =
            Dosage.RateDosage_ >-> snd_

        
        let getRateUnitRateDosage = Optic.get rateUnitRageDosagePrism


        let setRateUnitRateDosage = Optic.set rateUnitRageDosagePrism


        let normWeightUnitRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.normWeightUnitLens


        let getNormWeightUnitRateDosage = Optic.get normWeightUnitRateDosagePrism


        let setNormWeightUnitRateDosage = Optic.set normWeightUnitRateDosagePrism


        let inclMinNormRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinNormLens


        let getInclMinNormRateDosage = Optic.get inclMinNormRateDosagePrism


        let setInclMinNormRateDosage = Optic.set inclMinNormRateDosagePrism


        let exclMinNormRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinNormLens


        let getExclMinNormRateDosage = Optic.get exclMinNormRateDosagePrism


        let setExclMinNormRateDosage = Optic.set exclMinNormRateDosagePrism


        let inclMaxNormRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxNormLens


        let getInclMaxNormRateDosage = Optic.get inclMaxNormRateDosagePrism


        let setInclMaxNormRateDosage = Optic.set inclMaxNormRateDosagePrism


        let exclMaxNormRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxNormLens


        let getExclMaxNormRateDosage = Optic.get exclMaxNormRateDosagePrism


        let setExclMaxNormRateDosage = Optic.set exclMaxNormRateDosagePrism


        let inclMinNormWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinNormWeightLens


        let getInclMinNormWeightRateDosage = Optic.get inclMinNormWeightRateDosagePrism


        let setInclMinNormWeightRateDosage = Optic.set inclMinNormWeightRateDosagePrism


        let exclMinNormWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinNormWeightLens


        let getExclMinNormWeightRateDosage = Optic.get exclMinNormWeightRateDosagePrism


        let setExclMinNormWeightRateDosage = Optic.set exclMinNormWeightRateDosagePrism


        let inclMaxNormWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxNormWeightLens


        let getInclMaxNormWeightRateDosage = Optic.get inclMaxNormWeightRateDosagePrism


        let setInclMaxNormWeightRateDosage = Optic.set inclMaxNormWeightRateDosagePrism


        let exclMaxNormWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxNormWeightLens


        let getExclMaxNormWeightRateDosage = Optic.get exclMaxNormWeightRateDosagePrism


        let setExclMaxNormWeightRateDosage = Optic.set exclMaxNormWeightRateDosagePrism


        let normBSAUnitRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.normBSAUnitLens


        let getNormBSAUnitRateDosage = Optic.get normBSAUnitRateDosagePrism


        let setNormBSAUnitRateDosage = Optic.set normBSAUnitRateDosagePrism


        let inclMinNormBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinNormBSALens


        let getInclMinNormBSARateDosage = Optic.get inclMinNormBSARateDosagePrism


        let setInclMinNormBSARateDosage = Optic.set inclMinNormBSARateDosagePrism


        let exclMinNormBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinNormBSALens


        let getExclMinNormBSARateDosage = Optic.get exclMinNormBSARateDosagePrism


        let setExclMinNormBSARateDosage = Optic.set exclMinNormBSARateDosagePrism


        let inclMaxNormBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxNormBSALens


        let getInclMaxNormBSARateDosage = Optic.get inclMaxNormBSARateDosagePrism


        let setInclMaxNormBSARateDosage = Optic.set inclMaxNormBSARateDosagePrism


        let exclMaxNormBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxNormBSALens


        let getExclMaxNormBSARateDosage = Optic.get exclMaxNormBSARateDosagePrism


        let setExclMaxNormBSARateDosage = Optic.set exclMaxNormBSARateDosagePrism


        let timeUnitTotalDosagePrism =
            Dosage.TotalDosage_ >-> snd_

        
        let getTimeUnitTotalDosage = Optic.get timeUnitTotalDosagePrism


        let setTimeUnitTotalDosage = Optic.set timeUnitTotalDosagePrism


        let normWeightUnitTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.normWeightUnitLens


        let getNormWeightUnitTotalDosage = Optic.get normWeightUnitTotalDosagePrism


        let setNormWeightUnitTotalDosage = Optic.set normWeightUnitTotalDosagePrism


        let inclMinNormTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinNormLens


        let getInclMinNormTotalDosage = Optic.get inclMinNormTotalDosagePrism


        let setInclMinNormTotalDosage = Optic.set inclMinNormTotalDosagePrism


        let exclMinNormTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinNormLens


        let getExclMinNormTotalDosage = Optic.get exclMinNormTotalDosagePrism


        let setExclMinNormTotalDosage = Optic.set exclMinNormTotalDosagePrism


        let inclMaxNormTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxNormLens


        let getInclMaxNormTotalDosage = Optic.get inclMaxNormTotalDosagePrism


        let setInclMaxNormTotalDosage = Optic.set inclMaxNormTotalDosagePrism


        let exclMaxNormTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxNormLens


        let getExclMaxNormTotalDosage = Optic.get exclMaxNormTotalDosagePrism


        let setExclMaxNormTotalDosage = Optic.set exclMaxNormTotalDosagePrism


        let inclMinNormWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinNormWeightLens


        let getInclMinNormWeightTotalDosage = Optic.get inclMinNormWeightTotalDosagePrism


        let setInclMinNormWeightTotalDosage = Optic.set inclMinNormWeightTotalDosagePrism


        let exclMinNormWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinNormWeightLens


        let getExclMinNormWeightTotalDosage = Optic.get exclMinNormWeightTotalDosagePrism


        let setExclMinNormWeightTotalDosage = Optic.set exclMinNormWeightTotalDosagePrism


        let inclMaxNormWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxNormWeightLens


        let getInclMaxNormWeightTotalDosage = Optic.get inclMaxNormWeightTotalDosagePrism


        let setInclMaxNormWeightTotalDosage = Optic.set inclMaxNormWeightTotalDosagePrism


        let exclMaxNormWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxNormWeightLens


        let getExclMaxNormWeightTotalDosage = Optic.get exclMaxNormWeightTotalDosagePrism


        let setExclMaxNormWeightTotalDosage = Optic.set exclMaxNormWeightTotalDosagePrism


        let inclMinNormBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinNormBSALens


        let getInclMinNormBSATotalDosage = Optic.get inclMinNormBSATotalDosagePrism


        let setInclMinNormBSATotalDosage = Optic.set inclMinNormBSATotalDosagePrism


        let exclMinNormBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinNormBSALens


        let getExclMinNormBSATotalDosage = Optic.get exclMinNormBSATotalDosagePrism


        let setExclMinNormBSATotalDosage = Optic.set exclMinNormBSATotalDosagePrism


        let inclMaxNormBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxNormBSALens


        let getInclMaxNormBSATotalDosage = Optic.get inclMaxNormBSATotalDosagePrism


        let setInclMaxNormBSATotalDosage = Optic.set inclMaxNormBSATotalDosagePrism


        let exclMaxNormBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxNormBSALens


        let getExclMaxNormBSATotalDosage = Optic.get exclMaxNormBSATotalDosagePrism


        let setExclMaxNormBSATotalDosage = Optic.set exclMaxNormBSATotalDosagePrism


        let inclMinAbsStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMinAbsLens


        let getInclMinAbsStartDosage = Optic.get inclMinAbsStartDosagePrism


        let setInclMinAbsStartDosage = Optic.set inclMinAbsStartDosagePrism


        let exclMinAbsStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMinAbsLens


        let getExclMinAbsStartDosage = Optic.get exclMinAbsStartDosagePrism


        let setExclMinAbsStartDosage = Optic.set exclMinAbsStartDosagePrism


        let inclMaxAbsStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMaxAbsLens


        let getInclMaxAbsStartDosage = Optic.get inclMaxAbsStartDosagePrism


        let setInclMaxAbsStartDosage = Optic.set inclMaxAbsStartDosagePrism


        let exclMaxAbsStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMaxAbsLens


        let getExclMaxAbsStartDosage = Optic.get exclMaxAbsStartDosagePrism


        let setExclMaxAbsStartDosage = Optic.set exclMaxAbsStartDosagePrism


        let absWeightUnitStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.absWeightUnitLens


        let getAbsWeightUnitStartDosage = Optic.get absWeightUnitStartDosagePrism


        let setAbsWeightUnitStartDosage = Optic.set absWeightUnitStartDosagePrism


        let inclMinAbsWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMinAbsWeightLens


        let getInclMinAbsWeightStartDosage = Optic.get inclMinAbsWeightStartDosagePrism


        let setInclMinAbsWeightStartDosage = Optic.set inclMinAbsWeightStartDosagePrism


        let exclMinAbsWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMinAbsWeightLens


        let getExclMinAbsWeightStartDosage = Optic.get exclMinAbsWeightStartDosagePrism


        let setExclMinAbsWeightStartDosage = Optic.set exclMinAbsWeightStartDosagePrism


        let inclMaxAbsWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMaxAbsWeightLens


        let getInclMaxAbsWeightStartDosage = Optic.get inclMaxAbsWeightStartDosagePrism


        let setInclMaxAbsWeightStartDosage = Optic.set inclMaxAbsWeightStartDosagePrism


        let exclMaxAbsWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMaxAbsWeightLens


        let getExclMaxAbsWeightStartDosage = Optic.get exclMaxAbsWeightStartDosagePrism


        let setExclMaxAbsWeightStartDosage = Optic.set exclMaxAbsWeightStartDosagePrism


        let absBSAUnitStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.absBSAUnitLens


        let getAbsBSAUnitStartDosage = Optic.get absBSAUnitStartDosagePrism


        let setAbsBSAUnitStartDosage = Optic.set absBSAUnitStartDosagePrism


        let inclMinAbsBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMinAbsBSALens


        let getInclMinAbsBSAStartDosage = Optic.get inclMinAbsBSAStartDosagePrism


        let setInclMinAbsBSAStartDosage = Optic.set inclMinAbsBSAStartDosagePrism


        let exclMinAbsBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMinAbsBSALens


        let getExclMinAbsBSAStartDosage = Optic.get exclMinAbsBSAStartDosagePrism


        let setExclMinAbsBSAStartDosage = Optic.set exclMinAbsBSAStartDosagePrism


        let inclMaxAbsBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMaxAbsBSALens


        let getInclMaxAbsBSAStartDosage = Optic.get inclMaxAbsBSAStartDosagePrism


        let setInclMaxAbsBSAStartDosage = Optic.set inclMaxAbsBSAStartDosagePrism


        let exclMaxAbsBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMaxAbsBSALens


        let getExclMaxAbsBSAStartDosage = Optic.get exclMaxAbsBSAStartDosagePrism


        let setExclMaxAbsBSAStartDosage = Optic.set exclMaxAbsBSAStartDosagePrism
    

        let inclMinAbsSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMinAbsLens


        let getInclMinAbsSingleDosage = Optic.get inclMinAbsSingleDosagePrism


        let setInclMinAbsSingleDosage = Optic.set inclMinAbsSingleDosagePrism


        let exclMinAbsSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMinAbsLens


        let getExclMinAbsSingleDosage = Optic.get exclMinAbsSingleDosagePrism


        let setExclMinAbsSingleDosage = Optic.set exclMinAbsSingleDosagePrism


        let inclMaxAbsSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMaxAbsLens


        let getInclMaxAbsSingleDosage = Optic.get inclMaxAbsSingleDosagePrism


        let setInclMaxAbsSingleDosage = Optic.set inclMaxAbsSingleDosagePrism


        let exclMaxAbsSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMaxAbsLens


        let getExclMaxAbsSingleDosage = Optic.get exclMaxAbsSingleDosagePrism


        let setExclMaxAbsSingleDosage = Optic.set exclMaxAbsSingleDosagePrism


        let absWeightUnitSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.absWeightUnitLens


        let getAbsWeightUnitSingleDosage = Optic.get absWeightUnitSingleDosagePrism


        let setAbsWeightUnitSingleDosage = Optic.set absWeightUnitSingleDosagePrism


        let inclMinAbsWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMinAbsWeightLens


        let getInclMinAbsWeightSingleDosage = Optic.get inclMinAbsWeightSingleDosagePrism


        let setInclMinAbsWeightSingleDosage = Optic.set inclMinAbsWeightSingleDosagePrism


        let exclMinAbsWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMinAbsWeightLens


        let getExclMinAbsWeightSingleDosage = Optic.get exclMinAbsWeightSingleDosagePrism


        let setExclMinAbsWeightSingleDosage = Optic.set exclMinAbsWeightSingleDosagePrism


        let inclMaxAbsWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMaxAbsWeightLens


        let getInclMaxAbsWeightSingleDosage = Optic.get inclMaxAbsWeightSingleDosagePrism


        let setInclMaxAbsWeightSingleDosage = Optic.set inclMaxAbsWeightSingleDosagePrism


        let exclMaxAbsWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMaxAbsWeightLens


        let getExclMaxAbsWeightSingleDosage = Optic.get exclMaxAbsWeightSingleDosagePrism


        let setExclMaxAbsWeightSingleDosage = Optic.set exclMaxAbsWeightSingleDosagePrism


        let absBSAUnitSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.absBSAUnitLens


        let getAbsBSAUnitSingleDosage = Optic.get absBSAUnitSingleDosagePrism


        let setAbsBSAUnitSingleDosage = Optic.set absBSAUnitSingleDosagePrism


        let inclMinAbsBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMinAbsBSALens


        let getInclMinAbsBSASingleDosage = Optic.get inclMinAbsBSASingleDosagePrism


        let setInclMinAbsBSASingleDosage = Optic.set inclMinAbsBSASingleDosagePrism


        let exclMinAbsBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMinAbsBSALens


        let getExclMinAbsBSASingleDosage = Optic.get exclMinAbsBSASingleDosagePrism


        let setExclMinAbsBSASingleDosage = Optic.set exclMinAbsBSASingleDosagePrism


        let inclMaxAbsBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMaxAbsBSALens


        let getInclMaxAbsBSASingleDosage = Optic.get inclMaxAbsBSASingleDosagePrism


        let setInclMaxAbsBSASingleDosage = Optic.set inclMaxAbsBSASingleDosagePrism


        let exclMaxAbsBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMaxAbsBSALens


        let getExclMaxAbsBSASingleDosage = Optic.get exclMaxAbsBSASingleDosagePrism


        let setExclMaxAbsBSASingleDosage = Optic.set exclMaxAbsBSASingleDosagePrism


        let inclMinAbsRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinAbsLens


        let getInclMinAbsRateDosage = Optic.get inclMinAbsRateDosagePrism


        let setInclMinAbsRateDosage = Optic.set inclMinAbsRateDosagePrism


        let exclMinAbsRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinAbsLens


        let getExclMinAbsRateDosage = Optic.get exclMinAbsRateDosagePrism


        let setExclMinAbsRateDosage = Optic.set exclMinAbsRateDosagePrism


        let inclMaxAbsRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxAbsLens


        let getInclMaxAbsRateDosage = Optic.get inclMaxAbsRateDosagePrism


        let setInclMaxAbsRateDosage = Optic.set inclMaxAbsRateDosagePrism


        let exclMaxAbsRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxAbsLens


        let getExclMaxAbsRateDosage = Optic.get exclMaxAbsRateDosagePrism


        let setExclMaxAbsRateDosage = Optic.set exclMaxAbsRateDosagePrism


        let absWeightUnitRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.absWeightUnitLens


        let getAbsWeightUnitRateDosage = Optic.get absWeightUnitRateDosagePrism


        let setAbsWeightUnitRateDosage = Optic.set absWeightUnitRateDosagePrism


        let inclMinAbsWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinAbsWeightLens


        let getInclMinAbsWeightRateDosage = Optic.get inclMinAbsWeightRateDosagePrism


        let setInclMinAbsWeightRateDosage = Optic.set inclMinAbsWeightRateDosagePrism


        let exclMinAbsWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinAbsWeightLens


        let getExclMinAbsWeightRateDosage = Optic.get exclMinAbsWeightRateDosagePrism


        let setExclMinAbsWeightRateDosage = Optic.set exclMinAbsWeightRateDosagePrism


        let inclMaxAbsWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxAbsWeightLens


        let getInclMaxAbsWeightRateDosage = Optic.get inclMaxAbsWeightRateDosagePrism


        let setInclMaxAbsWeightRateDosage = Optic.set inclMaxAbsWeightRateDosagePrism


        let exclMaxAbsWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxAbsWeightLens


        let getExclMaxAbsWeightRateDosage = Optic.get exclMaxAbsWeightRateDosagePrism


        let setExclMaxAbsWeightRateDosage = Optic.set exclMaxAbsWeightRateDosagePrism


        let absBSAUnitRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.absBSAUnitLens


        let getAbsBSAUnitRateDosage = Optic.get absBSAUnitRateDosagePrism


        let setAbsBSAUnitRateDosage = Optic.set absBSAUnitRateDosagePrism


        let inclMinAbsBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinAbsBSALens


        let getInclMinAbsBSARateDosage = Optic.get inclMinAbsBSARateDosagePrism


        let setInclMinAbsBSARateDosage = Optic.set inclMinAbsBSARateDosagePrism


        let exclMinAbsBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinAbsBSALens


        let getExclMinAbsBSARateDosage = Optic.get exclMinAbsBSARateDosagePrism


        let setExclMinAbsBSARateDosage = Optic.set exclMinAbsBSARateDosagePrism


        let inclMaxAbsBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxAbsBSALens


        let getInclMaxAbsBSARateDosage = Optic.get inclMaxAbsBSARateDosagePrism


        let setInclMaxAbsBSARateDosage = Optic.set inclMaxAbsBSARateDosagePrism


        let exclMaxAbsBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxAbsBSALens


        let getExclMaxAbsBSARateDosage = Optic.get exclMaxAbsBSARateDosagePrism


        let setExclMaxAbsBSARateDosage = Optic.set exclMaxAbsBSARateDosagePrism


        let inclMinAbsTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinAbsLens


        let getInclMinAbsTotalDosage = Optic.get inclMinAbsTotalDosagePrism


        let setInclMinAbsTotalDosage = Optic.set inclMinAbsTotalDosagePrism


        let exclMinAbsTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinAbsLens


        let getExclMinAbsTotalDosage = Optic.get exclMinAbsTotalDosagePrism


        let setExclMinAbsTotalDosage = Optic.set exclMinAbsTotalDosagePrism


        let inclMaxAbsTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxAbsLens


        let getInclMaxAbsTotalDosage = Optic.get inclMaxAbsTotalDosagePrism


        let setInclMaxAbsTotalDosage = Optic.set inclMaxAbsTotalDosagePrism


        let exclMaxAbsTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxAbsLens


        let getExclMaxAbsTotalDosage = Optic.get exclMaxAbsTotalDosagePrism


        let setExclMaxAbsTotalDosage = Optic.set exclMaxAbsTotalDosagePrism


        let absWeightUnitTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.absWeightUnitLens


        let getAbsWeightUnitTotalDosage = Optic.get absWeightUnitTotalDosagePrism


        let setAbsWeightUnitTotalDosage = Optic.set absWeightUnitTotalDosagePrism


        let inclMinAbsWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinAbsWeightLens


        let getInclMinAbsWeightTotalDosage = Optic.get inclMinAbsWeightTotalDosagePrism


        let setInclMinAbsWeightTotalDosage = Optic.set inclMinAbsWeightTotalDosagePrism


        let exclMinAbsWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinAbsWeightLens


        let getExclMinAbsWeightTotalDosage = Optic.get exclMinAbsWeightTotalDosagePrism


        let setExclMinAbsWeightTotalDosage = Optic.set exclMinAbsWeightTotalDosagePrism


        let inclMaxAbsWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxAbsWeightLens


        let getInclMaxAbsWeightTotalDosage = Optic.get inclMaxAbsWeightTotalDosagePrism


        let setInclMaxAbsWeightTotalDosage = Optic.set inclMaxAbsWeightTotalDosagePrism


        let exclMaxAbsWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxAbsWeightLens


        let getExclMaxAbsWeightTotalDosage = Optic.get exclMaxAbsWeightTotalDosagePrism


        let setExclMaxAbsWeightTotalDosage = Optic.set exclMaxAbsWeightTotalDosagePrism


        let absBSAUnitTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.absBSAUnitLens


        let getAbsBSAUnitTotalDosage = Optic.get absBSAUnitTotalDosagePrism


        let setAbsBSAUnitTotalDosage = Optic.set absBSAUnitTotalDosagePrism


        let inclMinAbsBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinAbsBSALens


        let getInclMinAbsBSATotalDosage = Optic.get inclMinAbsBSATotalDosagePrism


        let setInclMinAbsBSATotalDosage = Optic.set inclMinAbsBSATotalDosagePrism


        let exclMinAbsBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinAbsBSALens


        let getExclMinAbsBSATotalDosage = Optic.get exclMinAbsBSATotalDosagePrism


        let setExclMinAbsBSATotalDosage = Optic.set exclMinAbsBSATotalDosagePrism


        let inclMaxAbsBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxAbsBSALens


        let getInclMaxAbsBSATotalDosage = Optic.get inclMaxAbsBSATotalDosagePrism


        let setInclMaxAbsBSATotalDosage = Optic.set inclMaxAbsBSATotalDosagePrism


        let exclMaxAbsBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxAbsBSALens


        let getExclMaxAbsBSATotalDosage = Optic.get exclMaxAbsBSATotalDosagePrism


        let setExclMaxAbsBSATotalDosage = Optic.set exclMaxAbsBSATotalDosagePrism



    let toString ({ StartDosage = start; SingleDosage = single; RateDosage = rate; TotalDosage = total; Frequencies = freqs }) =
        let (>+) sl sr = 
            let l, s = sr
            
            if s |> String.isNullOrWhiteSpace then sl
            else l + "\n" + s + (if sl |> String.isNullOrWhiteSpace then sl else "\n" + sl)
            
        let rt, ru = rate
        let tt, tu = total
        
        let ru = if ru |> String.isNullOrWhiteSpace then ru else "/" + ru
        let tu = if tu |> String.isNullOrWhiteSpace then tu else "/" + tu
        
        ""
        >+ ("Start dosering: ", start |> DoseRange.toString)
        >+ ("Keer dosering: ", single |> DoseRange.toString)
        >+ ("Continue dosering: ", (rt |> DoseRange.toString) + " " + ru)
        >+ ("Totaal dosering: ", (tt |> DoseRange.toString) + " " + tu)
        |> (fun s -> 
            if freqs |> List.isEmpty then s
            else
                let tu = "dag"
                sprintf "%s\nToegestande frequenties: %s keer per %s" s (freqs |> List.toString) tu
        )



module Patient = 


    type MinMax = MinMax.MinMax


    type Patient =
        {
            GestAge : MinMax
            Age : MinMax
            Weight : MinMax
            BSA : MinMax
            Gender : Gender
        }
    and Gender = Male | Female | Undetermined


    let create ga age wght bsa gend =
        {
            GestAge = ga
            Age = age
            Weight = wght
            BSA = bsa
            Gender = gend
        }


    let empty = create MinMax.empty MinMax.empty MinMax.empty MinMax.empty Undetermined


    type Patient with

        static member GestAge_ : 
            (Patient -> MinMax) * (MinMax -> Patient -> Patient) =
            (fun p -> p.GestAge), (fun a p -> { p with GestAge = a })

        static member Age_ : 
            (Patient -> MinMax) * (MinMax -> Patient -> Patient) =
            (fun p -> p.Age), (fun a p -> { p with Age = a })

        static member Weight_ : 
            (Patient -> MinMax) * (MinMax -> Patient -> Patient) =
            (fun p -> p.Weight), (fun w p -> { p with Weight = w })

        static member BSA_ : 
            (Patient -> MinMax) * (MinMax -> Patient -> Patient) =
            (fun p -> p.BSA), (fun b p -> { p with BSA = b })

        static member Gender_ : 
            (Patient -> Gender) * (Gender -> Patient -> Patient) =
            (fun p -> p.Gender), (fun g p -> { p with Gender = g })



    module Props =
    
        module MinMax = MinMax.Props

        let getGestAge = Optic.get Patient.GestAge_


        let setGestAge = Optic.set Patient.GestAge_


        let inclMinGestAge =
            Patient.GestAge_ >-> MinMax.inclMinLens


        let getInclMinGestAge = Optic.get inclMinGestAge


        let setInclMinGestAge = Optic.set inclMinGestAge


        let exclMinGestAge =
            Patient.GestAge_ >-> MinMax.exclMinLens


        let getExclMinGestAge = Optic.get exclMinGestAge


        let setExclMinGestAge = Optic.set exclMinGestAge


        let inclMaxGestAge =
            Patient.GestAge_ >-> MinMax.inclMaxLens


        let getInclMaxGestAge = Optic.get inclMaxGestAge


        let setInclMaxGestAge = Optic.set inclMaxGestAge


        let exclMaxGestAge =
            Patient.GestAge_ >-> MinMax.exclMaxLens


        let getExclMaxGestAge = Optic.get exclMaxGestAge


        let setExclMaxGestAge = Optic.set exclMaxGestAge


        let getAge = Optic.get Patient.Age_


        let setAge = Optic.set Patient.Age_


        let inclMinAge =
            Patient.Age_ >-> MinMax.inclMinLens


        let getInclMinAge = Optic.get inclMinAge


        let setInclMinAge = Optic.set inclMinAge


        let exclMinAge =
            Patient.Age_ >-> MinMax.exclMinLens


        let getExclMinAge = Optic.get exclMinAge


        let setExclMinAge = Optic.set exclMinAge


        let inclMaxAge =
            Patient.Age_ >-> MinMax.inclMaxLens


        let getInclMaxAge = Optic.get inclMaxAge


        let setInclMaxAge = Optic.set inclMaxAge


        let exclMaxAge =
            Patient.Age_ >-> MinMax.exclMaxLens


        let getExclMaxAge = Optic.get exclMaxAge


        let setExclMaxAge = Optic.set exclMaxAge


        let getWeight = Optic.get Patient.Weight_


        let setWeight = Optic.set Patient.Weight_


        let inclMinWeight =
            Patient.Weight_ >-> MinMax.inclMinLens


        let getInclMinWeight = Optic.get inclMinWeight


        let setInclMinWeight = Optic.set inclMinWeight


        let exclMinWeight =
            Patient.Weight_ >-> MinMax.exclMinLens


        let getExclMinWeight = Optic.get exclMinWeight


        let setExclMinWeight = Optic.set exclMinWeight


        let inclMaxWeight =
            Patient.Weight_ >-> MinMax.inclMaxLens


        let getInclMaxWeight = Optic.get inclMaxWeight


        let setInclMaxWeight = Optic.set inclMaxWeight


        let exclMaxWeight =
            Patient.Weight_ >-> MinMax.exclMaxLens


        let getExclMaxWeight = Optic.get exclMaxWeight


        let setExclMaxWeight = Optic.set exclMaxWeight


        let getBSA = Optic.get Patient.BSA_


        let setBSA = Optic.set Patient.BSA_


        let inclMinBSA =
            Patient.BSA_ >-> MinMax.inclMinLens


        let getInclMinBSA = Optic.get inclMinBSA


        let setInclMinBSA = Optic.set inclMinBSA


        let exclMinBSA =
            Patient.BSA_ >-> MinMax.exclMinLens


        let getExclMinBSA = Optic.get exclMinBSA


        let setExclMinBSA = Optic.set exclMinBSA


        let inclMaxBSA =
            Patient.BSA_ >-> MinMax.inclMaxLens


        let getInclMaxBSA = Optic.get inclMaxBSA


        let setInclMaxBSA = Optic.set inclMaxBSA


        let exclMaxBSA =
            Patient.BSA_ >-> MinMax.exclMaxLens


        let getExclMaxBSA = Optic.get exclMaxBSA


        let setExclMaxBSA = Optic.set exclMaxBSA


        let getGender = Optic.get Patient.Gender_
    
    
        let setGender = Optic.set Patient.Gender_


    let genderToString = function
    | Male -> "man"
    | Female -> "vrouw"
    | Undetermined -> ""


    let toString ({ GestAge = ga; Age = age; Weight = wght; BSA = bsa; Gender = gen }) =
        let (>+) sl sr = 
            let l, s = sr
            
            if s |> String.isNullOrWhiteSpace then sl
            else sl + " " + l + s
        
        "Patient: "
        >+ ("Zwangerschapsduur: ", ga |> MinMax.toString)
        >+ ("Leeftijd: ", age |> MinMax.toString)
        >+ ("Gewicht: ", wght |> MinMax.toString)
        >+ ("BSA: ", bsa |> MinMax.toString)
        >+ ("Geslacht: ", gen |> genderToString)



module DoseRule =


    type Dosage = Dosage.Dosage
    type MinMax = MinMax.MinMax
    type Patient = Patient.Patient


    /// Doserule
    type DoseRule =
        {   
            // Generic the doserule applies to
            Generic : string
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
    and IndicationDosage =
        {
            // The indication(-s) the dose rule applies to
            Indications : string list
            // The dosage rules per administration route
            RouteDosages : RouteDosage list
        }
    and RouteDosage =
        {
            // Administration route
            Route : string
            // TradeProducts the doserule applies to
            TradeProducts : string list
            // GenericProducts the doserule applies to
            GenericProducts : string list
            // The dosage rules per patient group
            PatientDosages : PatientDosage list
        } 
    and PatientDosage =
        {
            // The patient group the doserules applies
            Patient : Patient
            // List of shapes that have a dosage
            ShapeDosages : Dosage list
            // List of substances that have a dosage
            SubstanceDosages : Dosage list
        }


    let doseRule gen atc thg sub ggp gsg tps gps idl =
        {   
            Generic = gen
            ATC = atc
            ATCTherapyGroup = thg
            ATCTherapySubGroup = sub
            GenericGroup = ggp
            GenericSubGroup = gsg
            IndicationsDosages = idl
        }


    let createIndicationsDosages inds dr =
        { Indications = inds; RouteDosages = [] }::dr.IndicationsDosages


    let addIndications inds (dr : DoseRule) =
        {
            dr with 
                IndicationsDosages =
                    dr |> createIndicationsDosages inds
        }

 
    type PatientDosage with
    
        static member Patient_ :
            (PatientDosage -> Patient) * (Patient -> PatientDosage -> PatientDosage) =
            (fun pd -> pd.Patient) ,
            (fun pat pd -> { pd with Patient = pat })
    
        static member ShapeDosages_ :
            (PatientDosage -> Dosage list) * (Dosage list -> PatientDosage -> PatientDosage) =
            (fun pd -> pd.ShapeDosages) ,
            (fun sd pd -> { pd with ShapeDosages = sd })
    
        static member SubstanceDosages_ :
            (PatientDosage -> Dosage list) * (Dosage list -> PatientDosage -> PatientDosage) =
            (fun pd -> pd.SubstanceDosages) ,
            (fun sd pd -> { pd with SubstanceDosages = sd })
 
        
    type RouteDosage with
    
        static member Route_ :
            (RouteDosage -> string) * (string -> RouteDosage -> RouteDosage) =
            (fun rd -> rd.Route) ,
            (fun s rd -> { rd with Route = s })
            
        static member PatientDosages_ :
            (RouteDosage -> PatientDosage list) * (PatientDosage list -> RouteDosage -> RouteDosage) =
            (fun rd -> rd.PatientDosages) ,
            (fun pdl rd -> { rd with PatientDosages = pdl })            
            
        
    type IndicationDosage with
    
        static member Indications_ :
            (IndicationDosage -> string list) * (string list -> IndicationDosage -> IndicationDosage) =
            (fun inds -> inds.Indications) ,
            (fun sl inds -> { inds with Indications = sl })
    
        static member RouteDosages_ :
            (IndicationDosage -> RouteDosage list) * (RouteDosage list -> IndicationDosage -> IndicationDosage) =
            (fun inds -> inds.RouteDosages) ,
            (fun rdl inds -> { inds with RouteDosages = rdl })

    
    type DoseRule with
    
        static member Generic_ :
            (DoseRule -> string) * (string -> DoseRule -> DoseRule) =
            (fun dr -> dr.Generic),
            (fun s dr -> { dr with Generic = s })

        static member IndicationDosages_ :
            (DoseRule -> IndicationDosage list) * (IndicationDosage list -> DoseRule -> DoseRule) =
            (fun dr -> dr.IndicationsDosages) ,
            (fun inds dr -> { dr with IndicationsDosages = inds })


    module Props =
        
        module Patient = Patient.Props
        module Dosage = Dosage.Props


        let getGeneric = Optic.get DoseRule.Generic_
    
    
        let setGeneric = Optic.set DoseRule.Generic_

    
        let getIndicationsDosages = Optic.get DoseRule.IndicationDosages_
    
    
        let setIndicationsDosages = Optic.set DoseRule.IndicationDosages_
    
    
        let indDosIndicationsPrism n =
            DoseRule.IndicationDosages_ >-> List.pos_ n >?> IndicationDosage.Indications_


        let getIndications n dr = 
            match dr |> Optic.get (indDosIndicationsPrism n) with 
            | Some ids -> ids
            | None     -> []
    
    
        let setIndications n = Optic.set (indDosIndicationsPrism n)
    
    
        let indDosDosagesLens n =
            DoseRule.IndicationDosages_ >-> List.pos_ n >?> IndicationDosage.RouteDosages_


        let getRouteDosages inds (dr : DoseRule) =
            match 
                dr.IndicationsDosages
                |> List.tryFindIndex (fun id -> id.Indications = inds) with 
            | Some n -> 
                match dr |> Optic.get (indDosDosagesLens n) with
                | Some drs -> drs
                | None -> []
            | None -> []
        
    
        let setRouteDosages inds rds (dr : DoseRule) =
            match 
                dr.IndicationsDosages
                |> List.tryFindIndex (fun id -> id.Indications = inds) with 
                | Some n -> dr |> Optic.set (indDosDosagesLens n) rds
                | None -> dr
            
        
        let indxIndications inds (dr : DoseRule) =
            dr.IndicationsDosages
            |> List.tryFindIndex (fun id -> id.Indications = inds)       
        
    
        let addRoute inds rt (dr : DoseRule) =
            let rds = [ { Route = rt; GenericProducts = []; TradeProducts = []; PatientDosages = [] } ]
        
            match 
                dr |> indxIndications inds with 
                | Some n -> 
                    dr 
                    |> Optic.set (indDosDosagesLens n) (dr |> getRouteDosages inds |> List.append rds)
                | None -> dr


        let routeDosPatientDosagesPrism n1 n2 =
            (indDosDosagesLens n1) >?> List.pos_ n2 >?> RouteDosage.PatientDosages_
        

        let indxRoute rt (ind : IndicationDosage) =
            ind.RouteDosages
            |> List.tryFindIndex (fun id -> id.Route = rt)       
        
        
        let getRoutePatientDosages inds rt (dr : DoseRule) =

            match dr |> indxIndications inds with 
            | Some n1 -> 
                match 
                    dr.IndicationsDosages.[n1]
                    |> indxRoute rt with 
                | Some n2 ->
                    match dr 
                          |> Optic.get (routeDosPatientDosagesPrism n1 n2) with 
                    | Some pds -> pds
                    | None -> []              
                | None -> []
            | None -> []    
        
        
        let setRoutePatientDosages inds rt pds (dr : DoseRule) =
        
            match dr |> indxIndications inds with 
            | Some n1 -> 
                match 
                    dr.IndicationsDosages.[n1]
                    |> indxRoute rt with 
                | Some n2 ->
                    dr |> Optic.set (routeDosPatientDosagesPrism n1 n2) pds
                | None -> dr
            | None -> dr
        
        
        let addPatient inds rt pat (dr : DoseRule) =
        
            let pds =
                dr
                |> getRoutePatientDosages inds rt
                |> List.append [ { Patient = pat; ShapeDosages = []; SubstanceDosages = [] } ]
        
            dr 
            |> setRoutePatientDosages inds rt pds
    
    
        let indxPatient pat (rtd : RouteDosage) =
            rtd.PatientDosages
            |> List.tryFindIndex (fun rd -> rd.Patient = pat)
        
    
        let patDosPatientPrism n1 n2 n3 =
            (routeDosPatientDosagesPrism n1 n2) >?> List.pos_ n3 >?> PatientDosage.Patient_
    
    
        let private patientGetter prism inds rt pat dr = 
            match dr |> indxIndications inds with 
            | Some n1 ->
                match dr.IndicationsDosages.[n1] |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        dr |> Optic.get ((patDosPatientPrism n1 n2 n3) >?> prism)
                    | None -> None
                | None -> None
            | None -> None
    
    
        let private patientSetter prism inds rt vu pat dr = 
            match dr |> indxIndications inds with 
            | Some n1 ->
                match dr.IndicationsDosages.[n1] |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        let pat = 
                            pat |> prism vu
                        dr |> Optic.set ((patDosPatientPrism n1 n2 n3)) pat, pat
                    | None -> dr, pat
                | None -> dr, pat
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
    
    
        let shapeDosagesPrism n1 n2 n3 =
            (routeDosPatientDosagesPrism n1 n2) >?> List.pos_ n3 >?> PatientDosage.ShapeDosages_ 
    
    
        let indxShapeDosage n (pat : PatientDosage) =
            pat.ShapeDosages
            |> List.tryFindIndex (fun sd -> sd.Name = n)
         
        
        let getShapeDosages inds rt pat (dr : DoseRule) =

            match dr |> indxIndications inds with 
            | Some n1 -> 
                match 
                    dr.IndicationsDosages.[n1]
                    |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        match dr 
                              |> Optic.get (shapeDosagesPrism n1 n2 n3) with 
                        | Some sds -> sds
                        | None -> []
                    | None -> []              
                | None -> []
            | None -> []    
        
        
        let setShapeDosages inds rt sds pat (dr : DoseRule) =
        
            match dr |> indxIndications inds with 
            | Some n1 -> 
                match 
                    dr.IndicationsDosages.[n1]
                    |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        dr 
                        |> Optic.set (shapeDosagesPrism n1 n2 n3) sds 
                    | None -> dr              
                | None -> dr
            | None -> dr
        
        
        let addShape inds rt shape pat (dr : DoseRule) =
        
            let sds =
                dr
                |> getShapeDosages inds rt pat
                |> List.append [ { Dosage.empty with Name = shape } ]
        
            dr
            |> setShapeDosages inds rt sds pat, pat

    
        let shapeDosagePrism n1 n2 n3 n4 =
            (routeDosPatientDosagesPrism n1 n2) >?> List.pos_ n3 >?> PatientDosage.ShapeDosages_ >?> List.pos_ n4
 
    
        let private shapeDosageGetter prism inds rt pat shape dr = 
            match dr |> indxIndications inds with 
            | Some n1 ->
                match dr.IndicationsDosages.[n1] |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        match dr.IndicationsDosages.[n1].RouteDosages.[n2].PatientDosages.[n3] |> indxShapeDosage shape with 
                        | Some n4 ->
                            dr |> Optic.get ((shapeDosagePrism n1 n2 n3 n4) >?> prism) |> Some
                        | None -> None
                    | None -> None
                | None -> None
            | None -> None


        let private shapeDosageSetter prism inds rt shape vu pat dr = 
            match dr |> indxIndications inds with 
            | Some n1 ->
                match dr.IndicationsDosages.[n1] |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        match dr.IndicationsDosages.[n1].RouteDosages.[n2].PatientDosages.[n3] |> indxShapeDosage shape with 
                        | Some n4 ->
                            dr |> Optic.set ((shapeDosagePrism n1 n2 n3 n4) >?> prism) vu, pat
                        | None -> dr, pat
                    | None -> dr, pat
                | None -> dr, pat
            | None -> dr, pat
 
    
        let private shapeFrequenciesGetter prism inds rt pat shape dr = 
            match dr |> indxIndications inds with 
            | Some n1 ->
                match dr.IndicationsDosages.[n1] |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        match dr.IndicationsDosages.[n1].RouteDosages.[n2].PatientDosages.[n3] |> indxShapeDosage shape with 
                        | Some n4 ->
                            dr |> Optic.get ((shapeDosagePrism n1 n2 n3 n4) >?> prism)
                        | None -> None
                    | None -> None
                | None -> None
            | None -> None


        let private shapeFrequenciesSetter prism inds rt shape vu pat dr = 
            match dr |> indxIndications inds with 
            | Some n1 ->
                match dr.IndicationsDosages.[n1] |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        match dr.IndicationsDosages.[n1].RouteDosages.[n2].PatientDosages.[n3] |> indxShapeDosage shape with 
                        | Some n4 ->
                            dr |> Optic.set ((shapeDosagePrism n1 n2 n3 n4) >?> prism) vu, pat
                        | None -> dr, pat
                    | None -> dr, pat
                | None -> dr, pat
            | None -> dr, pat


        let getFrequenciesShapeDosage = shapeFrequenciesGetter Dosage.Frequencies_


        let setFrequenciesShapeDosage = shapeFrequenciesSetter Dosage.Frequencies_

        
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
    

        let substanceDosagesPrism n1 n2 n3 =
            (routeDosPatientDosagesPrism n1 n2) >?> List.pos_ n3 >?> PatientDosage.SubstanceDosages_ 
    
    
        let indxSubstanceDosage n (pat : PatientDosage) =
            pat.SubstanceDosages
            |> List.tryFindIndex (fun sd -> sd.Name = n)
         
        
        let getSubstanceDosages inds rt pat (dr : DoseRule) =

            match dr |> indxIndications inds with 
            | Some n1 -> 
                match 
                    dr.IndicationsDosages.[n1]
                    |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        match dr 
                              |> Optic.get (substanceDosagesPrism n1 n2 n3) with 
                        | Some sds -> sds
                        | None -> []
                    | None -> []              
                | None -> []
            | None -> []    
        
        
        let setSubstanceDosages inds rt sds pat (dr : DoseRule) =
        
            match dr |> indxIndications inds with 
            | Some n1 -> 
                match 
                    dr.IndicationsDosages.[n1]
                    |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        dr 
                        |> Optic.set (substanceDosagesPrism n1 n2 n3) sds 
                    | None -> dr              
                | None -> dr
            | None -> dr

        
        let addSubstance inds rt substance pat (dr : DoseRule) =
        
            let sds =
                dr
                |> getSubstanceDosages inds rt pat
                |> List.append [ { Dosage.empty with Name = substance } ]
        
            dr
            |> setSubstanceDosages inds rt sds pat, pat

    
        let substanceDosagePrism n1 n2 n3 n4 =
            (routeDosPatientDosagesPrism n1 n2) >?> List.pos_ n3 >?> PatientDosage.SubstanceDosages_ >?> List.pos_ n4
 
    
        let private substanceDosageGetter prism inds rt pat substance dr = 
            match dr |> indxIndications inds with 
            | Some n1 ->
                match dr.IndicationsDosages.[n1] |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        match dr.IndicationsDosages.[n1].RouteDosages.[n2].PatientDosages.[n3] |> indxSubstanceDosage substance with 
                        | Some n4 ->
                            dr |> Optic.get ((substanceDosagePrism n1 n2 n3 n4) >?> prism) |> Some
                        | None -> None
                    | None -> None
                | None -> None
            | None -> None


        let private substanceDosageSetter prism inds rt substance vu pat dr = 
            match dr |> indxIndications inds with 
            | Some n1 ->
                match dr.IndicationsDosages.[n1] |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        match dr.IndicationsDosages.[n1].RouteDosages.[n2].PatientDosages.[n3] |> indxSubstanceDosage substance with 
                        | Some n4 ->
                            dr |> Optic.set ((substanceDosagePrism n1 n2 n3 n4) >?> prism) vu, pat
                        | None -> dr, pat
                    | None -> dr, pat
                | None -> dr, pat
            | None -> dr, pat

    
        let private substanceFrequenciesGetter prism inds rt pat substance dr = 
            match dr |> indxIndications inds with 
            | Some n1 ->
                match dr.IndicationsDosages.[n1] |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        match dr.IndicationsDosages.[n1].RouteDosages.[n2].PatientDosages.[n3] |> indxSubstanceDosage substance with 
                        | Some n4 ->
                            dr |> Optic.get ((substanceDosagePrism n1 n2 n3 n4) >?> prism)
                        | None -> None
                    | None -> None
                | None -> None
            | None -> None


        let private substanceFrequenciesSetter prism inds rt substance vu pat dr = 
            match dr |> indxIndications inds with 
            | Some n1 ->
                match dr.IndicationsDosages.[n1] |> indxRoute rt with 
                | Some n2 ->
                    match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                    | Some n3 ->
                        match dr.IndicationsDosages.[n1].RouteDosages.[n2].PatientDosages.[n3] |> indxSubstanceDosage substance with 
                        | Some n4 ->
                            dr |> Optic.set ((substanceDosagePrism n1 n2 n3 n4) >?> prism) vu, pat
                        | None -> dr, pat
                    | None -> dr, pat
                | None -> dr, pat
            | None -> dr, pat


        let getFrequenciessubstanceDosage = substanceFrequenciesGetter Dosage.Frequencies_


        let setFrequenciessubstanceDosage = substanceFrequenciesSetter Dosage.Frequencies_

        
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
    

    
    module Operators =
        
        let (|>>) (x1, x2) f = f x2 x1 

 
 
    let mdText = """
Stofnaam: {generic}

ATC code: {atc}

Therapeutische groep: {thergroup} 

Therapeutische subgroep: {thersub}

Generiek groep: {gengroup}

Generiek subgroep: {gensub}

Doseringen:
"""

    let mdIndicationText = """
Indicatie: {indication}
"""


    let mdRouteText = """
Route: {route}
"""

    let mdPatientText = """
{patient}

"""

    let mdDosageText = """
{dosage}

"""

    let print (dr : DoseRule) =
        mdText
        |> String.replace "{generic}" dr.Generic
        |> String.replace "{atc}" dr.ATC
        |> String.replace "{thergroup}" dr.ATCTherapyGroup
        |> String.replace "{thersub}" dr.ATCTherapySubGroup
        |> String.replace "{gengroup}" dr.GenericGroup
        |> String.replace "{gensub}" dr.GenericSubGroup
        |> (fun s ->
            dr.IndicationsDosages
            |> List.fold (fun acc id ->
                let i = 
                    id.Indications 
                    |> String.concat ", "

                id.RouteDosages
                |> List.fold (fun acc rd -> 
                    rd.PatientDosages
                    |> List.fold (fun acc pd ->
                        let shds =
                            pd.ShapeDosages
                            |> List.fold (fun acc sd ->
                                acc + (sd |> Dosage.toString)
                            ) (acc + (mdPatientText |> String.replace "{patient}" (pd.Patient |> Patient.toString)))
                            
                        pd.SubstanceDosages 
                        |> List.fold (fun acc sd ->
                            acc + (sd |> Dosage.toString)
                        ) shds
                        |> (fun sds -> mdDosageText |> String.replace "{dosage}" sds)
                                                
                    ) (acc + (mdRouteText |> String.replace "{route}" rd.Route))
                ) (acc + (mdIndicationText |> String.replace "{indication}" i))
            ) s
        )



module Test =

    open DoseRule.Operators
    
    module DoseRule = DoseRule.Props

    let test () =
        DoseRule.doseRule "paracetamol" "N02BE01" "Zenuwstelsel" "Analgetica" "Overige analgetica en antipyretica" "Aceetanilidederivaten" [] [] []
        |> DoseRule.addIndications ["Milde pijn en koorts"]
        |> DoseRule.addRoute ["Milde pijn en koorts"] "Oraal"
        |> DoseRule.addPatient ["Milde pijn en koorts"] "Oraal" (Patient.empty)
        |> DoseRule.setPatientInclMinAge ["Milde pijn en koorts"] "Oraal" (ValueUnit.create 3. "maanden") (Patient.empty) 
        |>> DoseRule.setPatientExclMaxAge ["Milde pijn en koorts"] "Oraal" (ValueUnit.create 18. "jaar") 
        |>> DoseRule.addSubstance ["Milde pijn en koorts"] "Oraal" "paracetamol"
        |>> DoseRule.setFrequenciessubstanceDosage  ["Milde pijn en koorts"] "Oraal" "paracetamol" [1..4]
        |>> DoseRule.setInclMinNormWeightSingleSubstanceDosage ["Milde pijn en koorts"] "Oraal" "paracetamol" (ValueUnit.create 10. "mg")
        |>> DoseRule.setInclMaxNormWeightSingleSubstanceDosage ["Milde pijn en koorts"] "Oraal" "paracetamol" (ValueUnit.create 15. "mg")
        |> fst
        |> DoseRule.print
        |> printfn "%s"
