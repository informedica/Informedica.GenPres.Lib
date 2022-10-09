#I __SOURCE_DIRECTORY__

#load "./../../../.paket/load/netstandard2.0/main.group.fsx"

#r "./../../Informedica.GenUtils.Lib/bin/Debug/netstandard2.0/Informedica.GenUtils.Lib.dll"

#time


open Informedica.GenUtils.Lib.BCL
open Informedica.GenUtils.Lib

module Lens =
        
    open FSharp.Quotations
    open FSharp.Quotations.Patterns
    open FSharp.Quotations.Evaluator
    open Microsoft.FSharp.Reflection
    open System.Reflection

    let private eval = QuotationEvaluator.EvaluateUntyped

    module internal Record =

        let Fields = FSharpType.GetRecordFields
        let Type (p:PropertyInfo) = p.DeclaringType
        let ValuesOr p v o = 
            Type p |> Fields 
            |> Array.map (fun x -> if x = p then v else x.GetValue(o))
        let Make t xs = FSharpValue.MakeRecord(t,xs,false)
        let With p v o = ValuesOr p v o |> Make (Type p)
        let rec Update<'a,'b> = function
            | PropertyGet(None,_,[]),v -> v
            | PropertyGet(Some(PropertyGet(_) as pg),p,[]),v -> 
                Update(pg,(With p v (eval pg)))
            | _ -> failwith "blaargh"

    let With (e:Expr<'a>) (v:'a) = Record.Update(e.Raw,v) :?> 'b


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
    
    let apply f (vu : ValueUnit) = f vu

    
    let get = apply id

    
    let getValue vu = (vu |> get).Value


    let getUnit vu = (vu |> get).Unit


    let toString vu = 
        sprintf "%A %s" (vu |> getValue) (vu |> getUnit)



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
            
        
    let indxIndications inds (dr : DoseRule) =
        dr.IndicationsDosages
        |> List.tryFindIndex (fun id -> id.Indications = inds)       

        
    let addRoute inds rt (dr : DoseRule) =
        let rds = [ { Route = rt; GenericProducts = []; TradeProducts = []; PatientDosages = [] } ]
        
        match 
            dr |> indxIndications inds with 
            | Some n -> 
                { 
                    dr with
                        IndicationsDosages =
                            dr.IndicationsDosages
                            |> List.mapi (fun i dos ->
                                if i = n then
                                    { 
                                        dos with 
                                            RouteDosages = 
                                                dos.RouteDosages 
                                                |> List.append rds 
                                    }
                                else dos
                            )
                }

            | None -> dr
    

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
    

    let test () =
        DoseRule.doseRule "paracetamol" "N02BE01" "Zenuwstelsel" "Analgetica" "Overige analgetica en antipyretica" "Aceetanilidederivaten" [] [] []
        |> DoseRule.addIndications ["Milde pijn en koorts"]
        |> DoseRule.addRoute ["Milde pijn en koorts"] "Oraal"
        |> DoseRule.addPatient ["Milde pijn en koorts"] "Oraal" (Patient.empty)
        //|> DoseRule.setPatientInclMinAge ["Milde pijn en koorts"] "Oraal" (ValueUnit.create 3. "maanden") (Patient.empty) 
        //|>> DoseRule.setPatientExclMaxAge ["Milde pijn en koorts"] "Oraal" (ValueUnit.create 18. "jaar") 
        //|>> DoseRule.addSubstance ["Milde pijn en koorts"] "Oraal" "paracetamol"
        //|>> DoseRule.setFrequenciessubstanceDosage  ["Milde pijn en koorts"] "Oraal" "paracetamol" [1..4]
        //|>> DoseRule.setInclMinNormWeightSingleSubstanceDosage ["Milde pijn en koorts"] "Oraal" "paracetamol" (ValueUnit.create 10. "mg")
        //|>> DoseRule.setInclMaxNormWeightSingleSubstanceDosage ["Milde pijn en koorts"] "Oraal" "paracetamol" (ValueUnit.create 15. "mg")
        //|> fst
        |> DoseRule.print
        |> printfn "%s"
