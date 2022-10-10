
#load "load.fsx"


open Aether  
open Aether.Optics


open Informedica.ZForm.Lib


module MinMaxTests =

    module MinMax = MinMax.Optics

    let mmToStr = MinMax.toString "van" "tot"

    let v1, v2 = 
        ValueUnit.valueUnitFromGStandUnitString 10. "milligram" |> Option.get ,
        ValueUnit.valueUnitFromGStandUnitString 20. "milligram" |> Option.get
        
    let incl1, incl2 =
        v1 |> MinMax.inclusive, 
        v2 |> MinMax.inclusive

    let v3, v4 = 
        ValueUnit.valueUnitFromGStandUnitString 30. "milligram" |> Option.get ,
        ValueUnit.valueUnitFromGStandUnitString 40. "milligram" |> Option.get
        
    let incl3, incl4 =
        v3 |> MinMax.inclusive, 
        v4 |> MinMax.inclusive


    let toString () =
        MinMax.empty
        |> MinMax.setMin (ValueUnit.valueUnitFromGStandUnitString 1. "milligram"  |> Option.get |> MinMax.Inclusive)
        |> MinMax.setMax (ValueUnit.valueUnitFromGStandUnitString 10. "milligram" |> Option.get |> MinMax.Inclusive)
        |> mmToStr
        
        
    let a1, a2 =
        0.1 |> ValueUnit.ageInMo |> Option.bind (MinMax.Inclusive >> Some) |> Option.get ,
        0.1 |> ValueUnit.ageInYr |> Option.bind (MinMax.Exclusive >> Some) |> Option.get
        
        
    let ageToString () =
        MinMax.empty
        |> MinMax.setMin a1
        |> MinMax.setMax a2
        |> MinMax.ageToString


    let valueComp () =

        printfn "%A < %A = %A" incl1 incl2 (MinMax.valueST incl1 incl2)
        printfn "%A < %A = %A" incl1 incl1 (MinMax.valueST incl1 incl1)
        printfn "%A <= %A = %A" incl1 incl2 (MinMax.valueSTE incl1 incl2)
        printfn "%A <= %A = %A" incl1 incl1 (MinMax.valueSTE incl1 incl1)
        printfn ""
        printfn "%A > %A = %A" incl1 incl2 (MinMax.valueLT incl1 incl2)
        printfn "%A > %A = %A" incl1 incl1 (MinMax.valueLT incl1 incl1)
        printfn "%A >= %A = %A" incl1 incl2 (MinMax.valueLTE incl1 incl2)
        printfn "%A >= %A = %A" incl1 incl1 (MinMax.valueLTE incl1 incl1)


    // ToDo handle None cases correctly?
    let testFold () =
        let mms = 
            [
                MinMax.empty
                MinMax.empty |> MinMax.setMin incl1
                MinMax.empty |> MinMax.setMin incl2
                MinMax.empty |> MinMax.setMax incl3
                MinMax.empty |> MinMax.setMax incl4
                MinMax.empty |> MinMax.setMin incl1 |> MinMax.setMax incl3
                MinMax.empty |> MinMax.setMin incl2 |> MinMax.setMax incl3
                MinMax.empty |> MinMax.setMin incl3 |> MinMax.setMax incl3
                MinMax.empty |> MinMax.setMin incl4 |> MinMax.setMax incl4
            ]

        mms
        |> MinMax.foldMaximize
        |> mmToStr
        |> printfn "Maximized:\n%s"


        mms
        |> MinMax.foldMinimize
        |> mmToStr
        |> printfn "Minimized:\n%s"


    let inRange () =
        let mm1 = MinMax.empty
        let mm2 = 
            MinMax.empty
            |> MinMax.setMin incl1
        let mm3 = 
            MinMax.empty
            |> MinMax.setMax incl4
        let mm4 =
            MinMax.empty
            |> MinMax.setMin incl2
            |> MinMax.setMax incl3

        let test v mm =
            printfn "%A in range: %A = %A" (v |> MinMax.valueToString) (mm |> mmToStr) (MinMax.inRange v mm)


        [
            (incl1, mm1)
            (incl2, mm1)
            (incl3, mm1)
            (incl4, mm1)
            (incl1, mm2)
            (incl2, mm2)
            (incl3, mm2)
            (incl4, mm2)
            (incl1, mm3)
            (incl2, mm3)
            (incl3, mm3)
            (incl4, mm3)
            (incl1, mm4)
            (incl2, mm4)
            (incl3, mm4)
            (incl4, mm4)
        ]
        |> List.iter (fun (v, mm) -> test v mm)



module DoseRangeTests =

    module DoseRange = DoseRule.DoseRange

    let setMinNormDose = Optic.set DoseRange.Optics.inclMinNormLens
    let setMaxNormDose = Optic.set DoseRange.Optics.inclMaxNormLens

    let setMinNormPerKgDose = Optic.set DoseRange.Optics.inclMinNormWeightLens
    let setMaxNormPerKgDose = Optic.set DoseRange.Optics.inclMaxNormWeightLens

    let setMinAbsDose = Optic.set DoseRange.Optics.inclMinAbsLens
    let setMaxAbsDose = Optic.set DoseRange.Optics.inclMaxAbsLens

    let drToStr = DoseRange.toString None

    let toString () =
        DoseRange.empty
        |> setMaxNormDose (ValueUnit.valueUnitFromGStandUnitString 10. "milligram")
        |> setMaxAbsDose (ValueUnit.valueUnitFromGStandUnitString 100. "milligram")
        |> drToStr

    let toRateString () =
        DoseRange.empty
        |> setMinNormDose (ValueUnit.valueUnitFromGStandUnitString 10. "milligram")
        |> setMaxNormDose (ValueUnit.valueUnitFromGStandUnitString 100. "milligram")
        |> DoseRange.toString (Some ValueUnit.Units.hour)        

    let toRatePerKgString () =
        DoseRange.empty
        |> setMinNormPerKgDose (ValueUnit.valueUnitFromGStandUnitString 0.001 "milligram")
        |> setMaxNormPerKgDose (ValueUnit.valueUnitFromGStandUnitString 1. "milligram")
        |> DoseRange.convertTo (ValueUnit.Units.mcg)
        |> DoseRange.toString (Some ValueUnit.Units.hour)        

    let convert () =
        DoseRange.empty
        |> setMaxNormDose (ValueUnit.valueUnitFromGStandUnitString 1. "milligram")
        |> setMinNormDose (ValueUnit.valueUnitFromGStandUnitString 0.001 "milligram")
        |> DoseRange.convertTo (ValueUnit.Units.mcg)
        |> drToStr
        



module DosageTests =
    
    module Dosage = DoseRule.Dosage

    let setNormMinStartDose = Optic.set Dosage.Optics.inclMinNormStartDosagePrism
    let setAbsMaxStartDose = Optic.set Dosage.Optics.inclMaxAbsStartDosagePrism

    let setNormMinSingleDose = Optic.set Dosage.Optics.inclMinNormSingleDosagePrism
    let setAbsMaxSingleDose = Optic.set Dosage.Optics.inclMaxAbsSingleDosagePrism

    let setNormMaxSingleDose = Optic.set Dosage.Optics.inclMaxNormSingleDosagePrism

    let setNormMinRateDose = Optic.set Dosage.Optics.inclMinNormRateDosagePrism
    let setNormMaxRateDose = Optic.set Dosage.Optics.inclMaxNormRateDosagePrism
    let setRateUnit = Optic.set Dosage.Optics.rateUnitRateDosagePrism

    let toString () =
        Dosage.empty
        |> setNormMinStartDose (ValueUnit.valueUnitFromGStandUnitString 10. "milligram")
        |> setAbsMaxStartDose (ValueUnit.valueUnitFromGStandUnitString 1. "gram")
        |> setNormMinSingleDose (ValueUnit.valueUnitFromGStandUnitString 10. "milligram")
        |> setAbsMaxSingleDose (ValueUnit.valueUnitFromGStandUnitString 1. "gram")
        |> Dosage.toString true

    
    let convert () =
        Dosage.empty
        |> setNormMinSingleDose (ValueUnit.valueUnitFromGStandUnitString 0.01 "milligram")
        |> setNormMaxSingleDose (ValueUnit.valueUnitFromGStandUnitString 1. "milligram")
        |> Dosage.convertSubstanceUnitTo (ValueUnit.Units.mcg)
        |> Dosage.toString false
        

    let convertRate () =
        Dosage.empty
        |> setNormMinRateDose (ValueUnit.valueUnitFromGStandUnitString 0.01 "milligram")
        |> setNormMaxRateDose (ValueUnit.valueUnitFromGStandUnitString 1. "milligram")
        |> setRateUnit (ValueUnit.Units.hour)
        |> Dosage.convertSubstanceUnitTo (ValueUnit.Units.mcg)
        |> Dosage.convertRateUnitTo (ValueUnit.Units.min)
        |> Dosage.toString false



module PatientTests =

    module Patient = Patient.Optics

    type Patient = Patient.Patient
    
    let toString () =
        Patient.empty
        |> Patient.setInclMinGestAge (28.  |> ValueUnit.ageInWk)
        |> Patient.setExclMaxGestAge (33.  |> ValueUnit.ageInWk)
        |> Patient.setExclMinAge (1. |> ValueUnit.ageInMo)
        |> Patient.setInclMaxAge (120. |> ValueUnit.ageInWk)
        |> Patient.setInclMinWeight (0.15  |> ValueUnit.weightInKg)
        |> Patient.setInclMaxWeight (4.00  |> ValueUnit.weightInKg)
        |> Patient.setInclMinBSA (0.15  |> ValueUnit.bsaInM2)
        |> Patient.setInclMaxBSA (1.00  |> ValueUnit.bsaInM2)
        |> (fun p -> p |> (Optic.set Patient.Gender_) Patient.Male)
        |> Patient.toString




module GStandTests =

    open GStand
    open Informedica.Utils.Lib.BCL

    module Dosage = DoseRule.Dosage
    
    module RF = Informedica.ZIndex.Lib.RuleFinder 
    module DR = Informedica.ZIndex.Lib.DoseRule
    module GPP = Informedica.ZIndex.Lib.GenPresProduct

    let cfg = { UseAll = true ; IsRate = false ; SubstanceUnit = None ; TimeUnit = None }

    let cfgmcg = { cfg with SubstanceUnit = (Some ValueUnit.Units.mcg) }

    let createWithCfg cfg = GStand.createDoseRules cfg None None None None

    let createDoseRules = createWithCfg cfg
    
    let createCont su tu = 
        let cfg = { cfg with IsRate = true ; SubstanceUnit = su ; TimeUnit = tu }
        GStand.createDoseRules cfg None None None None

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
      {dosage}

"""


    let mdConfig = 
        {
            DoseRule.mdConfig with
                MainText = mdText
                IndicationText = mdIndicationText
                RouteText = mdRouteText
                ShapeText = mdShapeText
                PatientText = mdPatientText
                DosageText = mdDosageText
        }


    let toStr = DoseRule.toStringWithConfig mdConfig false


    let printDoseRules rs = 
        rs
        |> Seq.iter (fun dr -> 
            dr 
            |> toStr
            |> Markdown.toBrowser

        )


    let mapFrequency () =
        DR.get ()
        |> Seq.map (fun dr -> dr.Freq)
        |> Seq.distinct
        |> Seq.sortBy (fun fr -> fr.Time, fr.Frequency)
        |> Seq.map (fun fr -> fr, fr |> GStand.mapFreq)
        |> Seq.iter (fun (fr, vu) ->
            printfn "%A %s = %s" fr.Frequency fr.Time (vu |> ValueUnit.toStringPrec 0)
        )

    let tests () =

        createDoseRules "trimethoprim/sulfamethoxazol" "" "intraveneus"
        |> printDoseRules

        createDoseRules "clonidine" "" "oraal"
        |> printDoseRules

        GStand.createDoseRules cfg (Some 0.) (Some 12.) None None "paracetamol" "" "oraal"
        |> printDoseRules

        GStand.createDoseRules cfg (Some 100.) None (None) (Some 167541) "" "" ""
        |> printDoseRules
        |> (printfn "%A")
         
        GStand.createDoseRules cfg (Some 0.) (Some 1.5) None None "gentamicine" "" "intraveneus"
        |> printDoseRules

        createWithCfg cfgmcg "fentanyl" "" "intraveneus"
        |> printDoseRules

        createCont (Some ValueUnit.Units.mcg) (Some ValueUnit.Units.min) "dopamine" "" "intraveneus"
        |> printDoseRules

        createWithCfg cfgmcg "digoxine" "" ""
        |> printDoseRules


        RF.createFilter None None None None "paracetamol" "" ""
        |> RF.find true
        |> getSubstanceDoses cfg
        |> Seq.iter (fun (inds, sd) -> 
            printfn "Indication %s" (inds |> String.concat ", ")
            printfn "%s" (sd |> Dosage.toString true)
        )
 

        RF.createFilter None None None None "gentamicine" "" ""
        |> RF.find true
        |> getPatients cfg
        |> Seq.iter (fun (pat, sds, _) -> 
            printfn "%s" (pat |> Patient.toString)
            sds
            |> Seq.iter (fun (inds, sd) -> 
            printfn "Indication %s" (inds |> String.concat ", ")
            printfn "%s" (sd |> Dosage.toString true)
            )
        )


        GStand.createDoseRules cfg (Some 2.) (Some 4.) None None "paracetamol" "" "oraal"
        |> printDoseRules

        DR.get ()
        |> Seq.filter (fun dr -> 
            dr.Freq.Frequency = 1. && 
            dr.Freq.Time = "per uur" &&
            dr.Routes = [|"intraveneus"|]
        )
        |> Seq.collect (fun dr -> dr.GenericProduct |> Seq.map (fun gp -> gp.Name))
        |> Seq.distinct
        |> Seq.sort
        |> Seq.iter (printfn "%s")
//        |> Seq.length

        DR.get ()
        |> Seq.filter (fun dr ->
            dr.GenericProduct 
            |> Seq.map (fun gp -> gp.Name)
            |> Seq.exists (String.startsWithCapsInsens "salbutamol")
        )
        //|> Seq.collect (fun dr -> 
        //    dr.GenericProduct
        //    |> Seq.map (fun gp -> gp.Name, dr.Routes)
        //)
        |> Seq.map (DR.toString ",")
        |> Seq.distinct
        |> Seq.iter (printfn "%A")

        DR.get ()
        |> Seq.filter (fun dr ->
            dr.GenericProduct 
            |> Seq.map (fun gp -> gp.Name)
            |> Seq.exists (String.startsWithCapsInsens "fentanyl") &&
            dr.Freq.Time |> String.startsWithCapsInsens "eenmalig"
        )
        //|> Seq.collect (fun dr -> 
        //    dr.GenericProduct
        //    |> Seq.map (fun gp -> gp.Name, dr.Routes)
        //)
        |> Seq.map (DR.toString ",")
        |> Seq.distinct
        |> Seq.iter (printfn "%A")

        DR.get ()
        |> Seq.filter (fun dr ->
            dr.GenericProduct 
            |> Seq.map (fun gp -> gp.Name)
            |> Seq.exists (String.startsWithCapsInsens "fentanyl") 
        )
        //|> Seq.collect (fun dr -> 
        //    dr.GenericProduct
        //    |> Seq.map (fun gp -> gp.Name, dr.Routes)
        //)
        |> Seq.map (fun dr -> dr.Freq.Time)
        |> Seq.distinct
        |> Seq.iter (printfn "%A")


        GPP.get false
        |> Seq.filter (fun gpp -> gpp.Name |> String.equalsCapInsens "paracetamol")
        |> Seq.iter (fun gpp -> 
            gpp 
            |> (printfn "%A")
        )

        DR.get ()
        |> Seq.collect (fun r -> r.Routes)
        |> Seq.distinct
        |> Seq.sort
        |> Seq.iter (printfn "%s")

        GPP.get true
        |> Seq.filter (fun gpp -> 
            gpp.Route |> Seq.exists (fun r -> r |> String.equalsCapInsens "parenteraal")
        )
        |> Seq.distinct
        |> Seq.sort
        |> Seq.iter (GPP.toString >> printfn "%s")


        GPP.filter true "" "" "oraal"
        |> Seq.length
        |> ignore

        printfn "DoseRule Routes"
        DR.routes ()
        |> Seq.filter (fun r ->
            
            GPP.getRoutes ()
            |> Seq.exists (fun r' -> r = r') 
            |> not
        )
        |> Seq.sort
        |> Seq.iter (printfn "|%s|")
        printfn ""        
        printfn "GenPresProduct Routes"
        GPP.getRoutes ()
        |> Seq.filter (fun r ->
            DR.routes ()
            |> Seq.exists (fun r' -> r = r')
            |> not
        )
        |> Seq.sort
        |> Seq.iter (printfn "|%s|")


    GStand.createDoseRules cfg (Some 1.1) (Some 5.) None None "gentamicine" "" "intraveneus"
    |> printDoseRules

    
    
    