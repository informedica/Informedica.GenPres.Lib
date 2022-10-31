namespace Informedica.MetaVision.Lib

open ClosedXML.Excel

open System.Runtime
open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.ZForm.Lib.DoseRule.ShapeDosage
open Informedica.ZForm.Lib.Utils
open Informedica.ZIndex.Lib


type GenPresProduct = GenPresProduct.GenPresProduct
type GenericProduct = GenericProduct.GenericProduct


module Constants =


    let includeSols =
        [|
        "abatacept"
        "abelcet"
        "acetazolamide"
        "acetylcholine"
        "acetylcysteine"
        "acetylsalicylzuur"
        "aciclovir"
        "adenosine"
        "adrenaline"
        "alanylglutamine"
        "albumine"
        "albutrepenonacog"
        "alcohol"
        "aldesleukine"
        "alemtuzumab"
        "alfacalcidol"
        "alfentanil"
        "alprostadil"
        "alteplase"
        "amfotericine"
        "amikacine"
        "amiodaron"
        "amoxicilline"
        "amoxicilline/clavulaanzuur"
        "anakinra"
        "anidulafungin"
        "anifrolumab"
        "antitrombine"
        "argatroban"
        "arginine"
        "argipressine"
        "artesunaat"
        "ascorbinezuur"
        "astrazeneca"
        "atosiban"
        "atracurium"
        "atropinesulfaat"
        "aztreonam"
        "basiliximab"
        "bcg-medac"
        "belatacept"
        "belimumab"
        "benzylpenicilline"
        "betamethason"
        "bevacizumab"
        "biperideen"
        "bivalirudine"
        "botuline"
        "bumetanide"
        "bupivacaine"
        "c1-esteraseremmer"
        "cabotegravir"
        "calcitonine"
        "calciumgluconaat"
        "cangrelor"
        "caplacizumab"
        "carbacholine"
        "carbetocine"
        "casirivimab"
        "caspofungin"
        "cefazoline"
        "cefiderocol"
        "cefotaxim"
        "ceftazidim"
        "wijzigingen"
        "ceftolozaan-tazobactam"
        "ceftriaxon"
        "cefuroxim"
        "cernevit"
        "cetrorelix"
        "chirhostim"
        "chlooramfenicol"
        "chloorprocaine"
        "choriongonadotrofine"
        "ciclosporine"
        "cidofovir"
        "ciprofloxacine"
        "cisatracurium"
        "citra-lock"
        "clemastine"
        "clindamycine"
        "clonazepam"
        "clonidine"
        "coffeine"
        "colistine"
        "comirnaty"
        "corticoreline"
        "cotrimoxazol"
        "cuvitru"
        "cyanocobalamine"
        "dalteparine"
        "danaparoide"
        "dantroleen"
        "daptomycine"
        "daratumumab"
        "darbepoetine"
        "deferoxamine"
        "defibrotide"
        "denosumab"
        "desmopressine"
        "dexamethason"
        "dexmedetomidine"
        "diazepam"
        "diclofenac"
        "digoxine"
        "dimenhydrinaat"
        "dobutamine"
        "dopamine"
        "doxapram"
        "doxycycline"
        "droperidol"
        "dtp-vaccin"
        "eculizumab"
        "efedrine"
        "efmoroctocog"
        "eftrenonacog"
        "emicizumab"
        "epoetine"
        "epoprostenol"
        "eptacog"
        "ertapenem"
        "erytromycine"
        "esketamine"
        "esmolol"
        "esomeprazol"
        "etanercept"
        "ethanol"
        "etomidaat"
        "factor"
        "fenobarbital"
        "fenol"
        "fentanyl"
        "fentolamine"
        "fenylefrine"
        "fenytoine"
        "ferricarboxymaltose"
        "ferriisomaltoside"
        "ferrioxidesaccharaat"
        "fibrinogeen"
        "filgrastim"
        "flecainide"
        "flucloxacilline"
        "fluconazol"
        "flucytosine"
        "flumazenil"
        "fluoresceine"
        "flupentixol"
        "folinezuur"
        "fondaparinux"
        "fosaprepitant"
        "foscarnet"
        "fosfomycine"
        "furosemide"
        "fytomenadion"
        "ganciclovir"
        "ganirelix"
        "gelatine"
        "gentamicine"
        "glucagon"
        "glucarpidase"
        "glucose"
        "glycerofosforzuur"
        "glycopyrronium"
        "golimumab"
        "gonadoreline"
        "granisetron"
        "haloperidol"
        "hemine"
        "heparine"
        "hepatitis-b-immunoglobuline"
        "hepatitis-b-vaccin"
        "hyaluronidase"
        "hyaluronidase/immunoglobuline"
        "hydrocortison"
        "hydroxocobalamine"
        "hydroxyethylzetmeel"
        "ibuprofen"
        "icatibant"
        "idarucizumab"
        "iloprost"
        "imipenem/cilastatine"
        "immunoglobuline"
        "indocyaninegroen"
        "infliximab"
        "insuline"
        "intralipid"
        "isatuximab"
        "isavuconazol"
        "isoniazide"
        "isoprenaline"
        "covid"
        "kaliumchloride"
        "ketanserine"
        "labetalol"
        "levetiracetam"
        "levobupivacaine"
        "levocarnitine"
        "levofloxacine"
        "levomepromazine"
        "levosimendan"
        "levothyroxine"
        "lidocaine"
        "linezolid"
        "liothyronine"
        "lipidemicrosferen"
        "lorazepam"
        "lutropine"
        "lymfocytenimmunoglobuline"
        "magnesiumchloride"
        "magnesiumsulfaat"
        "mannitol"
        "mecasermine"
        "meningokokkenvaccin"
        "menopauzegonadotrofine"
        "mepivacaine"
        "mepolizumab"
        "mercapto-ethaansulfonzuur"
        "meropenem"
        "metamizol"
        "methadon"
        "methoxypolyethyleenglycol-epoetine"
        "methyleenblauw"
        "methylergometrine"
        "methylnaltrexon"
        "methylprednisolon"
        "metoclopramide"
        "metoprolol"
        "metronidazol"
        "micafungine"
        "midazolam"
        "milrinon"
        "morfine"
        "moxifloxacine"
        "mycofenolaat"
        "nalbufine"
        "naloxon"
        "natalizumab"
        "natrium"
        "natriumbenzoaat"
        "natriumbenzoaat/natriumfenylacetaat"
        "natriumchloride"
        "natriumperchloraat"
        "natriumthiosulfaat"
        "natriumwaterstofcarbonaat"
        "neostigmine"
        "nicardipine"
        "nimodipine"
        "nitroglycerine"
        "nitroprusside"
        "nonacog"
        "noradrenaline"
        "nutriflex"
        "nuvaxovid"
        "octocog"
        "octreotide"
        "olanzapine"
        "olimel"
        "omalizumab"
        "omnipaque"
        "onasemnogene"
        "ondansetron"
        "oxybutynine"
        "oxycodon"
        "oxytocine"
        "palivizumab"
        "pamidronaat"
        "pantoprazol"
        "papaverine"
        "paracetamol"
        "parecoxib"
        "patentblauw"
        "pentamidine"
        "pethidine"
        "piperacilline"
        "piritramide"
        "pneumokokkenvaccin"
        "polidocanol"
        "posaconazol"
        "prednisolon"
        "prilocaine"
        "primene"
        "procainamide"
        "promethazine"
        "propofol"
        "protamine"
        "protrombine"
        "pyridoxine"
        "rabiesvaccin"
        "rasburicase"
        "remdesivir"
        "remifentanil"
        "remimazolam"
        "rhesus(d)"
        "rifampicine"
        "rilpivirine"
        "ringerlactaat"
        "risperidon"
        "rituximab"
        "rocuronium"
        "ropivacaine"
        "ropivacaïne"
        "ropivacaïne/sufentanil"
        "salbutamol"
        "sarilumab"
        "scopolaminebutyl"
        "sildenafil"
        "silibinin"
        "smoflipid"
        "somatoreline"
        "somatropine"
        "sotrovimab"
        "sufentanil"
        "sugammadex"
        "sulfametrol"
        "sulproston"
        "sumatriptan"
        "supliven"
        "suxamethonium"
        "tacrolimus"
        "taurolidine"
        "taurolock"
        "teicoplanine"
        "temoporfine"
        "terlipressine"
        "testosteron"
        "tetanusimmunoglobuline"
        "tetanusvaccin"
        "tetracosactide"
        "theofylline"
        "thiamine"
        "thiopental"
        "thymocytenimmunoglobuline"
        "tigecycline"
        "tirofiban"
        "tobramycine"
        "tocilizumab"
        "tramadol"
        "tranexaminezuur"
        "trastuzumab"
        "triamcinolonacetonide"
        "nieuwe"
        "triamcinolonhexacetonide"
        "triptoreline"
        "trometamol"
        "turoctocog"
        "urokinase"
        "ustekinumab"
        "valproaat"
        "vancomycine"
        "varicellazosterimmunoglobuline"
        "varicella-zostervaccin"
        "vedolizumab"
        "verapamil"
        "verteporfine"
        "vitaminen,"
        "willebrandfactor"
        "vonicog"
        "voriconazol"
        "wilate"
        "zanamivir"
        "zidovudine"
        "zoledroninezuur"
        "zuclopentixol"
    |]


    let routeHeadings =
        [|
            "ExternalCode"
            "RouteName"
            "OrderingType"
            "RouteLocations"
        |]


    let doseFormHeadings =
        [|
            "ExternalCode"
            "DoseFormName"
            "Routes"
            "DefaultUnit"
            "OrderingType"
            "IsDrugInSolution"
            "Category"
            "IsDispensableAmountAllowed"
        |]


    let ingredientHeadings =
        [|
            "ExternalCode"
            "IngredientName"
            "Unit"
        |]


    let medicationHeadings =
        [|
            "ExternalCode"
            "MedicationName"
            "Unit"
            "ATCCode"
            "Status"
            "Format"
            "IncrementValue"
            "CodeSnippetName"
            "Frequencies"
            "DoseForms"
            "Routes"
            "AdditivesGroup"
            "DiluentsGroup"
            "DrugInDiluentGroup"
            "DrugFamily"
            "DrugSubfamily"
            "HideInAllergyEntry"
            "AllergyLookBackTime"
            "AllergyLookBackTimeMeasure"
            "NormalQuantity"
            "NormalQuantityUnit"
            "MaskQuantity"
            "MaskQuantityUnit"
            "NormalRate"
            "NormalRateUnit"
            "MaskRate"
            "MaskRateUnit"
            "NormalConcentration"
            "NormalConcentrationMassUnit"
            "NormalConcentrationVolumeUnit"
            "MaskConcentration"
            "MaskConcentrationMassUnit"
            "MaskConcentrationVolumeUnit"
            "IsFormulary"
        |]


    let complexMedicationHeadings =
        [|
            "ComplexMedicationName"
            "IngredientName"
            "Concentration"
            "ConcentrationUnit"
            "In"
            "InUnit"
        |]


    let brandHeadings =
        [|
            "ExternalCode"
            "BrandName"
            "Manufacturer"
            "MedicationName"
        |]


    let productHeadings =
        [|
            "ExternalCode"
            "ProductID"
            "ProductName"
            "MedicationName"
            "BrandName"
            "Manufacturer"
            "DoseForm"
            "Routes"
            "Status"
            "Format"
            "IncrementValue"
            "Unit"
            "DefaultUnit"
            "IsUnknownStrength"
            "StrengthLEFT"
            "StrengthLEFTUnit"
            "StrengthRIGHT"
            "StrengthRIGHTUnit"
            "DiluentGroup"
            "ProductRequiresReconstitution"
            "IsVolumeKnown"
            "Volume"
            "VolumeUnit"
            "DiluentName"
            "Barcode"
            "ATCCode"
            "MinimumDispensibleAmount"
            "IsFormulary"
        |]


    let orderTemplateHeadings =
        [|
            "OrderTemplateName"
            "MedicationName"
            "ProductName"
            "DoseForm"
            "Route"
            "Location"
            "IsPRN"
            "PRNIndication"
            "MaxDosePer24Hr"
            "MaxDosePer24HrUnit"
            "PatternMode"
            "RepeatEvery"
            "RepeatUnit"
            "PatternPeriod"
            "DosePatternTime"
            "Frequency"
            "FrequencyValue"
            "FrequencyUnit"
            "OrderingStyle"
            "LockerTemplate"
            "TitrationMode"
            "ComponentType"
            "ComponentMedicationName"
            "ComponentProductName"
            "ComponentQuantityVolumeValue"
            "ComponentQuantityVolumeUnit"
            "ComponentConcentrationValue"
            "ComponentConcentrationMassUnit"
            "ComponentConcentrationVolumeUnit"
            "ComponentDrugInDiluentDiluentMedicationName"
            "ComponentDrugInDiluentDiluentProductName"
            "ComponentDrugInDiluentVolumeValue"
            "ComponentDrugInDiluentVolumeUnit"
            "ComponentDailyDosageAmount"
            "ComponentDailyDosageMaxRangeAmount"
            "ComponentDailyDosageUnit / Day"
            "DoseValue"
            "DoseMaxRange"
            "DoseUnit"
            "RateValue"
            "RateMaxRange"
            "RateUnit"
            "TotalVolumeValue"
            "TotalVolumeUnit"
            "InfuseOverValue"
            "InfuseOverMaxRange"
            "InfuseOverUnit"
            "StartMethod"
            "StartMethodValue"
            "StartMethodValueUnit"
            "EndMethod"
            "EndMethodValue"
            "EndMethodValueUnit"
            "DisallowSubstitutionReason"
            "WeightType"
            "Comment"
            "Caption"
            "AvailableInRT"
            "MarkForRemoval"
        |]


    let solutionHeadings =
        [|
            "GPK"
            "Generic"
            "Shape"
            "Route"
            "DoseType"
            "Dep"
            "CVL"
            "PVL"
            "DiluentVol"
            "Diluent"
            "MinAge"
            "MaxAge"
            "MinWeight"
            "MaxWeight"
            "MinDose"
            "MaxDose"
            "Solutions"
            "MinVol"
            "MaxVol"
            "MinPerc"
            "MaxPerc"
            "MinTime"
            "MaxTime"
            "MinRate"
            "MaxRate"
            "RateUnit"
            "Substance"
            "Unit"
            "Quantities"
            "MinQty"
            "MaxQty"
            "MinConc"
            "MaxConc"
        |]


[<AutoOpen>]
module Utils =


    [<RequireQualifiedAccess>]
    module Array =

        let mapHeadings def headings xs =
            headings
            |> Array.map (fun h ->
                match xs |> Array.tryFind (fst >> (=) h) with
                | Some x -> x |> snd
                | None   -> def
            )


        let mapStringHeadings headings xs = mapHeadings "" headings xs



    let mappingRoute =
        Web.getDataFromSheet "RouteMapping"


    let mappingRouteShape =
        Web.getDataFromSheet "RouteShape2"


    let mappingFreqs =
        Web.getDataFromSheet "Frequencies"


    let mappingUnits =
        Web.getDataFromSheet "Units"


    let mappingShapeUnit =
        Web.getDataFromSheet "ShapeUnit"


    let mappingFormulary =
        Web.getDataFromSheet "Formulary"



    type OrderingType = Both | NonInfuse


    type Status = Active | Inactive | Retired


    type Formulary = UMCU | ICC | PICU | NICU


    [<Literal>]
    let NA = "NIET VAN TOEPASSING"

    let capitalize s =
            let s =
                s
                |> String.trim
                |> String.replace "'" ""

            match s |> String.trim |> String.splitAt '-' with
            | [|s1; s2|] -> $"{s1 |> String.capitalize}-{s2 |> String.capitalize}"
            | _ -> s |> String.capitalize

    let capitalizeRoute route =
        route
        |> capitalize
        |> fun s ->
            if s |> String.startsWith "Intra-" ||
               s |> String.startsWith "Intra" |> not then s
            else s |> String.replace "Intra" "Intra-" |> capitalize
            |> String.replace "-" ""


    let mapRoute s =
        let s = s |> String.toLower |> String.trim
        mappingRoute
        |> Array.tryFind (fun xs -> xs[0] |> String.toLower |> String.trim = s)
        |> function
        | Some xs -> xs[1]
        | None ->
            $"cannot find route: |{s}|"
            |> failwith


    let isMultiple (gpp : GenPresProduct) =
        gpp.GenericProducts
        |> Array.exists (fun gp ->
            gp.Substances
            |> Array.map (fun s -> s.SubstanceName)
            |> Array.distinct
            |> Array.length > 1)


    let hasNoUnit (gpp : GenPresProduct) =
        gpp.GenericProducts
        |> Array.filter (fun gp ->
            gp.Substances
            |> Array.forall (fun s -> s.GenericUnit <> NA)
        )
        |> Array.isEmpty


    let shapeInDiluent un shape =
        mappingShapeUnit
        |> Array.tryFind (fun xs ->
            shape |> String.trim |> String.equalsCapInsens xs[0] &&
            un |> String.trim |> String.equalsCapInsens xs[1]
        )
        |> function
        | Some xs ->
            xs
            |> Array.item 2
            |> ((=) "TRUE")
        | None ->
            failwith $"cannot find unit: {un}, shape: {shape}"

    let shapeInSolution un shape =
        mappingShapeUnit
        |> Array.tryFind (fun xs ->
            shape |> String.trim |> String.equalsCapInsens xs[0] &&
            un |> String.trim |> String.equalsCapInsens xs[1]
        )
        |> function
        | Some xs ->
            xs
            |> Array.item 3
            |> ((=) "TRUE")
        | None ->
            failwith $"cannot find unit: {un}, shape: {shape}"


    let getATCCodes (gpp : GenPresProduct) =
        gpp.GenericProducts
        |> Array.map (fun gp -> gp.ATC |> String.trim)
        |> Array.distinct


    let getRoutesLongShort () =
        Names.getItems Names.Route Names.TwentyFive
        |> Array.map snd
        |> Array.zip (Names.getItems Names.Route Names.Fifty |> Array.map snd)
        |> Array.map (fun (l, s) ->
            l
            |> String.replace "," "/"
            |> String.replace "/ " "/",
            s
            |> String.replace "," "/"
            |> String.replace "/ " "/"
            |> String.toLower
        )
        |> Array.collect (fun (l, s) ->
            s
            |> String.splitAt '/'
            |> Array.zip (l |> String.splitAt '/')
        )
        |> Array.distinct
        |> Array.sort
        |> Array.map (fun (l, s) -> $"{l}\t{s}")
        |> Array.iter (printfn "%s")




    let mapUnit un =
        let un = un |> String.trim |> String.toLower
        mappingUnits
        |> Array.tryFind (fun r ->
            r[0] = un || r[1] = un
        )
        |> function
        | Some r -> r[2]
        | None   ->
            printfn $"cannot find {un} in mapping"
            ""


    let mapBool b =
        if b then "TRUE" else "FALSE"


    let mapFreq freq =
        mappingFreqs
        |> Array.filter (fun r ->
            r[0] = freq
        )
        |> function
        | [||]   ->
            printfn $"cannot find {freq} in mapping"
            [||]
        | xs ->
            xs
            |> Array.collect (fun r -> r[1..2])
            |> Array.filter (String.isNullOrWhiteSpace >> not)
            |> Array.distinct


    let getFormulary gpk =
        mappingFormulary
        |> Array.skip 1
        |> Array.tryFind (fun xs -> xs[0] |> Int32.parse = gpk)
        |> function
        | Some xs ->
            [
                if xs[1] <> "" then UMCU
                if xs[6] = "TRUE" then ICC
                if xs[8] = "TRUE" then PICU
                if xs[7] = "TRUE" then NICU
            ]
        | None -> []


    let isSolutionUnit un =
        un
        |> String.trim
        |> String.toLower = "milliliter"


    let filter gpps =
        gpps
        |> Array.filter (hasNoUnit >> not)
        |> Array.map (fun gpp ->
            { gpp with
                GenericProducts =
                    gpp.GenericProducts
                    |> Array.filter (fun gp ->
                        gp.Substances
                        |> Array.forall (fun s -> s.SubstanceUnit <> NA)
                    )
            }
        )


    let removeEmptyUnitSubstances (gp : GenericProduct) =
        { gp with
            Substances =
                gp.Substances
                |> Array.filter (fun s ->
                    s.SubstanceUnit <> NA
                )
        }


    let getSynonyms (gp: GenericProduct) =
        GenPresProduct.get true
        |> Array.filter (fun gpp -> gpp.GenericProducts |> Array.exists ((=) gp))
        |> Array.collect (fun gpp ->
            gpp.GenericProducts
            |> Array.collect (fun gp ->
                gp.PrescriptionProducts
                |> Array.collect (fun pp ->
                    pp.TradeProducts
                    |> Array.map (fun tp -> tp.Brand)
                )
            )
        )
        |> Array.distinct


    let getFrequencies (id : int) =
        RuleFinder.createFilter
            None
            None
            None
            (Some id)
            ""
            ""
            ""
        |> RuleFinder.find true
        |> Array.map (fun dr -> $"{dr.Freq.Frequency} {dr.Freq.Time}")
        |> Array.distinct
        |> Array.collect mapFreq
        |> Array.distinct
        |> String.concat ";"


    let loadDataImport (sheet : string) xs =
        let wb = new XLWorkbook("data/output/DrugDatabaseForImport.xlsx")
        wb.Worksheet(sheet).Cell(2, 3).Value <- xs
        wb.Save()


    let print b file xs =
        if b then
            xs
            |> Array.map (String.split "\t")
            |> Array.map List.toArray
            |> Array.skip 1
            |> loadDataImport file

        xs
        |> String.concat "\n"
        |> File.writeTextToFile $"data/output/{file}.csv"

        xs


module MetaVision =


    let getDrugFamilies path =
        ATCGroup.get ()
        |> Array.map (fun g ->
            g.AnatomicalGroup |> capitalize,
            g.TherapeuticSubGroup |> capitalize
        )
        |> Array.distinct
        |> Array.sort
        |> Array.map (fun (m, s) -> $"{m}\t{s}")
        |> print false path


    let shapeUnits name =
        GenPresProduct.get true
        //|> Array.filter (fun gpp -> gpp.Shape |> String.toLower |> String.contains "concentraat" )
        |> Array.collect (fun gpp -> gpp.GenericProducts)
        |> Array.map (fun gp -> gp.Shape, gp.Substances[0].ShapeUnit)
        |> Array.distinct
        |> Array.map (fun (s, u) -> $"{s |> String.trim |> String.toLower}\t{u |> String.trim |> String.toLower}")
        |> print false name


    let createRoutes name =
        let mapRts = (Array.mapStringHeadings Constants.routeHeadings) >> (String.concat "\t")
        // Get routes and external codes
        // Intra seperated by -
        Names.getItems Names.Route Names.Fifty
        |> Array.filter (fun (_, r) -> r.Contains(",") |> not)
        |> Array.sortBy fst
        |> Array.map (fun (id, r) ->
            [|
                "ExternalCode", $"%i{id}"
                "RouteName", r |> mapRoute
                "OrderingType",
                    mappingRouteShape
                    |> Array.filter (fun xs -> r |> String.toLower = xs[0] )
                    |> Array.fold (fun acc xs ->
                        match acc with
                        | NonInfuse ->
                            if xs[2] |> String.contains "NoTime" then NonInfuse
                            else Both
                        | Both -> Both
                    ) NonInfuse
                    |> string
            |]
            |> mapRts
        )
        |> Array.append [| Constants.routeHeadings |> String.concat "\t" |]
        |> print true name


    let createDoseForms name =
        let mapForms = (Array.mapStringHeadings Constants.doseFormHeadings) >> (String.concat "\t")

        // Get doseforms
        Names.getItems Names.Shape Names.Fifty
        |> Array.distinct
        |> Array.map (fun (id, s) ->
            let s = s |> String.toLower
            {|
                ExternalCode = id
                DoseFormName = s |> String.toLower
                Routes =
                    GenPresProduct.get true
                    |> Array.filter (fun gpp -> gpp.Shape |> String.equalsCapInsens s)
                    |> Array.collect (fun gpp -> gpp.Route)
                    |> Array.collect (String.splitAt ',')
                    |> Array.filter ((String.equalsCapInsens "parenteraal") >> not)
                    |> Array.distinct
                OrderingType =
                    mappingRouteShape
                    |> Array.filter (fun xs -> s |> String.toLower = xs[1] )
                    |> Array.fold (fun acc xs ->
                        match acc with
                        | NonInfuse ->
                            if xs[2] |> String.contains "NoTime" then acc
                            else Both
                        | Both -> Both
                    ) NonInfuse
                IsDrugInSolution =
                    mappingShapeUnit
                    |> Array.tryFind (fun xs -> s = xs[0] )
                    |> function
                    | Some xs -> xs[2] = "TRUE"
                    | None ->
                        printfn $"cannot find shape {s} in ShapeUnit"
                        false
                Category = "G-Standaard"
                IsDispensableAmountAllowed = false

            |}
        )
        |> Array.filter (fun r -> r.Routes |> Array.isEmpty |> not)
        |> Array.sortBy (fun r -> r.ExternalCode)
        |> Array.map (fun r ->
            let rs =
                r.Routes
                |> Array.map mapRoute
                |> Array.distinct
                |> String.concat ";"
            [|
                "ExternalCode", $"%i{r.ExternalCode}"
                "DoseFormName", r.DoseFormName
                "Routes", rs
                "OrderingType",$"{r.OrderingType}"
                "IsDrugInSolution", $"{r.IsDrugInSolution |> mapBool}"
                "Category", r.Category
                "IsDispensableAmountAllowed", $"{r.IsDispensableAmountAllowed |> mapBool}"

            |]
            |> mapForms
        )
        |> Array.append [| Constants.doseFormHeadings |> String.concat "\t" |]
        |> print true name


    let createIngredients name (gpps : GenPresProduct[]) =
        let mapIngrs = (Array.mapStringHeadings Constants.ingredientHeadings) >> (String.concat "\t")

        let substs =
            gpps
            |> Array.collect (fun gpp -> gpp.GenericProducts)
            |> Array.collect (fun gp -> gp.Substances)

        // Ingredients
        substs
        |> Array.map (fun s ->
            {|
                ExternalCode =  $"%i{s.SubstanceId}"
                IngredientName = s.SubstanceName |> String.trim |> String.toLower
                Unit = s.SubstanceUnit |> mapUnit
            |}
        )
        |> Array.distinctBy (fun r -> r.ExternalCode)
        |> Array.filter (fun r ->
            if r.Unit |> String.isNullOrWhiteSpace then
                printfn $"{r.IngredientName} has no unit"

            r.Unit |> String.isNullOrWhiteSpace |> not
        )
        |> Array.sortBy (fun r -> r.IngredientName)
        |> fun ingrs ->
            substs
            |> Array.map (fun s ->
                let su = s.ShapeUnit |> mapUnit
                {|
                    ExternalCode = ""
                    IngredientName = su
                    Unit = su
                |}
            )
            |> Array.distinct
            |> Array.append ingrs
        |> Array.map (fun r ->
            [|
                "ExternalCode", r.ExternalCode
                "IngredientName", r.IngredientName
                "Unit", r.Unit

            |]
            |> mapIngrs
        )
        |> Array.append [| Constants.ingredientHeadings |> String.concat "\t" |]
        |> print true name
        |> ignore



    let createMedications ingrName medName complName brandName prodName meds =
        let mapMeds = (Array.mapStringHeadings Constants.medicationHeadings) >> (String.concat "\t")
        let mapComp = (Array.mapStringHeadings Constants.complexMedicationHeadings) >> (String.concat "\t")
        let mapBrand = (Array.mapStringHeadings Constants.brandHeadings) >> (String.concat "\t")
        let mapProd = (Array.mapStringHeadings Constants.productHeadings) >> (String.concat "\t")

        meds |> createIngredients ingrName

        let meds =
            meds
            |> filter
            |> Array.collect (fun gpp -> gpp.GenericProducts)
            |> Array.map removeEmptyUnitSubstances
            |> Array.filter (fun gp -> gp.Substances |> Array.isEmpty |> not)
            |> Array.distinct
            |> Array.map (fun gp ->
                let name =
                    gp.Name
                    |> String.trim
                    |> String.toLower
                    |> String.replace "'" ""
                let g =
                    ATCGroup.get ()
                    |> Array.filter (fun g ->
                        g.ATC5
                        |> String.contains (gp.ATC |> String.subString 0 4)
                    )
                    |> Array.tryHead
                let su =
                    gp.Substances[0].ShapeUnit
                    |> mapUnit
                let un =
                    gp.Substances[0].SubstanceUnit
                    |> mapUnit
                let assort =
                    gp.Id
                    |> getFormulary

                {|
                    ExternalCode = $"GPK-{gp.Id}"
                    MedicationName = name
                    Unit = un
                    ATC =
                        gp
                        |> getSynonyms
                        |> Array.append [| gp.ATC |]
                        |> Array.map String.trim
                        |> Array.filter (String.isNullOrWhiteSpace >> not)
                        |> Array.distinct
                        |> Array.map (String.replace "'" "")
                        |> Array.append (assort |> List.filter ((<>) UMCU) |> List.map string |> List.toArray)
                        |> String.concat ", "
                    Status = "Active"
                    Format = "1,234.5 (Include Zero)"
                    IncrementValue = 0.1
                    CodeSnippetName = $"GPK-{gp.Id}"
                    Frequencies =
                        let freqs = gp.Id |> getFrequencies
                        if freqs |> String.isNullOrWhiteSpace then "[All]"
                        else freqs
                    DoseForms = gp.Shape |> String.toLower |> String.trim
                    Routes =
                        GenPresProduct.get true
                        |> Array.filter (fun gpp -> gpp.GenericProducts |> Array.exists ((=) gp))
                        |> Array.collect (fun gpp -> gpp.Route)
                        |> Array.collect (String.splitAt ',')
                        |> Array.filter ((String.equalsCapInsens "Parenteraal") >> not)
                        |> Array.distinct
                        |> Array.map mapRoute
                        |> String.concat ";"
                    AdditivesGroup = "[None]"
                    DiluentsGroup =
                        if gp.Shape |> shapeInSolution gp.Substances[0].ShapeUnit then "Oplossingen"
                        else ""
                    DrugInDiluentGroup =
                        if gp.Shape |> shapeInDiluent gp.Substances[0].ShapeUnit then "Diluents"
                        else
                            "[None]"
                    DrugFamily = g |> Option.map (fun g -> g.AnatomicalGroup |> capitalize) |> Option.defaultValue ""
                    DrugSubfamily = g |> Option.map (fun g -> g.TherapeuticSubGroup |> capitalize) |> Option.defaultValue ""
                    IsFormulary = assort |> List.isEmpty |> not
                    IsSolution = gp.Substances[0].ShapeUnit |> isSolutionUnit

                    ComplexMedications =
                        if gp.Substances |> Array.length > 4 then [||]
                        else
                            let cms =
                                gp.Substances
                                |> Array.map (fun s ->
                                    {|
                                        ComplexMedictionName = name
                                        IngredientName =
                                            s.SubstanceName
                                            |> String.toLower
                                            |> String.trim
                                        Concentration = s.SubstanceQuantity
                                        ConcentrationUnit =
                                            s.SubstanceUnit
                                            |> mapUnit
                                        In = ""
                                        InUnit = ""
                                    |}
                                )

                            if gp.Substances[0].ShapeUnit |> isSolutionUnit then [||]
                            else
                                [|
                                    {|
                                        ComplexMedictionName = name
                                        IngredientName = su
                                        Concentration = 1.
                                        ConcentrationUnit = su
                                        In = ""
                                        InUnit = ""
                                    |}
                                |]
                            |> Array.append cms

                    Brands =
                        gp.PrescriptionProducts
                        |> Array.collect (fun pp ->
                            pp.TradeProducts
                            |> Array.map (fun tp -> tp.Brand)
                        )
                        |> Array.filter (String.isNullOrWhiteSpace >> not)
                        |> Array.distinct

                |}
            )
            |> Array.filter (fun r -> r.Routes |> String.isNullOrWhiteSpace |> not)
            |> Array.sortBy (fun r -> r.MedicationName)
            |> Array.map (fun r ->
                {| r with
                    Products =
                        if r.IsSolution |> not ||
                           r.ComplexMedications |> Array.isEmpty then [||]
                        else
                            [|
                                {|
                                    Id = r.ExternalCode |> String.replace "GPK" "PROD"
                                    ProductID = r.ExternalCode |> String.replace "GPK-" ""
                                    ProductName = r.MedicationName
                                    MedicationName = r.MedicationName
                                    Manufacturer = "Apotheek"
                                    DoseForm = r.DoseForms
                                    Routes = "[All]"
                                    Format = r.Format
                                    IncrementValue = r.IncrementValue
                                    Unit = "mL"
                                    DefaultUnit = r.Unit
                                    IsUnknownStrength = "FALSE"
                                    StrengthLEFT = r.ComplexMedications[0].Concentration
                                    StrengthLEFTUnit = r.Unit
                                    StrengthRIGHT = "1"
                                    StrengthRIGHTUnit = "mL"
                                    ProductRequiresReconstitution = "FALSE"
                                    IsVolumeKnown = "FALSE"
                                    Volume = "0"
                                |}
                            |]
                |}
            )

        meds
        |> Array.filter (fun r -> r.Brands |> Array.isEmpty |> not)
        |> Array.collect (fun r ->
            r.Brands
            |> Array.map (fun b ->
                [|
                    "BrandName", b
                    "MedicationName", r.MedicationName
                |]
                |> mapBrand
            )
        )
        |> Array.append [| Constants.brandHeadings |> String.concat "\t" |]
        |> print true brandName
        |> ignore

        meds
        |> Array.collect (fun r -> r.ComplexMedications)
        |> Array.map (fun r ->
            [|
                "ComplexMedicationName", r.ComplexMedictionName
                "IngredientName", r.IngredientName
                "Concentration", $"{r.Concentration |> Double.toStringNumberNLWithoutTrailingZeros}"
                "ConcentrationUnit", r.ConcentrationUnit
                "In", r.In
                "InUnit",r.InUnit
            |]
            |> mapComp
        )
        |> Array.append [| Constants.complexMedicationHeadings |> String.concat "\t" |]
        |> print true complName
        |> ignore

        meds
        |> Array.collect (fun r -> r.Products)
        |> Array.map (fun r ->
            [|
                "ExternalCode", r.Id
                "ProductID", r.ProductID
                "ProductName", r.ProductName
                "MedicationName", r.MedicationName
                "Manufacturer", r.Manufacturer
                "DoseForm", r.DoseForm
                "Routes", r.Routes
                "Format", r.Format
                "IncrementValue", r.IncrementValue |> Double.toStringNumberNLWithoutTrailingZeros
                "Unit", r.Unit
                "DefaultUnit", r.DefaultUnit
                "IsUnknownStrength", r.IsUnknownStrength
                "StrengthLEFT", r.StrengthLEFT |> Double.toStringNumberNLWithoutTrailingZeros
                "StrengthLEFTUnit", r.StrengthLEFTUnit
                "StrengthRIGHT", r.StrengthRIGHT
                "StrengthRIGHTUnit", r.StrengthRIGHTUnit
                "ProductRequiresReconstitution", r.ProductRequiresReconstitution
                "IsVolumeKnown", r.IsVolumeKnown
                "Volume", r.Volume
            |]
            |> mapProd
        )
        |> Array.append [| Constants.productHeadings |> String.concat "\t" |]
        |> print true prodName
        |> ignore


        meds
        |> Array.map (fun r ->
            [|
                "ExternalCode", r.ExternalCode
                "MedicationName", r.MedicationName
                "Unit", r.Unit
                "ATCCode", r.ATC
                "Status", r.Status
                "Format", r.Format
                "IncrementValue", $"{r.IncrementValue |> Double.toStringNumberNLWithoutTrailingZeros}"
                "CodeSnippetName", r.CodeSnippetName
                "Frequencies", r.Frequencies
                "DoseForms", r.DoseForms
                "Routes", r.Routes
                "AdditivesGroup", r.AdditivesGroup
                "DiluentsGroup", r.DiluentsGroup
                "DrugInDiluentGroup", r.DrugInDiluentGroup
                "DrugFamily", r.DrugFamily
                "DrugSubfamily", r.DrugSubfamily
                "IsFormulary", $"{r.IsFormulary |> mapBool}"

            |]
            |> mapMeds
        )
        |> Array.append [| Constants.medicationHeadings |> String.concat "\t" |]
        |> print true medName


    let createSolutions solPath (meds : GenPresProduct[]) =
        // get solutions
        meds
        |> Array.collect (fun gpp -> gpp.GenericProducts)
        |> Array.filter (fun gp -> gp.Id |> getFormulary |> List.isEmpty |> not)
        |> Array.filter (fun gp -> gp.Substances |> Array.length <= 4)
        |> Array.filter (fun gp ->
            Constants.includeSols |> Array.exists (fun s -> gp.Name |> String.toLower |> String.contains s)
        )
        |> Array.filter (fun gp ->
            let su = gp.Substances[0].ShapeUnit
            gp.Shape |> shapeInDiluent su ||
            gp.Shape |> shapeInSolution su
        )
        |> Array.sortBy (fun gp -> gp.Name, gp.Shape, gp.Route)
        |> Array.collect (fun gp ->
            let mapSols = (Array.mapStringHeadings Constants.solutionHeadings) >> (String.concat "\t")

            gp.Route
            |> Array.collect (fun r -> r |> String.splitAt ',')
            |> Array.map mapRoute
            |> Array.filter (fun r -> r = "iv" || r = "im")
            |> Array.collect (fun r ->
                [| "ICC"; "PICU"; "NICU" |]
                |> Array.map (fun dep ->
                    let su = gp.Substances[0].ShapeUnit
                    {|
                        GPK = gp.Id
                        Generic = gp.Name |> String.toLower |> String.trim
                        Shape = gp.Shape |> String.trim |> String.toLower
                        Route = r
                        Department = dep
                        DiluentVol =
                            if gp.Shape |> shapeInDiluent su then "1" else "0"
                        Solutions = "NaCl;gluc5;gluc10"
                        Substance =
                            gp.Substances[0].SubstanceName
                            |> String.toLower
                            |> String.trim
                    |}
                )
                |> Array.map (fun r ->
                    [|
                        "GPK", $"%i{r.GPK}"
                        "Generic", r.Generic
                        "Shape", r.Shape
                        "Route", r.Route
                        "Dep", r.Department
                        "DiluentVol", r.DiluentVol
                        "Solutions", r.Solutions
                        "Substance", r.Substance
                    |]
                    |> mapSols
                )
            )
        )
        |> Array.append [| Constants.solutionHeadings |> String.concat "\t" |]
        |> print false solPath