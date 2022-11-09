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


    let diluentsGroup = "Oplossen"


    let solutionGroup = "Verdunnen"


    let excludeShapes =
        [|
            "infuus"
            "injectie"
            "injectie/infuus"
        |]


    let solutionMeds =
        [|
            "water", "oplosvloeistof"
            "NaCl 0,9%", "oplosvloeistof"
            "gluc 5%", "oplosvloeistof"
            "gluc 10%", "oplosvloeistof"
            "emulsie", "emulsie"
        |]


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



    let meds =
        [|
            [|
                "MedicationName", "water"
                "Unit", "mL"
                "ATCCode", ""
                "Status", "Active"
                "Format", "1,234.5 (Include Zero)"
                "IncrementValue", "0,1"
                "CodeSnippetName", ""
                "Frequencies", "[All]"
                "DoseForms", "oplosvloeistof"
                "Routes", "[All]"
                "AdditivesGroup", "[All]"
                "DiluentsGroup", "[All]"
                "DrugInDiluentGroup", "[None]"
                "DrugFamily", "[None]"
                "DrugSubfamily", "[None]"
                "HideInAllergyEntry", "FALSE"
            |]
            [|
                "MedicationName", "NaCl 0,9%"
                "Unit", "mL"
                "ATCCode", ""
                "Status", "Active"
                "Format", "1,234.5 (Include Zero)"
                "IncrementValue", "0,1"
                "CodeSnippetName", ""
                "Frequencies", "[All]"
                "DoseForms", "oplosvloeistof"
                "Routes", "[All]"
                "AdditivesGroup", "[All]"
                "DiluentsGroup", "[All]"
                "DrugInDiluentGroup", "[None]"
                "DrugFamily", "[None]"
                "DrugSubfamily", "[None]"
                "HideInAllergyEntry", "FALSE"
            |]
            [|
                "MedicationName", "gluc 5%"
                "Unit", "mL"
                "ATCCode", ""
                "Status", "Active"
                "Format", "1,234.5 (Include Zero)"
                "IncrementValue", "0,1"
                "CodeSnippetName", ""
                "Frequencies", "[All]"
                "DoseForms", "oplosvloeistof"
                "Routes", "[All]"
                "AdditivesGroup", "[All]"
                "DiluentsGroup", "[All]"
                "DrugInDiluentGroup", "[None]"
                "DrugFamily", "[None]"
                "DrugSubfamily", "[None]"
                "HideInAllergyEntry", "FALSE"
            |]
            [|
                "MedicationName", "gluc 10%"
                "Unit", "mL"
                "ATCCode", ""
                "Status", "Active"
                "Format", "1,234.5 (Include Zero)"
                "IncrementValue", "0,1"
                "CodeSnippetName", ""
                "Frequencies", "[All]"
                "DoseForms", "oplosvloeistof"
                "Routes", "[All]"
                "AdditivesGroup", "[All]"
                "DiluentsGroup", "[All]"
                "DrugInDiluentGroup", "[None]"
                "DrugFamily", "[None]"
                "DrugSubfamily", "[None]"
                "HideInAllergyEntry", "FALSE"
            |]
            [|
                "MedicationName", "emulsie"
                "Unit", "mL"
                "ATCCode", ""
                "Status", "Active"
                "Format", "1,234.5 (Include Zero)"
                "IncrementValue", "0,1"
                "CodeSnippetName", ""
                "Frequencies", "[All]"
                "DoseForms", "emulsie"
                "Routes", "[All]"
                "AdditivesGroup", "[All]"
                "DiluentsGroup", "[All]"
                "DrugInDiluentGroup", "[None]"
                "DrugFamily", "[None]"
                "DrugSubfamily", "[None]"
                "HideInAllergyEntry", "FALSE"
            |]
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
        Web.getDataFromSheet "RouteShapeUnit"


    let mappingFreqs =
        Web.getDataFromSheet "Frequencies"


    let mappingUnits =
        Web.getDataFromSheet "Units"


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


    let filterRouteShapeUnit rte shape unt =
        mappingRouteShape
        |> Array.filter (fun xs ->
            let eqsRte = rte |> String.isNullOrWhiteSpace || rte |> String.trim |> String.equalsCapInsens xs[0]
            let eqsShp = shape |> String.isNullOrWhiteSpace || shape |> String.trim |> String.equalsCapInsens xs[1]
            let eqsUnt = unt |> String.isNullOrWhiteSpace || unt |> String.trim |> String.equalsCapInsens xs[2]
            eqsRte && eqsShp && eqsUnt
        )


    let shapeInDiluent rte unt shape =
        filterRouteShapeUnit rte shape unt
        |> Array.map (fun xs -> xs[5] = "TRUE")
        |> Array.fold (fun acc b -> acc || b) false


    let shapeIsSolution rte unt shape =
        if unt |> String.equalsCapInsens "druppel" ||
           unt = "mL" || unt = "µL" then true
        else
            filterRouteShapeUnit rte shape unt
            |> Array.map (fun xs -> xs[6] = "TRUE")
            |> Array.fold (fun acc b -> acc || b) false


    let shapeDoseUnit rts unt shape =
        rts
        |> Array.collect (fun rt -> filterRouteShapeUnit rt shape unt)
        |> Array.fold (fun acc xs ->
            match acc with
            | None   -> Some xs[3]
            | Some u ->
                if u |> String.startsWithCapsInsens xs[3] then acc
                else Some ""
        ) None


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
            un


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
        let un = un |> String.trim |> String.toLower
        un = "milliliter" || un = "druppel" || un = "ml"


    let removeEmptyUnitSubstances (gp : GenericProduct) =
        { gp with
            Substances =
                gp.Substances
                |> Array.filter (fun s ->
                    s.SubstanceUnit <> NA
                )
        }


    let filterGenericProducts gpps =
        gpps
        |> Array.filter (fun gpp ->
            gpp |> hasNoUnit |> not &&
            Constants.excludeShapes
            |> Array.exists (fun s -> gpp.Shape |> String.equalsCapInsens s)
            |> not
        )
        |> Array.map (fun gpp ->
            { gpp with
                GenericProducts =
                    gpp.GenericProducts
                    |> Array.filter (fun gp ->
                        gp.Substances
                        |> Array.forall (fun s ->
                            s.SubstanceUnit
                            |> String.trim
                            |> String.equalsCapInsens NA
                            |> not
                        )
                    )
            }
        )
        |> Array.collect (fun gpp -> gpp.GenericProducts)
        |> Array.map removeEmptyUnitSubstances
        |> Array.filter (fun gp -> gp.Substances |> Array.isEmpty |> not)
        |> Array.distinct


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


    let getFrequencies (drs : DoseRule.DoseRule []) =
        drs
        |> Array.map (fun dr -> $"{dr.Freq.Frequency} {dr.Freq.Time}")
        |> Array.distinct
        |> Array.collect mapFreq
        |> Array.distinct
        |> String.concat ";"


    let getDoseUnit (drs: DoseRule.DoseRule[]) =
        drs
        |> Array.fold(fun acc dr ->
            match acc with
            | None -> Some dr.Unit
            | Some u ->
                if u = dr.Unit then acc
                else Some ""
        ) None


    let loadDataImport (file : string) (sheet : string) xs =
        let wb = new XLWorkbook(file)
        wb.Worksheet(sheet).Cell(2, 3).Value <- xs
        wb.Save()


    let print file sheet xs =
        match file with
        | Some file ->
            xs
            |> Array.map (String.split "\t")
            |> Array.map List.toArray
            |> Array.skip 1
            |> loadDataImport file sheet
        | None -> ()

        xs
        |> String.concat "\n"
        |> File.writeTextToFile $"data/output/{sheet}.csv"

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
        |> print None path


    let routeShapeUnits name =
        GenPresProduct.get true
        //|> Array.filter (fun gpp -> gpp.Shape |> String.toLower |> String.contains "concentraat" )
        |> Array.collect (fun gpp -> gpp.GenericProducts)
        |> Array.collect (fun gp ->
            gp.Route
            |> Array.map (fun r ->
                r |> String.toLower |> String.trim
                , gp.Shape |> String.toLower |> String.trim
                , gp.Substances[0].ShapeUnit |> String.toLower |> String.trim
            )
        )
        |> Array.distinct
        |> Array.map (fun (r, s, u) -> $"{r}\t{s}\t{u}")
        |> print None name


    let createRoutes file name =
        let mapRts = (Array.mapStringHeadings Constants.routeHeadings) >> (String.concat "\t")
        // Get routes and external codes
        // Intra seperated by -
        let rts =
            Names.getItems Names.Route Names.Fifty
            |> Array.filter (fun (_, r) -> r.Contains(",") |> not)
            |> Array.sortBy fst
            |> Array.distinct

        rts
        |> Array.filter (fun (_, r) -> r |> mapRoute |> String.isNullOrWhiteSpace |> not)
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
        |> print file name
        |> ignore

        rts
        |> Array.map snd
        |> Array.map mapRoute
        |> Array.filter (String.isNullOrWhiteSpace >> not)


    let createDoseForms file name routes =
        let mapForms = (Array.mapStringHeadings Constants.doseFormHeadings) >> (String.concat "\t")

        // Get doseforms
        Names.getItems Names.Shape Names.Fifty
        |> Array.distinct
        |> Array.map (fun (id, s) ->
            let s = s |> String.toLower
            let rts =
                GenPresProduct.get true
                |> Array.filter (fun gpp -> gpp.Shape |> String.equalsCapInsens s)
                |> Array.collect (fun gpp -> gpp.Route)
                |> Array.collect (String.splitAt ',')
                |> Array.filter (mapRoute >> String.isNullOrWhiteSpace >> not)
                |> Array.distinct

            {|
                ExternalCode = id
                DoseFormName = s |> String.toLower
                Routes = rts
                OrderingType =
                    mappingRouteShape
                    |> Array.filter (fun xs ->
                        rts
                        |> Array.exists (fun rt ->
                            rt |> String.equalsCapInsens xs[0]
                        ) &&
                        s |> String.toLower = xs[1]

                    )
                    |> Array.fold (fun acc xs ->
                        match acc with
                        | NonInfuse ->
                            if xs[4] = "TRUE" then Both
                            else acc
                        | Both -> Both
                    ) NonInfuse
                IsDrugInSolution =
                    s |> shapeIsSolution "" "" ||
                    s |> shapeInDiluent "" ""
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
                |> Array.filter (String.isNullOrWhiteSpace >> not)
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
        |> Array.append (
            [| "oplosvloeistof"; "emulsie" |]
            |> Array.map (fun s ->
                [|
                    "DoseFormName", s
                    "Routes", routes |> String.concat ";"
                    "DefaultUnit", "mL"
                    "OrderingType", "Both"
                    "IsDrugInSolution", false |> mapBool
                    "Category", Constants.solutionGroup
                    "IsDispensableAmountAllowed", false |> mapBool
                |]
                |> mapForms
            )
        )
        |> Array.append [| Constants.doseFormHeadings |> String.concat "\t" |]
        |> print file name


    let createIngredients file name (gp : GenericProduct[]) =
        let mapIngrs = (Array.mapStringHeadings Constants.ingredientHeadings) >> (String.concat "\t")

        let substs =
            gp
            |> Array.collect (fun gp -> gp.Substances)
            |> Array.filter (fun s ->
                s.SubstanceName |> String.equalsCapInsens "water" |> not &&
                s.GenericName |> String.equalsCapInsens "water" |> not 
            )

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
        |> print file name
        |> ignore



    let createMedications file ingrName medName complName brandName prodName meds =
        let mapMeds = (Array.mapStringHeadings Constants.medicationHeadings) >> (String.concat "\t")
        let mapComp = (Array.mapStringHeadings Constants.complexMedicationHeadings) >> (String.concat "\t")
        let mapBrand = (Array.mapStringHeadings Constants.brandHeadings) >> (String.concat "\t")
        let mapProd = (Array.mapStringHeadings Constants.productHeadings) >> (String.concat "\t")

        let gps =
            meds
            |> filterGenericProducts

        gps |> createIngredients file ingrName

        let meds =
            gps
            |> Array.map (fun gp ->
                let drs =
                    RuleFinder.createFilter
                        None
                        None
                        None
                        (Some gp.Id)
                        ""
                        ""
                        ""
                    |> RuleFinder.find true

                let name =
                    gp.Name
                    |> String.trim
                    |> String.toLower
                    |> String.replace "'" ""

                let grps =
                    ATCGroup.get ()
                    |> Array.filter (fun g ->
                        g.ATC5
                        |> String.contains (gp.ATC |> String.subString 0 4)
                    )
                    |> Array.tryHead

                let rts =
                    GenPresProduct.get true
                        |> Array.filter (fun gpp -> gpp.GenericProducts |> Array.exists ((=) gp))
                        |> Array.collect (fun gpp -> gpp.Route)
                        |> Array.collect (String.splitAt ',')
                        |> Array.filter ((String.equalsCapInsens "Parenteraal") >> not)
                        |> Array.distinct

                let su =
                    gp.Substances[0].ShapeUnit
                    |> mapUnit

                let un =
                    match gp.Shape |> shapeDoseUnit rts gp.Substances[0].ShapeUnit with
                    | Some u when u |> String.isNullOrWhiteSpace |> not -> u
                    | _ ->
                        let subst = gp.Substances[0]

                        if subst.SubstanceQuantity >= subst.GenericQuantity then subst.SubstanceUnit
                        else subst.GenericUnit
                    |> mapUnit

                let assort = gp.Id |> getFormulary

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
                    CodeSnippetName = $"GPK-{gp.Id} {System.Guid.NewGuid().ToString()}"
                    Frequencies =
                        let freqs = drs |> getFrequencies
                        if freqs |> String.isNullOrWhiteSpace then "[All]"
                        else freqs
                    DoseForms = gp.Shape |> String.toLower |> String.trim
                    Routes =
                        rts
                        |> Array.map mapRoute
                        |> Array.filter (String.isNullOrWhiteSpace >> not)
                        |> String.concat ";"
                    AdditivesGroup = "[None]"
                    DiluentsGroup = // "Verduninngen"
                        if gp.Shape |> shapeIsSolution "" gp.Substances[0].ShapeUnit then Constants.diluentsGroup
                        else ""
                    DrugInDiluentGroup = // "Oplossingen"
                        if gp.Shape |> shapeIsSolution "" un then "[None]"
                        else
                            if gp.Shape |> shapeInDiluent "" gp.Substances[0].ShapeUnit ||
                               gp.Shape |> shapeIsSolution "" gp.Substances[0].ShapeUnit then Constants.solutionGroup
                            else
                                "[None]"
                    DrugFamily = "" //g |> Option.map (fun g -> g.AnatomicalGroup |> capitalize) |> Option.defaultValue ""
                    DrugSubfamily = "" //g |> Option.map (fun g -> g.TherapeuticSubGroup |> capitalize) |> Option.defaultValue ""
                    IsFormulary = assort |> List.isEmpty |> not
                    CreateProduct =
                        un <> "keer" &&
                        gp.Substances[0].ShapeUnit |> isSolutionUnit

                    ComplexMedications =
                        if gp.Substances |> Array.length > 4 ||
                           un = "keer" then [||]
                        else
                            let cms =
                                gp.Substances
                                |> Array.map (fun s ->
                                    let q, u =
                                        if s.SubstanceQuantity >= s.GenericQuantity then
                                            s.SubstanceQuantity, s.SubstanceUnit
                                        else
                                            s.GenericQuantity, s.GenericUnit

                                    let u = u |> mapUnit

                                    {|
                                        ComplexMedictionName = name
                                        IngredientName =
                                            s.SubstanceName
                                            |> String.toLower
                                            |> String.trim
                                        Concentration = q
                                        ConcentrationUnit = u 
                                        In =
                                            if (gp.Shape |> shapeIsSolution "" un || un = "dosis") &&
                                               un <> u then "1" else ""
                                        InUnit =
                                            if gp.Shape |> shapeIsSolution "" un |> not &&
                                               un <> "dosis" then ""
                                            else
                                                 if un = "druppel" then "mL" else un
                                    |}
                                )
                                |> Array.groupBy (fun cm -> cm.IngredientName)
                                |> Array.collect (fun (_, cms) ->
                                    match cms |> Array.tryHead with
                                    | None -> [||]
                                    | Some cm ->
                                        [|
                                            {| cm with
                                                Concentration =
                                                    cms
                                                    |> Array.sumBy (fun cm -> cm.Concentration)
                                            |}
                                        |]

                                )

                            if gp.Substances[0].ShapeUnit |> isSolutionUnit ||
                               un |> isSolutionUnit || un = su then [||]
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
                        if r.CreateProduct |> not ||
                           r.Unit = "druppel" || r.Unit = "mL" ||
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
                                    Routes = r.Routes
                                    Format = r.Format
                                    IncrementValue = r.IncrementValue
//                                    Unit = "mL"
                                    DefaultUnit = r.Unit
                                    IsUnknownStrength = "FALSE"
                                    StrengthLEFT = r.ComplexMedications[0].Concentration
                                    StrengthLEFTUnit = r.ComplexMedications[0].ConcentrationUnit
                                    StrengthRIGHT = "1"
                                    StrengthRIGHTUnit = "mL"
                                    DiluentGroup = Constants.solutionGroup
                                    ProductRequiresReconstitution = "FALSE"
                                    IsVolumeKnown = "FALSE"
                                    Volume = "0"
                                    DiluentName =
                                        if r.DoseForms |> String.contains "emulsie" then "emulsie"
                                        else "water"
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
        |> print file brandName
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
        |> print file complName
        |> ignore

        meds
        |> Array.collect (fun p -> p.Products)
        |> Array.map (fun p ->
            [|
                "ExternalCode", p.Id
                "ProductID", p.ProductID
                "ProductName", p.ProductName
                "MedicationName", p.MedicationName
                "Manufacturer", p.Manufacturer
                "DoseForm", p.DoseForm
                "Routes", p.Routes
                "Status", "Active"
                "Format", p.Format
                "IncrementValue", p.IncrementValue |> Double.toStringNumberNLWithoutTrailingZeros
//                "Unit", r.Unit
                "DefaultUnit", p.DefaultUnit
                "IsUnknownStrength", p.IsUnknownStrength
                "StrengthLEFT", p.StrengthLEFT |> Double.toStringNumberNLWithoutTrailingZeros
                "StrengthLEFTUnit", p.StrengthLEFTUnit
                "StrengthRIGHT", p.StrengthRIGHT
                "StrengthRIGHTUnit", p.StrengthRIGHTUnit
                "DiluentGroup", p.DiluentGroup
                "ProductRequiresReconstitution", p.ProductRequiresReconstitution
                "IsVolumeKnown", p.IsVolumeKnown
                "Volume", p.Volume
                "DiluentName", p.DiluentName
            |]
            |> mapProd
        )
        |> Array.append [| Constants.productHeadings |> String.concat "\t" |]
        |> print file prodName
        |> ignore

        meds
        |> Array.map (fun m ->
            [|
                "ExternalCode", m.ExternalCode
                "MedicationName", m.MedicationName
                "Unit", m.Unit
                "ATCCode", m.ATC
                "Status", m.Status
                "Format", m.Format
                "IncrementValue", $"{m.IncrementValue |> Double.toStringNumberNLWithoutTrailingZeros}"
                "CodeSnippetName", m.CodeSnippetName
                "Frequencies", m.Frequencies
                "DoseForms", m.DoseForms
                "Routes", m.Routes
                "AdditivesGroup", m.AdditivesGroup
                "DiluentsGroup", m.DiluentsGroup
                "DrugInDiluentGroup", m.DrugInDiluentGroup
                "DrugFamily", m.DrugFamily
                "DrugSubfamily", m.DrugSubfamily
                "IsFormulary", $"{m.IsFormulary |> mapBool}"

            |]
            |> mapMeds
        )
        |> Array.append (Constants.meds |> Array.map mapMeds)
        |> Array.append [| Constants.medicationHeadings |> String.concat "\t" |]
        |> print file medName |> ignore
        meds


    let createSolutions file solName (meds : GenPresProduct[]) =
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
            gp.Shape |> shapeInDiluent "" su ||
            gp.Shape |> shapeIsSolution "" su
        )
        |> Array.sortBy (fun gp -> gp.Name, gp.Shape, gp.Route)
        |> Array.collect (fun gp ->
            let mapSols = (Array.mapStringHeadings Constants.solutionHeadings) >> (String.concat "\t")

            gp.Route
            |> Array.collect (fun r -> r |> String.splitAt ',')
            |> Array.map mapRoute
            |> Array.filter (String.isNullOrWhiteSpace >> not)
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
                            if gp.Shape |> shapeInDiluent r su then "1" else "0"
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
        |> print file solName


    type ImportConfig =
        {
            Ingredients : string
            Medications : string
            ComplexMedications : string
            Brands : string
            Products : string
            OrderTemplates : string
            ImportFile : string option
        }


    let config =
        {
            Ingredients = "Ingredients"
            Medications = "Medications"
            ComplexMedications = "ComplexMedications"
            Brands = "Brands"
            Products = "Products"
            OrderTemplates = "OrderTemplates"
            ImportFile = Some "data/output/DrugDatabaseForImport.xlsx"
        }


    let createImport (config : ImportConfig) =
        printfn "creating routes"
        let rts =
            createRoutes config.ImportFile "Routes"

        printfn "creating dose forms"
        rts
        |> createDoseForms config.ImportFile "DoseForms"
        |> ignore

        printfn "creating medications"
        createMedications
            config.ImportFile
            config.Ingredients
            config.Medications
            config.ComplexMedications
            config.Brands
            config.Products
//            config.OrderTemplates


