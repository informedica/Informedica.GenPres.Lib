
#load "load.fsx"

#time

open System.Collections.Generic


open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.GenCore.Lib.Types.ZIndex
open Informedica.ZIndex.Lib


// File
File.exists <| FilePath.GStandPath + "BST000T"


Json.clearCache ()
// Load all
printfn "Loading GenPresProduct ..."
GenPresProduct.load true
printfn "Loading ATCGroup ..."
ATCGroup.load ()
printfn "Loading DoseRule ..."
DoseRule.load ()


// Print log genericproducts
let logGenericProducts () =
    printfn "Start ..."
    let _log = List<string>()
    let logf s =
        _log.Add(s)
    GenericProduct.getWithLog logf [] |> ignore
    _log.ToArray()
    |> Array.distinct
    |> Array.iter (printfn "%s")

logGenericProducts ()


// ProductRange
ProductRange.data ()
//|> Array.filter (fun d -> d.GPK |> Option.isNone)
|> Array.iter (printfn "%A")

// Substance
Substance.get()
|> Array.sortBy (fun s -> s.Name)
|> Array.iter (fun s -> printfn $"{s.Name} {s.Mole}")


Substance.get ()
|> Array.filter (fun s -> s.Name |> String.startsWithCapsInsens "clavulaan")


// Consumer product
ConsumerProduct.get(100)
|> printf "%A"

GenPresProduct.get true
|> Seq.length


// GenPres product
GenPresProduct.get true
|> Seq.sortBy (fun gpp -> gpp.Name, gpp.Shape, gpp.Routes)
|> Seq.map (fun gpp -> $"""{gpp.Name}, {gpp.Shape}, {gpp.Routes |> String.concat "/"}""")
|> Seq.take 200
|> Seq.iter (fun n -> printfn "%s" n)


GenPresProduct.filter true "paracetamol" "" "oraal"
|> Array.map GenPresProduct.toString
|> Array.iter (printfn "%s")

// Dose rules
DoseRule.get ()
|> Array.length

DoseRule.get ()
|> Array.take 100
|> Array.map DoseRule.toString2
|> Array.iter (printfn "%s")


// Get all possible dose units
DoseRule.get()
|> Array.map (fun dr -> dr.Unit)
|> Array.distinct
|> Array.iter (printfn "%s")


// Get all possible patient gender
DoseRule.get()
|> Array.map (fun dr -> dr.Gender)
|> Array.distinct
|> Array.iter (printfn "%s")


// Get all dose rules for the filter
RuleFinder.createFilter None None None None "PARACETAMOL" "tablet" "ORAAL"
|> RuleFinder.find true
|> RuleFinder.convertToResult


DoseRule.get()
|> Array.map (fun dr -> dr.Indication)
|> Array.distinct
|> Array.sort
|> Array.iter (printfn "%s")
//|> Array.length

// Get all distinct indications
DoseRule.indications ()
|> Array.iter (printfn "%s")

// Get all distinct routes
DoseRule.routes ()
|> Array.iter (printfn "%s")

type FrequencyTime = { Value : float; Unit : string }

// Get all possible freq Time
DoseRule.frequencies ()
|> Array.map (fun f ->
    f.Time
    |> String.replace "per " ""
)
|> Array.distinct

// Get all distinct frequencies
DoseRule.frequencies ()
|> Array.sortBy (fun f ->
    let t =
        f.Time
        |> String.replace "per " ""
        |> String.split " "
        |> function
        | [u] ->
            { Value = 1.; Unit = u }
        | [v;u] ->
            printfn "%s" v
            { Value = v |> Double.parse; Unit = u }
        | _ -> { Value = 0.; Unit = ""}

    (t.Unit)
)
|> Array.iter (fun f -> printfn "%s %s" (f.Frequency |> string) (f.Time))


DoseRule.frequencies ()
|> Array.sortBy(fun f -> f.Time, f.Frequency)
|> Array.map (fun f ->
    $"{f.Frequency} {f.Time}"
)
|> Array.distinct
|> Array.iter(printfn "%s")

// Get all distinct frequencies times
DoseRule.frequencies ()
|> Array.map (fun f -> f.Time)
|> Array.distinct
|> Array.sort
|> Array.iter (printfn "%s")


DoseRule.get()
|> Array.filter (fun dr ->
    dr.Freq.Time |> String.equalsCapInsens "per dag" &&
    dr.Freq.Frequency > 24m
)
|> Array.iter (fun dr -> dr |> DoseRule.toString ", " |> (printfn "%s"))



GenPresProduct.filter true "amoxiciline" "" "iv"
|> Array.collect (fun gpp ->
    gpp.GenericProducts
)
|> Array.map (fun gp ->
    gp.Name
)

// Get alle names and route
GenPresProduct.get true
|> Array.map (fun gpp -> gpp.Name, gpp.Routes)
|> Array.sort
|> Array.distinct
|> Array.collect (fun (nm, rts) ->
    rts
    |> Array.map (fun rt -> nm, rt)
)
|> Array.iter (fun (nm, rt) ->
    printfn "%s, %s" nm rt
)

// Get all dose rules for age 12 months weight 10 kg paracetamol rect
RuleFinder.createFilter (Some 12m) (Some 10m) None None "paracetamol" "" ""
|> RuleFinder.find true
|> Array.map (fun r -> DoseRule.toString ", " r |> printfn "%s"; r)
|> RuleFinder.convertToResult


DoseRule.get ()
|> Array.filter (fun dr ->
    dr.Freq.Time = "eenmalig" && dr.Freq.Frequency > 1.0m
)
|> Array.iter (fun dr ->
    dr
    |> DoseRule.toString ", "
    |> printfn "%s"
)

GenPresProduct.get true
|> Array.iter (fun gpp ->
    let dbls =
        GenPresProduct.get true
        |> Array.filter (fun gpp_ -> gpp.Name = gpp_.Name && gpp.Shape = gpp_.Shape)
    if dbls |> Array.length > 1 then
        dbls
        |> Array.iter (fun gpp__ ->
            printfn "%s %s %s" gpp__.Name gpp__.Shape (gpp__.Routes |> String.concat "/")
        )
)

GenPresProduct.get true
|> Array.collect (fun gpp ->
    gpp.GenericProducts
    |> Array.collect (fun gp ->
        gp.Substances
        |> Array.map (fun s -> s.GenericUnit)
    )
) |> Array.distinct |> Array.sort

DoseRule.get ()
|> Array.filter (fun dr ->
    dr.PrescriptionProduct |> Array.length > 0 &&
    dr.TradeProduct |> Array.length = 0
)
|> Array.map (DoseRule.toString ", ")
|> Array.iter (printfn "%s")

let rts =
    DoseRule.get()
    |> Array.collect (fun dr ->
        dr.Routes
    ) |> Array.sort |> Array.distinct

GenPresProduct.getRoutes()
|> Array.filter (fun r ->
    rts
    |> Array.exists ((=) r)
    |> not
)

RuleFinder.createFilter None None None None "diclofenac" "" ""
|> RuleFinder.find true
|> Array.map (fun dr ->
    (dr.Routes |> Array.map RuleFinder.createRoute, dr)
)
|> Array.groupBy (fun (r, _) ->
    r
)
|> Array.map (fun (r, drs) ->
    let drs =
        drs
        |> Array.map snd
        |> Array.groupBy (fun dr ->
            (dr.Gender, dr.Age, dr.Weight, dr.BSA)
        )
    (r, drs)
)
|> Array.map (fun (r, drs) ->
    let drs =
        drs |> Array.map (fun (pat, drs) ->
            (pat, drs |> Array.map (DoseRule.toString ", ") |> Array.distinct)
        )
    (r, drs)
)


DoseRule.get ()
|> Array.map (fun dr ->
    dr.Freq
)
|> Array.distinct
|> Array.sortBy (fun fr -> fr.Time)
|> Array.map DoseRule.freqToString
|> Array.iter (printfn "%s")

DoseRule.get ()
|> Array.filter (fun dr ->
    dr.Freq.Time |> String.contains "eenmalig" &&
    dr.Freq.Frequency > 1.0m
)
|> Array.map (DoseRule.toString ", ")
|> Array.iter (printfn "%s")

Zindex.BST711T.records ()
|> Array.map (fun r ->
    Names.getName r.GPNMNR Names.Full
)
|> Array.filter (fun n ->
    n |> String.toLower |> String.contains "meropenem"
)

GenPresProduct.get true
|> Array.length


Zindex.BST921T.records ()
|> Array.take 100


printfn "start"
GenericProduct.get []
|> Array.iter (fun gp ->
    if gp.Substances |> Array.isEmpty then
        printfn "%s" gp.Name
)

// Get all unique products
GenPresProduct.get true
|> Array.map (fun gpp ->
    gpp.Name
)
|> Array.sort
|> Array.distinct
|> Array.iter (printfn "%s")

// check if each substance exists
GenPresProduct.get true
|> Array.forall (fun gpp ->
    let ss =
        gpp.GenericProducts
        |> Array.collect (fun gp ->
            gp.Substances
            |> Array.map (fun s -> s.SubstanceName)
        )

    gpp.Name |> String.split "/"
    |> List.forall (fun sn ->
        ss
        |> Array.exists (fun s -> s = sn)
    )
)

// look for each generic product if there are doserules
GenPresProduct.get true
|> Array.sortBy (fun gpp -> gpp.Name)
|> Array.filter (fun gpp ->
    gpp.GenericProducts
    |> Array.exists (fun gp ->
        let zeroRules =
            RuleFinder.createFilter None  None None (Some gp.Id) "" "" ""
            |> RuleFinder.find true
            |> Array.length
            |> fun c -> c = 0
        if zeroRules then printfn "no doserules for: %i - %s" gp.Id gp.Name
        zeroRules
    )
)
|> Array.length



open RuleFinder
open DoseRule

let freqDoseToString (fd : FreqDose) =
    let minmaxToString u { Min = min; Max = max } =
        match min, max with
        | Some min, Some max ->
            sprintf "%A - %A %s" min max u
        | Some min, None ->
            sprintf "vanaf %A %s" min u
        | None, Some max ->
            sprintf "tot %A %s" max u
        | None, None -> ""

    let norm = fd.NormDose |> minmaxToString fd.Unit
    let abs = fd.AbsDose |> minmaxToString fd.Unit
    let perKg = fd.NormKg |> minmaxToString (fd.Unit + "/kg")
    let absPerKg = fd.AbsKg |> minmaxToString (fd.Unit + "/kg")
    let perM2 = fd.NormM2 |> minmaxToString (fd.Unit + "/m2")
    let absPerM2 = fd.AbsM2 |> minmaxToString (fd.Unit + "/m2")

    let doseLabel lbl s =
        if s |> String.isNullOrWhiteSpace then ""
        else lbl + ": " + s

    printfn "%A %s %s %s %s %s %s %s"
        fd.Freq.Frequency
        fd.Freq.Time
        (norm |> doseLabel "norm dose")
        (abs |> doseLabel "abs dose")
        (perKg |> doseLabel "norm dose")
        (absPerKg |> doseLabel "abs dose")
        (perM2 |> doseLabel "norm dose")
        (absPerM2 |> doseLabel "abs dose")



// Get for paracetamol the dose result
GenPresProduct.get true
|> Array.filter (fun gpp ->
    gpp.Name |> String.equalsCapInsens "paracetamol"
)
|> Array.collect (fun gpp ->
    gpp.GenericProducts
    |> Array.map (fun gp ->
        RuleFinder.createFilter None None None (Some gp.Id) "" "" ""
        |> RuleFinder.find true
        |> RuleFinder.convertToResult
    )
)
|> Array.iter (fun dro ->
    match dro with
    | Some dr ->
        printfn "%s %s" dr.Product.Name (dr.Product.Routes |> String.concat "/")
        dr.DoseRules |> Array.iter (printfn "%s")
        dr.Doses
        |> Array.iter (fun d ->
            d |> freqDoseToString
        )
    | None -> ()
)


// Get dose result routes for paracetamol
// contains intradermal route??
GenPresProduct.get true
|> Array.filter (fun gpp ->
    gpp.Name |> String.equalsCapInsens "paracetamol"
)
|> Array.collect (fun gpp ->
    gpp.GenericProducts
    |> Array.map (fun gp ->
        RuleFinder.createFilter None None None (Some gp.Id) "" "" ""
        |> RuleFinder.find true
        |> RuleFinder.convertToResult
    )
)
|> Array.collect (fun dro ->
    match dro with
    | Some dr ->
        dr.Product.Routes
    | None -> Array.empty
)
|> Array.distinct


// Look for parenteral dose rules (should not exist?)
DoseRule.get ()
|> Array.filter (fun dr ->
    let parent =
        dr.Routes
        |> Array.exists (String.equalsCapInsens "parenteraal")
    if parent then dr |> DoseRule.toString " " |> printfn "%s"
    parent
)
|> Array.length


// Find a product
let search n =
    let contains s2 s1 =
        s1
        |> String.toLower
        |> String.contains (s2 |> String.toLower)

    GenPresProduct.get true
    |> Array.filter (fun gpp ->
        gpp.Name |> contains n ||
        gpp.GenericProducts
        |> Array.exists (fun gp ->
            gp.Name |> contains n ||
            gp.PrescriptionProducts
            |> Array.exists (fun pp ->
                pp.TradeProducts
                |> Array.exists (fun tp ->
                    tp.Label
                    |> contains n
                )
            )
        )
    )
    |> Array.collect (fun gpp ->
        gpp.GenericProducts
        |> Array.collect (fun gp ->
            gp.ATC
            |> ATCGroup.findByATC5 ()
        )
    )

search "augmentin"
|> Array.length


GenericProduct.get []
|> Array.filter (fun gp ->
    gp.Name
    |> String.toLower
    |> String.contains "insuline"
)
|> Array.map (fun gp ->
    gp.Id, gp.Label, gp.Substances[0].SubstanceUnit
)

GenericProduct.get []
|> Array.filter (fun gp ->
    gp.Id = 130055
)


// get barcodes
GenericProduct.get []
|> Array.take 100
|> Array.collect (fun gp ->
//    gp.Label,
    gp.PrescriptionProducts
    |> Array.collect (fun pp ->
        pp.TradeProducts
        |> Array.collect (fun tp ->
            tp.ConsumerProducts
            |> Array.collect (fun cp ->
                cp.BarCodes
                |> Array.map (fun b -> (gp.Id, b))
            )
        )
    )
)
|> Array.groupBy fst
|> Array.map (fun (gpk, bc) ->
    gpk, bc |> Array.map snd
)
|> Array.iter (fun (gpk, bc) -> printfn $"gpk: {gpk}: %A{bc}")


// create product file
GenPresProduct.get true
|> Array.collect (fun gpp ->
    gpp.GenericProducts
    |> Array.collect (fun gp ->
        gp.Substances
        |> Array.collect (fun s ->
            gp.ATC
            |> ATCGroup.findByATC5 ()
            |> Array.map (fun atc ->
                {|
                    GPK = gp.Id
                    ATC = atc.ATC5
                    MainGroup = atc.AnatomicalGroup
                    SubGroup = atc.TherapeuticMainGroup
                    Generic = gpp.Name
                    TallMan = ""
                    Synonyms =
                        gp.PrescriptionProducts
                        |> Array.collect (fun pp ->
                            pp.TradeProducts
                            |> Array.map (fun tp -> tp.Brand)
                        )
                        |> Array.filter (String.isNullOrWhiteSpace >> not)
                        |> String.concat ";"
                    Product =
                        gp.PrescriptionProducts
                        |> Array.collect (fun pp ->
                            pp.TradeProducts
                            |> Array.map (fun tp -> tp.Label)
                        )
                        |> Array.tryHead
                        |> Option.defaultValue ""
                    Label = gp.Label
                    Shape = gpp.Shape
                    ShapeQuantity =
                        gp.PrescriptionProducts
                        |> Array.fold (fun acc pp ->
                            if pp.Quantity <> acc then pp.Quantity else acc
                        ) 1.0m
                        |> fun v -> if v <= 0m then 1m else v
                    ShapeVol = ""
                    ShapeUnit = gpp.Unit
                    Substance = s.SubstanceName
                    SubstanceQuantity = s.SubstanceQuantity
                    SubstanceUnit = s.SubstanceUnit
                    MultipleQuantity = 0m
                    MultipleUnit = ""
                    Divisible = 1m
                |}
            )
        )
    )
)
//|> Array.take 10
|> Array.map (fun r ->
    let strToStr s = $"\"{s}\""
    let numToStr n = $"{n}"
    [
        r.GPK |> numToStr
        r.ATC |> strToStr
        r.MainGroup |> strToStr
        r.SubGroup |> strToStr
        r.Generic |> String.toLower |> strToStr
        r.TallMan |> strToStr
        r.Synonyms |> strToStr
        r.Product |> strToStr
        r.Label |> strToStr
        r.Shape |> String.toLower |> strToStr
        r.ShapeQuantity |> strToStr
        r.ShapeVol
        r.ShapeUnit |> String.toLower |> strToStr
        r.Substance |> String.toLower |> strToStr
        r.SubstanceQuantity |> strToStr
        r.SubstanceUnit |> String.toLower |> strToStr
        r.SubstanceQuantity / (r.Divisible |> decimal) |> numToStr
        r.SubstanceUnit
        r.Divisible |> numToStr
    ]
    |> String.concat "\t"

)
|> Array.distinct
|> Array.append [|
    [
        "GPK"
        "ATC"
        "MainGroup"
        "SubGroup"
        "Generic"
        "TallMan"
        "Synonyms"
        "Product"
        "Label"
        "Shape"
        "ShapeQuantity"
        "ShapeVol"
        "ShapeUnit"
        "Substance"
        "SubstanceQuantity"
        "SubstanceUnit"
        "MultipleQuantity"
        "MultipleUnit"
        "Divisible"
    ]
    |> String.concat "\t"
|]
|> String.concat "\n"
|> fun s -> System.IO.File.WriteAllText("ZIndexProducts.csv", s)


// route shape
GenPresProduct.get true
|> Array.collect (fun gpp ->
    gpp.Routes
    |> Array.map (fun r ->
        r |> String.toLower,
        gpp.Shape |> String.toLower
    )
)
|> Array.distinct
|> Array.iter (fun (r, s) -> printfn $"{r}\t{s}")


Names.getItems Names.Route Names.Fifty
|> Array.iter (fun (id, s) -> printfn $"{id}: {s}")


Names.getItems Names.ShapeUnit Names.TwentyFive
|> Array.map snd
|> Array.zip (Names.getItems Names.ShapeUnit Names.Fifty |> Array.map snd)
|> Array.map (fun (l, s) ->
    l |> String.trim |> String.toLower,
    s |> String.trim |> String.toLower
)
|> Array.iter (fun (l, s) -> printfn $"{l}\t{s}")