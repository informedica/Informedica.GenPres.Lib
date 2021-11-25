

// #I __SOURCE_DIRECTORY__
// The above directive doesn't work anymore, instead
// the fsi will silent cd to the present script directory
// but still try to run the below load script first.
// So first start the fsi, only then you can run the below
// script.
#load "load.fsx"

open System
open MathNet.Numerics

open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

open Types

module Units = ValueUnit.Units
module DrugConstraint = DrugOrder.DrugConstraint
module Quantity = VariableUnit.Quantity

open OrderLogger

// Just to have a nice print of date time
fsi.AddPrinter<DateTime> (fun dt ->  $"{dt.ToShortDateString()}")

// Start the logger at an informative level
logger.Start Level.Informative

// Paracetamol supp example
// First define the drug order
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "paracetamol"
        Products = 
            [
                { 
                    DrugOrder.productComponent with 
                        Name = "paracetamol"
                        Quantities = [ 1N ]
                        TimeUnit = "day"
                        Substances =
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "paracetamol"
                                        Concentrations = 
                                            [ 60N; 120N; 240N; 500N; 1000N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]
                }
            ]
        Unit = "piece"
        TimeUnit = "day"
        Shape = "supp"
        Route = "rect"
        OrderType = DiscontinuousOrder
}
|> DrugOrder.create 
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "paracetamol"
            Frequencies = [ 2N..4N ]            // Allowed frequencies are 2, 3 or 4 per day
            SubstanceName = "paracetamol"
            MaxDoseQuantity = Some 1000N        // Max per single dose = 1000 mg
            MaxDoseTotal = Some 4000N           // Max daily dose = 4000 mg/day
            MinDoseTotalAdjust = Some 40N       // Min adjusted dose = 40 mg/kg/day
            MaxDoseTotalAdjust = Some 90N       // Max adjusted daily dose = 90 mg/kg/day
    }
|> DrugOrder.setAdjust "paracetamol" 10N        // Now calculate the scenarios for 10 kg
|> DrugOrder.evaluate logger.Logger
|> printScenarios false ["paracetamol"]

// report output to the fsi
logger.Report ()

// write results to the test.txt in this folder
$"{__SOURCE_DIRECTORY__}/test.txt"
|> logger.Write

logger.Start Level.Informative

let printItemConcentration = Order.printItemConcentration
open Informedica.GenSolver.Lib.Variable

let printComponentQuantity o =
    o.Orderable.Components
    |> Seq.map (fun c ->
        $"mapping component: %A{c.OrderableQuantity}" |> printfn "%s"
        c.OrderableQuantity
        |> Quantity.toValueUnitStringList None
        |> fun xs -> $"ValueUnit string list:\n%A{xs}" |> printfn "%s"; xs
        |> Seq.map (fun (_, q) ->
            $"{q} {c.Name |> Name.toString} ({c |> printItemConcentration})"
        )
        |> String.concat ""
    ) |> String.concat " + "


// Drug with multiple items
// cotrimoxazol for infection
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "cotrimoxazol"
        Products = 
            [
                { 
                    DrugOrder.productComponent with 
                        Name = "cotrimoxazol"
                        Quantities = [ 1N ]
                        TimeUnit = "day"
                        Substances =
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sulfamethoxazol"
                                        Concentrations = 
                                            [ 100N; 400N; 800N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "trimethoprim"
                                        Concentrations = 
                                            [ 20N; 80N; 160N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]
                }
            ]
        Unit = "piece"
        TimeUnit = "day"
        Shape = "tablet"
        Route = "or"
        OrderType = DiscontinuousOrder
}
|> DrugOrder.create
// setting dose limits for infection
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "cotrimoxazol"
            Frequencies = [ 2N ]
            SubstanceName = "sulfamethoxazol"
            MaxDoseTotal = Some 1600N
            MaxDoseTotalAdjust = Some 30N
    }
|> DrugOrder.setAdjust "cotrimoxazol" 10N
// is not be necessary when a single product is chosen
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "cotrimoxazol"
            Frequencies = [ 2N ]
            SubstanceName = "trimethoprim"
            MaxDoseTotal = Some 320N
            MaxDoseTotalAdjust = Some 6N
    }
|> DrugOrder.evaluate logger.Logger
|> fun xs ->
    xs |> List.iteri (fun i x -> 
        x |> printComponentQuantity |> (printfn "%i. %s" (i + 1))
    )
    xs
|> printScenarios false ["sulfamethoxazol"; "trimethoprim"]


logger.Start Level.Informative


// Paracetamol drink
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "paracetamol"
        Products = 
            [
                { 
                    DrugOrder.productComponent with 
                        Name = "paracetamol"
                        Quantities = [ 100N ]
                        TimeUnit = "day"
                        Substances =
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "paracetamol"
                                        Concentrations = [ 24N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]
                }
            ]
        Unit = "ml"
        TimeUnit = "day"
        Shape = "drink"
        Route = "or"
        OrderType = DiscontinuousOrder
}
|> DrugOrder.create
|> DrugOrder.setAdjust "paracetamol" 8N
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "paracetamol"
            Frequencies = [ 2N ]
            SubstanceName = "paracetamol"
            MaxDoseQuantity = Some 1000N
            MaxDoseTotal = Some 4000N
            MinDoseTotalAdjust = Some 40N
            MaxDoseTotalAdjust = Some 90N
    }
|> DrugOrder.evaluate logger.Logger
//|> List.length
|> printScenarios false ["paracetamol"]



// Drug with multiple items
// cotrimoxazol drink for infection
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "cotrimoxazol"
        Products = 
            [
                { 
                    DrugOrder.productComponent with 
                        Name = "cotrimoxazol"
                        Quantities = [ 1N ]
                        TimeUnit = "day"
                        Substances =
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sulfamethoxazol"
                                        Concentrations = 
                                            [ 40N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "trimethoprim"
                                        Concentrations = 
                                            [ 8N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]
                }
            ]
        Unit = "ml"
        TimeUnit = "day"
        Shape = "drink"
        Route = "or"
        OrderType = DiscontinuousOrder
}
|> DrugOrder.create
// setting dose limits for infection
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "cotrimoxazol"
            Frequencies = [ 2N ]
            SubstanceName = "sulfamethoxazol"
            MaxDoseTotal = Some 1600N
            MaxDoseTotalAdjust = Some 30N
    }
|> DrugOrder.setAdjust "cotrimoxazol" 10N
|> DrugOrder.evaluate logger.Logger
//|> List.length
|> printScenarios false ["sulfamethoxazol"; "trimethoprim"]



// Dopamin infusion calculate scenario's 
// with a number of standard solutions
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "dopamin infusion"
        Quantities = [ 50N ]
        Divisible = 2N
        Unit = "ml"
        TimeUnit = "day"
        Shape = "infusion fluid"
        Route = "iv"
        Products = 
            [
                { 
                    DrugOrder.productComponent with
                        Name = "dopamin"
                        Quantities = [ 5N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "dopamin"
                                        Concentrations = [ 40N ]
                                        OrderableQuantities = [ 80N; 200N; 400N ]
                                        Unit = "mg"
                                        DoseUnit = "mcg"
                                        TimeUnit = "min"
                                }
                            ]

                }
                { 
                    DrugOrder.productComponent with
                        Name = "saline"
                        Quantities = [ 5000N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sodium"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "chloride"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                            ]

                }
            ]
        OrderType = ContinuousOrder
}
|> DrugOrder.create
|> DrugOrder.setAdjust "dopamin infusion" 3N
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "dopamin infusion"
            SubstanceName = "dopamin"
            MinDoseRateAdjust = Some 2N
            MaxDoseRateAdjust = Some 20N
    }
|> DrugOrder.evaluate logger.Logger
//|> Order.calcScenarios2
|> printScenarios false ["dopamin"]


logger.Report()

// Dopamin infusion calculate scenario's 
// with a a fixed infusion - dose rate
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "dopamin infusion"
        Quantities = [ 50N ]
        Divisible = 1N
        Unit = "ml"
        TimeUnit = "day"
        Shape = "infusion fluid"
        Route = "iv"
        Products = 
            [
                { 
                    DrugOrder.productComponent with
                        Name = "dopamin"
                        Quantities = [ 5N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "dopamin"
                                        Concentrations = [ 40N ]
                                        Unit = "mg"
                                        DoseUnit = "mcg"
                                        TimeUnit = "min"
                                }
                            ]

                }
                { 
                    DrugOrder.productComponent with
                        Name = "saline"
                        Quantities = [ 5000N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sodium"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "chloride"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                            ]

                }
            ]
        OrderType = ContinuousOrder
}
|> DrugOrder.create
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "dopamin infusion"
            Rates = [ 1N ]
            SubstanceName = "dopamin"
            MinDoseRateAdjust = Some 2N
            MaxDoseRateAdjust = Some 20N
    }
|> DrugOrder.setAdjust "dopamin infusion" 10N
|> DrugOrder.evaluate logger.Logger
|> printScenarios false ["dopamin"]

let noLogger : Logger = { Log = ignore }

logger.Start Level.Informative

// gentamicin
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "gentamicin"
        Quantities = [ ]
        Divisible = 1N 
        Unit = "ml"
        TimeUnit = "day"
        Shape = "infusion fluid"
        Route = "iv"
        Products = 
            [
                { 
                    DrugOrder.productComponent with
                        Name = "gentamicin"
                        Quantities = [ 2N; 10N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "gentamicin"
                                        Concentrations = [ 10N; 40N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]

                }
                { 
                    DrugOrder.productComponent with
                        Name = "saline"
                        Quantities = [ 5000N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sodium"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "chloride"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                            ]

                }

            ]
        OrderType = TimedOrder
    }
|> DrugOrder.create
|> DrugOrder.setAdjust "gentamicin" (4N)
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "gentamicin"
            SubstanceName = "gentamicin"
            Frequencies = [ 1N ]
            MinDoseTotalAdjust = Some (4N)
            MaxDoseTotalAdjust = Some (6N)
    }
|> DrugOrder.setSolutionLimits 
    {
        DrugOrder.solutionLimits with
            Name = "gentamicin"
            Component = "gentamicin"
//            MinConcentration = Some (1N)
            MaxConcentration = Some (2N)
            DoseCount = Some (1N)
//            MinTime = (Some (1N/2N))
            MaxTime = (Some (1N/2N))

    }
|> DrugOrder.evaluate logger.Logger
|> printScenarios false ["gentamicin"]

logger.Report ()



[| 62/725; 279/3248; 651/7540; 279/3190; 713/8120; 1023/11600; 31/350; 837/9425; 279/3136; 31/348; 3627/40600; 713/7975; 93/1040; 651/7250; 341/3770; 279/3080; 713/7840; 1023/11200; 93/1015; 899/9800; 5859/63800; 62/675; 837/9100; 868/9425; 2139/23200; 31/336; 3751/40600; 279/3016; 3627/39200; 713/7700; 403/4350; 217/2340; 7533/81200; 93/1000; 744/7975; 217/2320; 341/3640; 31/330; 341/3625; 713/7560; 713/7540; 341/3600; 93/980; 7161/75400; 837/8800; 899/9450; 31/325; 155/1624; 2139/22400; 31/324; 3751/39200; 279/2912; 403/4200; 713/7425; 7533/78400; 279/2900; 651/6760; 217/2250; 7843/81200; 186/1925; 31/320; 341/3510; 31/319; 341/3500; 279/2860; 713/7280; 341/3480; 713/7250; 1023/10400; 31/315; 217/2200; 186/1885; 899/9100; 155/1568; 868/8775; 713/7200; 837/8450; 62/625; 3751/37800; 403/4060; 31/312; 3751/37700; 403/4050; 279/2800; 713/7150; 217/2175; 7533/75400; 7843/78400; 651/6500; 248/2475; 93/928; 217/2160; 6417/63800; 31/308; 1023/10150; 341/3380; 5859/58000; 341/3375; 806/7975; 279/2750; 341/3360; 713/7020; 713/7000; 2387/23400; 651/6380; 93/910; 1023/10000; 5859/57200; 713/6960; 899/8775; 155/1512; 372/3625; 434/4225; 899/8750; 155/1508; 403/3920; 2139/20800; 2387/23200; 837/8125; 3751/36400; 837/8120; 279/2704; 8091/78400; 31/300; 7533/72800; 1953/18850; 3627/35000; 713/6875; 7843/75600; 93/896; 7843/75400; 372/3575; 651/6250; 6417/61600; 1209/11600; 217/2080; 31/297; 1023/9800; 837/8000; 403/3850; 341/3250; 837/7975; 341/3248; 341/3240; 2139/20300; 713/6760; 713/6750; 93/880; 3069/29000; 7161/67600; 62/585; 713/6720; 93/875; 899/8450; 155/1456; 5859/55000; 341/3200; 403/3780; 837/7840; 868/8125; 3751/35100; 31/290; 2139/20000; 899/8400; 217/2025; 3751/35000; 279/2600; 403/3750; 7533/70000; 31/288; 7843/72800; 3751/34800; 62/575; 1209/11200; 713/6600; 744/6875; 2511/23200; 341/3150; 279/2576; 31/286; 217/2000; 1023/9425; 806/7425; 837/7700; 341/3136; 868/7975; 651/5980; 341/3125; 2139/19600; 4433/40600; 341/3120; 279/2552; 217/1980; 3069/28000; 713/6500; 558/5075; 713/6480; 93/845; 7161/65000; 124/1125; 279/2530; 155/1404; 2387/21600; 6417/58000; 899/8125; 31/280; 1953/17600; 3751/33800; 899/8100; 837/7540; 8091/72800; 403/3625; 1023/9200; 217/1950; 155/1392; 713/6400; 7533/67600; 279/2500; 3751/33600; 899/8050; 7843/70200; 713/6380; 93/832; 403/3600; 837/7475; 7843/70000; 2511/22400; 6417/57200; 651/5800; 31/276; 1023/9100; 3627/32200; 5859/52000; 7843/69600; 31/275; 341/3024; 217/1920; 341/3016; 4433/39200; 713/6300; 651/5750; 279/2464; 9207/81200; 2139/18850; 341/3000; 3627/31900; 651/5720; 279/2450; 341/2990; 713/6250; 713/6240; 186/1625; 93/812; 6417/56000; 155/1352; 899/7840; 2387/20800; 31/270; 837/7280; 217/1885; 403/3500; 899/7800; 279/2420; 155/1344; 3751/32500; 837/7250; 93/805; 1953/16900; 8091/70000; 217/1875; 713/6160; 3751/32400; 5859/50600; 403/3480; 7533/65000; 7843/67600; 868/7475; 93/800; 3751/32200; 341/2925; 186/1595; 279/2392; 6417/55000; 7843/67200; 899/7700; 403/3450; 868/7425; 1023/8750; 7533/64400; 837/7150; 341/2912; 5859/50000; 806/6875; 4433/37800; 31/264; 9207/78400; 2139/18200; 341/2900; 744/6325; 3627/30800; 713/6050; 217/1840; 3069/26000; 7533/63800; 62/525; 651/5500; 341/2880; 1116/9425; 341/2875; 93/784; 713/6000; 899/7560; 372/3125; 1209/10150; 31/260; 2387/20000; 403/3375; 837/7000; 155/1296; 8091/67600; 434/3625; 7161/59800; 899/7500; 403/3360; 713/5940; 1953/16250; 3751/31200; 279/2320; 899/7475; 155/1288; 2697/22400; 217/1800; 7843/65000; 2511/20800; 93/770; 1209/10000; 7843/64800; 5859/48400; 1023/8450; 899/7425; 31/256; 713/5880; 279/2300; 434/3575; 341/2808; 155/1276; 2139/17600; 837/6875; 341/2800; 713/5850; 279/2288; 1953/16000; 9207/75400; 403/3300; 2139/17500; 7533/61600; 899/7350; 1953/15950; 341/2784; 31/253; 279/2275; 3069/25000; 713/5800; 372/3025; 31/252; 217/1760; 93/754; 1209/9800; 6417/52000; 713/5775; 7161/58000; 899/7280; 341/2760; 217/1755; 2511/20300; 713/5760; 837/6760; 31/250; 155/1248; 2387/19200; 403/3240; 186/1495; 8091/65000; 279/2240; 713/5720; 217/1740; 899/7200; 341/2730; 3751/30000; 3627/29000; 403/3220; 651/5200; 62/495; 3751/29900; 2511/20000; 8091/64400; 7843/62400; 899/7150; 217/1725; 155/1232; 1023/8125; 403/3200; 7533/59800; 341/2704; 9889/78400; 868/6875; 341/2700; 403/3190; 93/736; 9207/72800; 31/245; 2139/16900; 2387/18850; 4433/35000; 279/2200; 341/2688; 1023/8050; 124/975; 713/5600; 5859/46000; 4433/34800; 806/6325; 558/4375; 3751/29400; 93/728; 1023/8000; 403/3150; 899/7020; 31/242; 2511/19600; 93/725; 6417/50000; 217/1690; 899/7000; 434/3375; 651/5060; 837/6500; 248/1925; 403/3125; 31/240; 3751/29000; 372/2875; 3627/28000; 155/1196; 713/5500; 2697/20800; 2387/18400; 7533/58000; 341/2625; 837/6440; 93/715; 651/5000; 3751/28800; 899/6900; 155/1188; 713/5460; 1953/14950; 7843/60000; 899/6875; 837/6400; 9889/75600; 403/3080; 341/2600; 837/6380; 8091/61600; 1209/9200; 217/1650; 9207/70000; 341/2592; 2139/16250; 7533/57200; 899/6825; 155/1176; 3627/27500; 4433/33600; 713/5400; 558/4225; 93/704; 3069/23200; 837/6325; 341/2576; 31/234; 6417/48400; 2387/18000; 93/700; 899/6760; 5859/44000; 899/6750; 2511/18850; 403/3025; 7843/58800; 3069/23000; 217/1625; 31/232; 2139/16000; 899/6720; 837/6250; 217/1620; 3751/28000; 279/2080; 31/231; 403/3000; 651/4840; 7533/56000; 155/1152; 31/230; 2697/20000; 713/5280; 186/1375; 8091/59800; 341/2520; 155/1144; 217/1600; 403/2970; 713/5250; 9889/72800; 837/6160; 3751/27600; 2387/17550; 7843/57600; 9207/67600; 899/6600; 62/455; 341/2500; 2511/18400; 1953/14300; 341/2496; 4433/32400; 1023/7475; 7533/55000; 3069/22400; 403/2940; 713/5200; 868/6325; 1116/8125; 1209/8800; 3751/27300; 93/676; 2697/19600; 4433/32200; 7161/52000; 31/225; 279/2024; 2511/18200; 1209/8750; 899/6500; 837/6050; 31/224; 558/4025; 899/6480; 434/3125; 2139/15400; 3751/27000; 217/1560; 341/2450; 279/2000; 806/5775; 899/6440; 403/2880; 837/5980; 7843/56000; 403/2875; 155/1104; 899/6400; 9889/70200; 31/220; 2387/16900; 9889/70000; 8091/57200; 713/5040; 651/4600; 9207/65000; 124/875; 155/1092; 1953/13750; 341/2400; 899/6325; 279/1960; 341/2392; 713/5000; 2511/17600; 899/6300; 9207/64400; 93/650; 7161/50000; 3627/25300; 434/3025; 2511/17500; 31/216; 7843/54600; 899/6250; 403/2800; 899/6240; 279/1936; 3751/26000; 93/644; 8091/56000; 217/1500; 7533/52000; 279/1925; 341/2352; 3627/25000; 217/1495; 93/640; 713/4900; 837/5750; 6417/44000; 899/6160; 403/2760; 1023/7000; 9889/67600; 837/5720; 5859/40000; 403/2750; 2697/18400; 155/1056; 2387/16250; 186/1265; 8091/55000; 9889/67200; 713/4840; 3069/20800; 31/210; 4433/30000; 651/4400; 341/2304; 2697/18200; 341/2300; 713/4800; 2511/16900; 899/6050; 93/625; 3751/25200; 7533/50600; 31/208; 2387/16000; 1116/7475; 837/5600; 2139/14300; 899/6000; 3627/24200; 341/2275; 3751/25000; 1209/8050; 1953/13000; 124/825; 899/5980; 7533/50000; 217/1440; 4433/29400; 7843/52000; 434/2875; 93/616; 1209/8000; 279/1840; 217/1430; 186/1225; 9889/65000; 837/5500; 341/2240; 403/2640; 2139/14000; 899/5880; 2387/15600; 155/1012; 2697/17600; 279/1820; 3069/20000; 403/2625; 9889/64400; 93/605; 155/1008; 4433/28800; 9207/59800; 2697/17500; 713/4620; 1953/12650; 341/2208; 2511/16250; 31/200; 93/598; 8091/52000; 7843/50400; 7533/48400; 899/5775; 7161/46000; 2511/16100; 899/5760; 558/3575; 341/2184; 3751/24000; 899/5750; 3069/19600; 713/4550; 2511/16000; 1209/7700; 899/5720; 217/1380; 1023/6500; 3627/23000; 217/1375; 31/196; 4433/28000; 9889/62400; 279/1760; 31/195; 403/2530; 279/1750; 2387/14950; 1023/6400; 8091/50600; 403/2520; 155/968; 6417/40000; 899/5600; 4433/27600; 837/5200; 62/385; 403/2500; 1953/12100; 31/192; 93/575; 713/4400; 341/2100; 93/572; 651/4000; 899/5520; 2511/15400; 3751/23000; 7843/48000; 899/5500; 372/2275; 7533/46000; 341/2080; 217/1320; 9207/56000; 403/2450; 2139/13000; 899/5460; 3627/22000; 62/375; 9889/59800; 837/5060; 93/560; 403/2420; 3069/18400; 217/1300; 8091/48400; 837/5000; 2697/16100; 155/924; 403/2400; 2511/14950; 9889/58800; 31/184; 2697/16000; 3069/18200; 93/550; 341/2016; 713/4200; 899/5280; 31/182; 341/2000; 837/4900; 7533/44000; 899/5250; 279/1625; 31/180; 1209/7000; 899/5200; 837/4840; 217/1250; 279/1600; 403/2310; 341/1950; 2697/15400; 2511/14300; 4433/25200; 31/176; 9889/56000; 9207/52000; 31/175; 1953/11000; 341/1920; 713/4000; 899/5040; 3751/21000; 93/520; 7161/40000; 2511/14000; 899/5000; 217/1200; 9889/54600; 279/1540; 3627/20000; 1023/5600; 403/2200; 899/4900; 2387/13000; 8091/44000; 837/4550; 31/168; 4433/24000; 93/500|]
|> Array.length

31./2160000000. = 62./725. * 1./8640000.

62./725. = (31./2160000000.) / (29./172800000.)


(837./5600.)/ ((31./17280000000.) / (1./86400000.))