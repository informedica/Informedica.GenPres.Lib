

// #I __SOURCE_DIRECTORY__
// The above directive doesn't work anymore, instead
// the fsi will silent cd to the present script directory
// but still try to run the below load script first.
// So first start the fsi, only then you can run the below
// script.
#load "load.fsx"

#time

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


module Examples =

    open Informedica.Utils.Lib.BCL


    let private toBigRational w =
        match w |> BigRational.fromFloat with
        | Some br -> br
        | None ->
            $"cannot convert {w} to BigRational"
            |> failwith


    let paracetamolSupp weight =
        let w = weight |> toBigRational

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
        |> DrugOrder.setAdjust "paracetamol" w          // Now calculate the scenarios for 10 kg
        |> DrugOrder.evaluate logger.Logger
        |> printScenarios false ["paracetamol"]


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


    let cotrimoxazolTablet weight =
        let w = weight |> toBigRational

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
        |> DrugOrder.setAdjust "cotrimoxazol" w
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


    // Paracetamol drink
    let paracetamolDrink weight =
        let w = weight |> toBigRational
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
        |> DrugOrder.setAdjust "paracetamol" w
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
    let cotrimoxazolDrink weight =
        let w = weight |> toBigRational
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
        |> DrugOrder.setAdjust "cotrimoxazol" w
        |> DrugOrder.evaluate logger.Logger
        //|> List.length
        |> printScenarios false ["sulfamethoxazol"; "trimethoprim"]


    
    // Dopamin infusion calculate scenario's 
    // with a number of standard solutions
    let dopaminStandardConcentrations weight =
        let w = weight |> toBigRational
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
        |> DrugOrder.setAdjust "dopamin infusion" w
        |> DrugOrder.setDoseLimits
            {   DrugOrder.doseLimits with
                    Name = "dopamin infusion"
                    SubstanceName = "dopamin"
                    MinDoseRateAdjust = Some 2N
                    MaxDoseRateAdjust = Some 20N
            }
        |> DrugOrder.evaluate { Log = ignore } //logger.Logger
        //|> Order.calcScenarios2
        |> printScenarios false ["dopamin"]

    // Dopamin infusion calculate scenario's 
    // with a a fixed infusion - dose rate
    let dopaminFixedRate weight =
        let w = weight |> toBigRational
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
        |> DrugOrder.setAdjust "dopamin infusion" w
        |> DrugOrder.evaluate logger.Logger
        |> printScenarios false ["dopamin"]


    // gentamicin
    let gentamicinIV weight =
        let w = weight |> toBigRational
        {
            DrugOrder.drugOrder with
                Id = "1"
                Name = "gentamicin"
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
                                Name = "gentamicin"
                                Quantities = [ 2N; 10N ]
                                TimeUnit = "day"
                                Substances = 
                                    [
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "gentamicin"
                                                Concentrations = [ 10N; 40N ]
//                                                OrderableQuantities = [10N; 20N; 40N; 50N; 100N]
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
        |> DrugOrder.setAdjust "gentamicin" w
        |> DrugOrder.setDoseLimits
            {   DrugOrder.doseLimits with
                    Name = "gentamicin"
                    SubstanceName = "gentamicin"
                    Frequencies = [ 1N ]
                    MinDoseTotalAdjust = Some 4N
                    MaxDoseTotalAdjust = Some 6N
            }
        |> DrugOrder.setSolutionLimits 
            {
                DrugOrder.solutionLimits with
                    Name = "gentamicin"
                    Component = "gentamicin"
                    MinConcentration = Some 1N
                    MaxConcentration = Some 2N
                    DoseCount = Some (1N)
                    //MinTime = (Some (1N/2N))
                    MaxTime = Some (1N/2N)

            }
        |> DrugOrder.evaluate logger.Logger
        |> printScenarios false ["gentamicin"]


Examples.paracetamolDrink 3.
Examples.paracetamolSupp 5.
Examples.cotrimoxazolDrink 3.4
Examples.cotrimoxazolTablet 12.
Examples.dopaminStandardConcentrations 7.4
Examples.dopaminFixedRate 1.3
Examples.gentamicinIV 20.



// Start the logger at an informative level
logger.Start Level.Error
// report output to the fsi
logger.Report ()

// write results to the test.txt in this folder
$"{__SOURCE_DIRECTORY__}/test.txt"
|> logger.Write
