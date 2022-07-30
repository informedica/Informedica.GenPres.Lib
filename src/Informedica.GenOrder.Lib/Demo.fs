namespace Informedica.GenOrder.Lib


module Examples =

    open System
    open MathNet.Numerics

    open Informedica.Utils.Lib.BCL
    open Informedica.GenUnits.Lib
    open Informedica.GenOrder.Lib

    open Types

    module Units = ValueUnit.Units
    module DrugConstraint = DrugOrder.DrugConstraint
    module Quantity = VariableUnit.Quantity

    open OrderLogger

    // print an order list
    let toScenarios sn (sc : Order list) =
        sc
        |> List.mapi (fun i o ->
            o
            |> Order.printPrescription sn
            |> fun (pres, prep, adm) ->
                {
                    No = i
                    Route = ""
                    Prescription = pres
                    Preparation = prep
                    Administration = adm
                }
        )


    let evaluate sn =
        DrugOrder.evaluate OrderLogger.logger.Logger
        >> toScenarios sn


    let private toBigRational w =
        match w |> BigRational.fromFloat with
        | Some br -> br
        | None ->
            $"cannot convert {w} to BigRational"
            |> failwith


    let setDoses doses o =
        doses
        |> List.fold (fun acc dose ->
            acc
            |> DrugOrder.setDoseLimits dose
        ) o
        

    let orders =
        [
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
                                                    Concentrations = 
                                                        [ 10N ]
                                                    Unit = "mg"
                                                    DoseUnit = "mg"
                                                    TimeUnit = "day"
                                            }
                                        ]
                            }
                        ]
                    Unit = "ml"
                    TimeUnit = "day"
                    Shape = "infusion fluid"
                    Route = "iv"
                    OrderType = DiscontinuousOrder
            }

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

            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "cotrimoxazol"
                    Divisible = 2N
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
            
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "cotrimoxazol"
                    Divisible = 2N
                    Products = 
                        [
                            { 
                                DrugOrder.productComponent with 
                                    Name = "cotrimoxazol"
                                    Quantities = [ 5N ]
                                    TimeUnit = "day"
                                    Substances =
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "sulfamethoxazol"
                                                    Concentrations = 
                                                        [ 80N ]
                                                    Unit = "mg"
                                                    DoseUnit = "mg"
                                                    TimeUnit = "day"
                                            }
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "trimethoprim"
                                                    Concentrations = 
                                                        [ 16N ]
                                                    Unit = "mg"
                                                    DoseUnit = "mg"
                                                    TimeUnit = "day"
                                            }
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "proplyleenglycol"
                                                    Concentrations = 
                                                        [ 410N ]
                                                    Unit = "mg"
                                                    DoseUnit = "mg"
                                                    TimeUnit = "day"
                                            }
                                        ]
                            }
                        ]
                    Unit = "ml"
                    TimeUnit = "day"
                    Shape = "infusion fluid"
                    Route = "iv"
                    OrderType = DiscontinuousOrder
            }

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
            
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "gentamicin"
                    Quantities = [ 1N; 2N; 5N; 10N; 50N; 100N ]
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

            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "cotrimoxazol (Rokiprim)"
                    Divisible = 2N
                    Products = 
                        [
                            { 
                                DrugOrder.productComponent with 
                                    Name = "cotrimoxazol"
                                    Quantities = [ 5N ]
                                    TimeUnit = "day"
                                    Substances =
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "sulfamethoxazol"
                                                    Concentrations = 
                                                        [ 80N ]
                                                    Unit = "mg"
                                                    DoseUnit = "mg"
                                                    TimeUnit = "day"
                                            }
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "trimethoprim"
                                                    Concentrations = 
                                                        [ 16N ]
                                                    Unit = "mg"
                                                    DoseUnit = "mg"
                                                    TimeUnit = "day"
                                            }
                                        ]
                            }
                        ]
                    Unit = "ml"
                    TimeUnit = "day"
                    Shape = "infusion fluid"
                    Route = "iv"
                    OrderType = DiscontinuousOrder
            }
        ]


    let listOrders () =
        orders
        |> List.map (fun o ->
            o.Name, o.Shape, o.Route
        )


    let getOrders n r =
        orders
        |> List.filter (fun o -> o.Name = n && o.Route = r)


    // Paracetamol drink
    let paracetamolDrink weight doses =
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
        |> setDoses doses
        |> evaluate ["paracetamol"]

    /// calculate the scenarios for a drug order
    /// o with substancenames ns and doses doses
    let calculate weight ns doses o =
        let w = weight |> toBigRational

        o
        |> DrugOrder.create
        |> DrugOrder.setAdjust o.Name w
        |> setDoses doses
        |> evaluate ns


    let paracetamolSupp weight doses =
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
        |> DrugOrder.setAdjust "paracetamol" w          // Now calculate the scenarios for 10 kg
        |> setDoses doses
        |> evaluate ["paracetamol"]


    let paracetamolIV weight doses =
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
                                Quantities = [ 100N ]
                                TimeUnit = "day"
                                Substances =
                                    [
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "paracetamol"
                                                Concentrations = 
                                                    [ 10N ]
                                                Unit = "mg"
                                                DoseUnit = "mg"
                                                TimeUnit = "day"
                                        }
                                    ]
                        }
                    ]
                Unit = "ml"
                TimeUnit = "day"
                Shape = "infusion fluid"
                Route = "iv"
                OrderType = DiscontinuousOrder
        }
        |> DrugOrder.create 
        |> setDoses doses
        |> DrugOrder.setAdjust "paracetamol" w          // Now calculate the scenarios for 10 kg
        |> evaluate ["paracetamol"]


    let cotrimoxazolTablet weight doses =
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
        |> DrugOrder.setAdjust "cotrimoxazol" w
        |> setDoses doses
        |> evaluate ["sulfamethoxazol"; "trimethoprim"]

    // Drug with multiple items
    // cotrimoxazol drink for infection
    let cotrimoxazolDrink weight doses =
        let w = weight |> toBigRational

        {
            DrugOrder.drugOrder with
                Id = "1"
                Name = "cotrimoxazol"
                Divisible = 2N
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
        |> setDoses doses
        |> DrugOrder.setAdjust "cotrimoxazol" w
        |> evaluate ["sulfamethoxazol"; "trimethoprim"]
    

    // Drug with multiple items
    // cotrimoxazol drink for infection
    let cotrimoxazolIV weight doses =
        let w = weight |> toBigRational

        {
            DrugOrder.drugOrder with
                Id = "1"
                Name = "cotrimoxazol"
                Divisible = 2N
                Products = 
                    [
                        { 
                            DrugOrder.productComponent with 
                                Name = "cotrimoxazol"
                                Quantities = [ 5N ]
                                TimeUnit = "day"
                                Substances =
                                    [
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "sulfamethoxazol"
                                                Concentrations = 
                                                    [ 80N ]
                                                Unit = "mg"
                                                DoseUnit = "mg"
                                                TimeUnit = "day"
                                        }
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "trimethoprim"
                                                Concentrations = 
                                                    [ 16N ]
                                                Unit = "mg"
                                                DoseUnit = "mg"
                                                TimeUnit = "day"
                                        }
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "proplyleenglycol"
                                                Concentrations = 
                                                    [ 410N ]
                                                Unit = "mg"
                                                DoseUnit = "mg"
                                                TimeUnit = "day"
                                        }
                                    ]
                        }
                    ]
                Unit = "ml"
                TimeUnit = "day"
                Shape = "infusion fluid"
                Route = "iv"
                OrderType = DiscontinuousOrder
        }
        |> DrugOrder.create
        // setting dose limits for infection
        |> setDoses doses
        |> DrugOrder.setAdjust "cotrimoxazol" w
        |> evaluate ["sulfamethoxazol"; "trimethoprim"]


    // Drug with multiple items
    // cotrimoxazol drink for infection
    let cotrimoxazolRokiIV weight doses =
        let w = weight |> toBigRational

        {
            DrugOrder.drugOrder with
                Id = "1"
                Name = "cotrimoxazol (Rokiprim)"
                Divisible = 2N
                Products = 
                    [
                        { 
                            DrugOrder.productComponent with 
                                Name = "cotrimoxazol"
                                Quantities = [ 5N ]
                                TimeUnit = "day"
                                Substances =
                                    [
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "sulfamethoxazol"
                                                Concentrations = 
                                                    [ 80N ]
                                                Unit = "mg"
                                                DoseUnit = "mg"
                                                TimeUnit = "day"
                                        }
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "trimethoprim"
                                                Concentrations = 
                                                    [ 16N ]
                                                Unit = "mg"
                                                DoseUnit = "mg"
                                                TimeUnit = "day"
                                        }
                                    ]
                        }
                    ]
                Unit = "ml"
                TimeUnit = "day"
                Shape = "infusion fluid"
                Route = "iv"
                OrderType = DiscontinuousOrder
        }
        |> DrugOrder.create
        // setting dose limits for infection
        |> setDoses doses
        |> DrugOrder.setAdjust "cotrimoxazol" w
        |> evaluate ["sulfamethoxazol"; "trimethoprim"]

    // Dopamin infusion calculate scenario's 
    // with a number of standard solutions
    let dopaminStandardConcentrations weight doses =
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
        |> setDoses doses
        |> evaluate ["dopamin"]

    // Dopamin infusion calculate scenario's 
    // with a a fixed infusion - dose rate
    let dopaminFixedRate weight doses =
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
        |> setDoses doses
        |> DrugOrder.setAdjust "dopamin infusion" w
        |> evaluate ["dopamin"]

    // gentamicin
    let gentamicinIV weight doses =
        let w = weight |> toBigRational

        {
            DrugOrder.drugOrder with
                Id = "1"
                Name = "gentamicin"
                Quantities = [ 1N; 2N; 5N; 10N; 50N; 100N ]
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
        |> DrugOrder.setAdjust "gentamicin" w
        |> setDoses doses
        |> DrugOrder.setSolutionLimits 
            {
                DrugOrder.solutionLimits with
                    Name = "gentamicin"
                    Component = "gentamicin"
                    MinConcentration = Some 1N
                    MaxConcentration = Some 2N
                    DoseCount = Some (1N)
                    MinTime = Some (1N/3N)
                    MaxTime = Some (1N/2N)

            }
        |> evaluate ["gentamicin"]


module Demo =

    open System
    open MathNet.Numerics

    open Informedica.Utils.Lib.BCL
    open Informedica.GenUnits.Lib
    open Informedica.GenOrder.Lib

    open Types

    module Units = ValueUnit.Units
    module DrugConstraint = DrugOrder.DrugConstraint
    module Quantity = VariableUnit.Quantity

    open OrderLogger

    let indications =
        [
            "chronische pijn", "paracetamol", "oraal",[
                {   DrugOrder.doseLimits with
                        Name = "paracetamol"
                        Frequencies = [ 3N; 4N ]            // Allowed frequencies are 3 or 4 per day
                        SubstanceName = "paracetamol"
                        MaxDoseQuantity = Some 1000N        // Max per single dose = 1000 mg
                        MaxDoseTotal = Some 3000N           // Max daily dose = 3000 mg/day
                        MinDoseTotalAdjust = Some 50N       // Min adjusted dose = 50 mg/kg/day
                        MaxDoseTotalAdjust = Some 70N       // Max adjusted daily dose = 70 mg/kg/day
                }
            ], [ "paracetamol" ]

            "chronische pijn", "paracetamol", "rectaal", [
                {   DrugOrder.doseLimits with
                        Name = "paracetamol"
                        Frequencies = [ 3N; 4N ]            // Allowed frequencies are 3 or 4 per day
                        SubstanceName = "paracetamol"
                        MaxDoseQuantity = Some 1000N        // Max per single dose = 1000 mg
                        MaxDoseTotal = Some 3000N           // Max daily dose = 3000 mg/day
                        MinDoseTotalAdjust = Some 50N       // Min adjusted dose = 50 mg/kg/day
                        MaxDoseTotalAdjust = Some 70N       // Max adjusted daily dose = 70 mg/kg/day
                }
            ], [ "paracetamol" ]

            "acute pijn/post operatief", "paracetamol", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "paracetamol"
                        Frequencies = [ 4N ]            // Allowed frequencies are 3 or 4 per day
                        SubstanceName = "paracetamol"
                        MaxDoseQuantity = Some 1000N        // Max per single dose = 1000 mg
                        MaxDoseTotal = Some 4000N           // Max daily dose = 3000 mg/day
                        MinDoseTotalAdjust = Some 50N       // Min adjusted dose = 50 mg/kg/day
                        MaxDoseTotalAdjust = Some 60N       // Max adjusted daily dose = 70 mg/kg/day
                }
            ], [ "paracetamol" ]

            "acute pijn/post operatief", "paracetamol", "oraal", [
                {   DrugOrder.doseLimits with
                        Name = "paracetamol"
                        Frequencies = [ 4N ]            // Allowed frequencies are 3 or 4 per day
                        SubstanceName = "paracetamol"
                        MaxDoseQuantity = Some 1000N        // Max per single dose = 1000 mg
                        MaxDoseTotal = Some 4000N           // Max daily dose = 3000 mg/day
                        MinDoseTotalAdjust = Some 70N       // Min adjusted dose = 50 mg/kg/day
                        MaxDoseTotalAdjust = Some 90N       // Max adjusted daily dose = 70 mg/kg/day
                }
            ], [ "paracetamol" ]

            "acute pijn/post operatief", "paracetamol", "rectaal", [
                {   DrugOrder.doseLimits with
                        Name = "paracetamol"
                        Frequencies = [ 3N ]            // Allowed frequencies are 3 or 4 per day
                        SubstanceName = "paracetamol"
                        MaxDoseQuantity = Some 1000N        // Max per single dose = 1000 mg
                        MaxDoseTotal = Some 3000N           // Max daily dose = 3000 mg/day
                        MinDoseTotalAdjust = Some 70N       // Min adjusted dose = 50 mg/kg/day
                        MaxDoseTotalAdjust = Some 90N       // Max adjusted daily dose = 70 mg/kg/day
                }
            ], [ "paracetamol" ]

            "bloeddruk verhoging", "dopamin", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "dopamin infusion"
                        Rates = [ 1N ]
                        SubstanceName = "dopamin"
                        MinDoseRateAdjust = Some 2N
                        MaxDoseRateAdjust = Some 20N
                }
            ], [ "dopamin" ]

            "bloeddruk verhoging", "dopamin", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "dopamin infusion"
                        Rates = [ 1N ]
                        SubstanceName = "dopamin"
                        MinDoseRateAdjust = Some 2N
                        MaxDoseRateAdjust = Some 20N
                }
            ], ["sulfamethoxazol"; "trimethoprim"]

            "infecties", "cotrimoxazol", "oraal", [
                {   DrugOrder.doseLimits with
                        Name = "cotrimoxazol"
                        Frequencies = [ 2N ]
                        SubstanceName = "sulfamethoxazol"
                        MaxDoseTotal = Some 1600N
                        MinDoseTotalAdjust = Some 25N
                        MaxDoseTotalAdjust = Some 35N
                }
            ], ["sulfamethoxazol"; "trimethoprim"]

            "infecties", "cotrimoxazol", "oraal", [
                {   DrugOrder.doseLimits with
                        Name = "cotrimoxazol"
                        Frequencies = [ 2N ]
                        SubstanceName = "sulfamethoxazol"
                        MaxDoseTotal = Some 1600N
                        MinDoseTotalAdjust = Some 25N
                        MaxDoseTotalAdjust = Some 35N
                }
            ], ["sulfamethoxazol"; "trimethoprim"]

            "infecties < 5 jaar", "cotrimoxazol", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "cotrimoxazol"
                        Frequencies = [ 2N ]
                        SubstanceName = "sulfamethoxazol"
                        MaxDoseTotal = Some 1600N
                        MinDoseTotalAdjust = Some 25N
                        MaxDoseTotalAdjust = Some 35N
                }

                { DrugOrder.doseLimits with
                    Name = "cotrimoxazol"
                    SubstanceName = "proplyleenglycol"
                    MaxDoseTotalAdjust = Some 50N
                }
            ], ["sulfamethoxazol"; "trimethoprim"]

            "infecties < 5 jaar", "cotrimoxazol", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "cotrimoxazol"
                        Frequencies = [ 2N ]
                        SubstanceName = "sulfamethoxazol"
                        MaxDoseTotal = Some 1600N
                        MinDoseTotalAdjust = Some 25N
                        MaxDoseTotalAdjust = Some 35N
                }
            ], ["sulfamethoxazol"; "trimethoprim"]

            "infecties >= 5 jaar", "cotrimoxazol", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "cotrimoxazol"
                        Frequencies = [ 2N ]
                        SubstanceName = "sulfamethoxazol"
                        MaxDoseTotal = Some 1600N
                        MinDoseTotalAdjust = Some 25N
                        MaxDoseTotalAdjust = Some 35N
                }
            ], ["sulfamethoxazol"; "trimethoprim"]

            "behandeling PJP", "cotrimoxazol", "oraal", [
                {   DrugOrder.doseLimits with
                        Name = "cotrimoxazol"
                        Frequencies = [ 3N; 4N ]
                        SubstanceName = "sulfamethoxazol"
                        MaxDoseTotal = Some 4800N
                        MinDoseTotalAdjust = Some 90N
                        MaxDoseTotalAdjust = Some 100N
                }
            ], ["sulfamethoxazol"; "trimethoprim"]

            "behandeling PJP", "cotrimoxazol", "oraal", [
                {   DrugOrder.doseLimits with
                        Name = "cotrimoxazol"
                        Frequencies = [ 3N; 4N ]
                        SubstanceName = "sulfamethoxazol"
                        MaxDoseTotal = Some 4800N
                        MinDoseTotalAdjust = Some 90N
                        MaxDoseTotalAdjust = Some 100N
                }
            ], ["sulfamethoxazol"; "trimethoprim"]

            "behandeling PJP < 5 jaar", "cotrimoxazol", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "cotrimoxazol"
                        Frequencies = [ 3N; 4N ]
                        SubstanceName = "sulfamethoxazol"
                        MaxDoseTotal = Some 4800N
                        MinDoseTotalAdjust = Some 90N
                        MaxDoseTotalAdjust = Some 100N
                }

                { DrugOrder.doseLimits with
                    Name = "cotrimoxazol"
                    SubstanceName = "proplyleenglycol"
                    MaxDoseTotalAdjust = Some 50N
                }

            ], ["sulfamethoxazol"; "trimethoprim"]

            "behandeling PJP > 5 jaar", "cotrimoxazol", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "cotrimoxazol"
                        Frequencies = [ 3N; 4N ]
                        SubstanceName = "sulfamethoxazol"
                        MaxDoseTotal = Some 4800N
                        MinDoseTotalAdjust = Some 90N
                        MaxDoseTotalAdjust = Some 100N
                }
            ], ["sulfamethoxazol"; "trimethoprim"]

            "behandeling PJP < 5 jaar", "cotrimoxazol", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "cotrimoxazol"
                        Frequencies = [ 3N; 4N ]
                        SubstanceName = "sulfamethoxazol"
                        MaxDoseTotal = Some 4800N
                        MinDoseTotalAdjust = Some 90N
                        MaxDoseTotalAdjust = Some 100N
                }
            ], ["sulfamethoxazol"; "trimethoprim"]

            "profylaxe", "cotrimoxazol", "oraal", [
                {   DrugOrder.doseLimits with
                        Name = "cotrimoxazol"
                        Frequencies = [ 1N ]
                        SubstanceName = "sulfamethoxazol"
                        MaxDoseTotal = Some 800N
                        MinDoseTotalAdjust = Some 15N
                        MaxDoseTotalAdjust = Some 25N
                }
            ], ["sulfamethoxazol"; "trimethoprim"]

            "profylaxe", "cotrimoxazol", "oraal", [
                {   DrugOrder.doseLimits with
                        Name = "cotrimoxazol"
                        Frequencies = [ 1N ]
                        SubstanceName = "sulfamethoxazol"
                        MaxDoseTotal = Some 800N
                        MinDoseTotalAdjust = Some 15N
                        MaxDoseTotalAdjust = Some 25N
                }
            ], ["sulfamethoxazol"; "trimethoprim"]

            "ernstige infecties", "gentamicin", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "gentamicin"
                        Frequencies = [ 1N ]
                        SubstanceName = "gentamicin"
                        MinDoseTotalAdjust = Some (65N/10N)
                        MaxDoseTotalAdjust = Some (75N/10N)
                }
            ], [ "gentamicin" ]

        ]


    let getIndications () =
        indications
        |> List.map (fun (i, _, _, _, _) -> i)
        |> List.distinct


    let getMedications () =
        indications
        |> List.map (fun (_, m, _, _, _) -> m)
        |> List.distinct


    let getRoutes () =
        indications
        |> List.map (fun (_, _, r, _, _) -> r)
        |> List.distinct


    let filterIndications ind med route =
        indications
        |> List.filter (fun (i, m, r, _, _) ->
            ind = i &&
            (med |> Option.isNone || med = Some m) &&
            (route |> Option.isNone || route = Some r)
        )


    let mapRoute = function
        | s when s = "oraal" -> "or"
        | s when s = "intraveneus" -> "iv"
        | s when s = "rectaal" -> "rect"
        | _ -> ""


    let create w ind med route =
        filterIndications ind med route
        |> List.collect (fun (_, m, r, d, ns) ->
            try
                r
                |> mapRoute
                |> Examples.getOrders m 
                |> List.collect (Examples.calculate w ns d)
                |> List.map (fun sc -> { sc with Route = r })
            with
            | _ -> []
        )
        |> List.distinct
        |> List.mapi (fun i sc -> { sc with No = i + 1 })