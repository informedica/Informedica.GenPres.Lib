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
    module Name = Informedica.GenSolver.Lib.Variable.Name

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
                    Name = o.Orderable.Name |> Name.toString
                    Shape = o.Orderable.Shape
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
            // paracetamol drank
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
                    Shape = "drank"
                    Route = "or"
                    OrderType = DiscontinuousOrder
            }
            // paracetamol zetpil
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
                    Shape = "zetpil"
                    Route = "rect"
                    OrderType = DiscontinuousOrder
            }
            // paracetamol infuusvloeistof
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
                    Shape = "infusievloeistof"
                    Route = "iv"
                    OrderType = DiscontinuousOrder
            }
            // cotrimoxazol tablet
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
            // contrimoxazol drank
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
                    Shape = "drank"
                    Route = "or"
                    OrderType = DiscontinuousOrder
            }
            // contrimoxazol infuusvloeistof
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
                    Shape = "infusievloeistof"
                    Route = "iv"
                    OrderType = DiscontinuousOrder
            }
            // cotrimoxazol Rokiprim infuusvloeistof
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
                    Shape = "infusievloeistof"
                    Route = "iv"
                    OrderType = DiscontinuousOrder
            }
            // morfine standaard concentraties
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "morfine pomp"
                    Quantities = [ 50N ]
                    Divisible = 1N
                    Unit = "ml"
                    TimeUnit = "day"
                    Shape = "infusievloeistof"
                    Route = "iv"
                    Products = 
                        [
                            { 
                                DrugOrder.productComponent with
                                    Name = "morfine"
                                    Quantities = [ 1N; 5N ]
                                    TimeUnit = "day"
                                    Substances = 
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "morfine"
                                                    Concentrations = [ 1N; 10N; ]
                                                    OrderableQuantities = [ 2N; 5N; 10N ]
                                                    Unit = "mg"
                                                    DoseUnit = "mcg"
                                                    TimeUnit = "hour"
                                            }
                                        ]

                            }
                            { 
                                DrugOrder.productComponent with
                                    Name = "NaCl 0,9%"
                                    Quantities = [ 5000N ]
                                    TimeUnit = "day"
                                    Substances = 
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "natrium"
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
            // dopamine standaard concentraties
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "dopamine pomp"
                    Quantities = [ 50N ]
                    Divisible = 1N
                    Unit = "ml"
                    TimeUnit = "day"
                    Shape = "infusievloeistof"
                    Route = "iv"
                    Products = 
                        [
                            { 
                                DrugOrder.productComponent with
                                    Name = "dopamine"
                                    Quantities = [ 5N ]
                                    TimeUnit = "day"
                                    Substances = 
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "dopamine"
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
                                    Name = "NaCl 0,9%"
                                    Quantities = [ 5000N ]
                                    TimeUnit = "day"
                                    Substances = 
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "natrium"
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
            // dopamine standaard loopsnelheid
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "dopamine pomp"
                    Quantities = [ 50N ]
                    Divisible = 1N
                    Unit = "ml"
                    TimeUnit = "day"
                    Shape = "infusievloeistof"
                    Route = "iv"
                    Products = 
                        [
                            { 
                                DrugOrder.productComponent with
                                    Name = "dopamine"
                                    Quantities = [ 5N ]
                                    TimeUnit = "day"
                                    Substances = 
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "dopamine"
                                                    Concentrations = [ 40N ]
                                                    Unit = "mg"
                                                    DoseUnit = "mcg"
                                                    TimeUnit = "min"
                                            }
                                        ]

                            }
                            { 
                                DrugOrder.productComponent with
                                    Name = "NaCl 0,9%"
                                    Quantities = [ 5000N ]
                                    TimeUnit = "day"
                                    Substances = 
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "natrium"
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
            // noradrenaline standaard loopsnelheid
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "noradrenaline pomp"
                    Quantities = [ 50N ]
                    Divisible = 1N
                    Unit = "ml"
                    TimeUnit = "day"
                    Shape = "infusievloeistof"
                    Route = "iv"
                    Products = 
                        [
                            { 
                                DrugOrder.productComponent with
                                    Name = "noradrenaline"
                                    Quantities = [ 1N; 5N ]
                                    TimeUnit = "day"
                                    Substances = 
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "noradrenaline"
                                                    Concentrations = [ 1N ]
                                                    OrderableQuantities = [1N; 2N; 5N]
                                                    Unit = "mg"
                                                    DoseUnit = "mcg"
                                                    TimeUnit = "min"
                                            }
                                        ]

                            }
                            { 
                                DrugOrder.productComponent with
                                    Name = "NaCl 0,9%"
                                    Quantities = [ 5000N ]
                                    TimeUnit = "day"
                                    Substances = 
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "natrium"
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

            // gentamicine infuusvloeistof
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "gentamicine"
                    Quantities = [ 1N; 2N; 5N; 10N; 50N; 100N ]
                    Divisible = 1N 
                    Unit = "ml"
                    TimeUnit = "day"
                    Shape = "infusievloeistof"
                    Route = "iv"
                    Products = 
                        [
                            { 
                                DrugOrder.productComponent with
                                    Name = "gentamicine"
                                    Quantities = [ 2N; 10N ]
                                    TimeUnit = "day"
                                    Substances = 
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "gentamicine"
                                                    Concentrations = [ 10N; 40N ]
                                                    Unit = "mg"
                                                    DoseUnit = "mg"
                                                    TimeUnit = "day"
                                            }
                                        ]

                            }
                            { 
                                DrugOrder.productComponent with
                                    Name = "NaCl 0,9%"
                                    Quantities = [ 5000N ]
                                    TimeUnit = "day"
                                    Substances = 
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "natrium"
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

            // ceftriaxon infuusvloeistof
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "ceftriaxon"
                    Unit = "ml"
                    TimeUnit = "day"
                    Shape = "infusievloeistof"
                    Route = "iv"
                    Products = 
                        [
                            { 
                                DrugOrder.productComponent with
                                    Name = "ceftriaxon"
                                    Quantities = [ 40N ]
                                    TimeUnit = "day"
                                    Substances = 
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "ceftriaxon"
                                                    Concentrations = [ 50N ]
                                                    Unit = "mg"
                                                    DoseUnit = "mg"
                                                    TimeUnit = "day"
                                            }
                                        ]

                            }

                        ]
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
        |> List.filter (fun o ->
            (o.Name = n  ||
            o.Products
            |> List.exists (fun p -> p.Name = n)) && o.Route = r
        )


    // Paracetamol drank
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
                Shape = "drank"
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

        // Paracetamol zetpil example
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
                Shape = "zetpil"
                Route = "rect"
                OrderType = DiscontinuousOrder
        }
        |> DrugOrder.create 
        |> DrugOrder.setAdjust "paracetamol" w          // Now calculate the scenarios for 10 kg
        |> setDoses doses
        |> evaluate ["paracetamol"]


    let paracetamolIV weight doses =
        let w = weight |> toBigRational

        // Paracetamol zetpil example
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
                Shape = "infusievloeistof"
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
    // cotrimoxazol drank for infection
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
                Shape = "drank"
                Route = "or"
                OrderType = DiscontinuousOrder
        }
        |> DrugOrder.create
        |> setDoses doses
        |> DrugOrder.setAdjust "cotrimoxazol" w
        |> evaluate ["sulfamethoxazol"; "trimethoprim"]
    

    // Drug with multiple items
    // cotrimoxazol drank for infection
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
                Shape = "infusievloeistof"
                Route = "iv"
                OrderType = DiscontinuousOrder
        }
        |> DrugOrder.create
        // setting dose limits for infection
        |> setDoses doses
        |> DrugOrder.setAdjust "cotrimoxazol" w
        |> evaluate ["sulfamethoxazol"; "trimethoprim"]


    // Drug with multiple items
    // cotrimoxazol drank for infection
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
                Shape = "infusievloeistof"
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
                Name = "dopamine infusion"
                Quantities = [ 50N ]
                Divisible = 1N
                Unit = "ml"
                TimeUnit = "day"
                Shape = "infusievloeistof"
                Route = "iv"
                Products = 
                    [
                        { 
                            DrugOrder.productComponent with
                                Name = "dopamine"
                                Quantities = [ 5N ]
                                TimeUnit = "day"
                                Substances = 
                                    [
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "dopamine"
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
                                Name = "NaCl 0,9%"
                                Quantities = [ 5000N ]
                                TimeUnit = "day"
                                Substances = 
                                    [
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "natrium"
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
        |> DrugOrder.setAdjust "dopamine infusion" w
        |> setDoses doses
        |> evaluate ["dopamine"]

    // Dopamin infusion calculate scenario's 
    // with a a fixed infusion - dose rate
    let dopaminFixedRate weight doses =
        let w = weight |> toBigRational

        {
            DrugOrder.drugOrder with
                Id = "1"
                Name = "dopamine pomp"
                Quantities = [ 50N ]
                Divisible = 1N
                Unit = "ml"
                TimeUnit = "day"
                Shape = "infusievloeistof"
                Route = "iv"
                Products = 
                    [
                        { 
                            DrugOrder.productComponent with
                                Name = "dopamine"
                                Quantities = [ 5N ]
                                TimeUnit = "day"
                                Substances = 
                                    [
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "dopamine"
                                                Concentrations = [ 40N ]
                                                Unit = "mg"
                                                DoseUnit = "mcg"
                                                TimeUnit = "min"
                                        }
                                    ]

                        }
                        { 
                            DrugOrder.productComponent with
                                Name = "NaCl 0,9%"
                                Quantities = [ 5000N ]
                                TimeUnit = "day"
                                Substances = 
                                    [
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "natrium"
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
        |> DrugOrder.setAdjust "dopamine infusion" w
        |> evaluate ["dopamine"]

    // gentamicin
    let gentamicinIV weight doses =
        let w = weight |> toBigRational

        {
            DrugOrder.drugOrder with
                Id = "1"
                Name = "gentamicine"
                Quantities = [ 1N; 2N; 5N; 10N; 50N; 100N ]
                Divisible = 1N 
                Unit = "ml"
                TimeUnit = "day"
                Shape = "infusievloeistof"
                Route = "iv"
                Products = 
                    [
                        { 
                            DrugOrder.productComponent with
                                Name = "gentamicine"
                                Quantities = [ 2N; 10N ]
                                TimeUnit = "day"
                                Substances = 
                                    [
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "gentamicine"
                                                Concentrations = [ 10N; 40N ]
                                                Unit = "mg"
                                                DoseUnit = "mg"
                                                TimeUnit = "day"
                                        }
                                    ]

                        }
                        { 
                            DrugOrder.productComponent with
                                Name = "NaCl 0,9%"
                                Quantities = [ 5000N ]
                                TimeUnit = "day"
                                Substances = 
                                    [
                                        {
                                            DrugOrder.substanceItem with
                                                Name = "natrium"
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
                    Name = "gentamicine"
                    Component = "gentamicine"
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
            // chronische pijn
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
            // acute pijn
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

            "acute pijn/post operatief", "morfine", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "morfine pomp"
                        SubstanceName = "morfine"
                        Rates = [1N]
                        MinDoseRateAdjust = Some 10N
                        MaxDoseRateAdjust = Some 40N
                }
            ], [ "morfine" ]
            // bloeddruk verhoging
            "bloeddruk verhoging", "dopamine", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "dopamine pomp"
                        Rates = [ 1N ]
                        SubstanceName = "dopamine"
                        MinDoseRateAdjust = Some 2N
                        MaxDoseRateAdjust = Some 20N
                }
            ], [ "dopamine" ]

            "bloeddruk verhoging", "noradrenaline", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "noradrenaline pomp"
                        Rates = [ 1N ]
                        SubstanceName = "noradrenaline"
                        MinDoseRateAdjust = Some (5N/100N)
                        MaxDoseRateAdjust = Some (5N/10N)
                }
            ], ["noradrenaline"]
            // infecties
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
            // behandeling PJP
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
            // profylaxe
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
            // ernstige infecties
            "ernstige infecties", "gentamicine", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "gentamicine"
                        Frequencies = [ 1N ]
                        SubstanceName = "gentamicine"
                        MinDoseTotalAdjust = Some (65N/10N)
                        MaxDoseTotalAdjust = Some (75N/10N)
                }
            ], [ "gentamicine" ]

            "ernstige infecties", "ceftriaxon", "intraveneus", [
                {   DrugOrder.doseLimits with
                        Name = "ceftriaxon"
                        Frequencies = [ 1N ]
                        SubstanceName = "ceftriaxon"
                        MinDoseTotalAdjust = Some (90N)
                        MaxDoseTotalAdjust = Some (100N)
                }
            ], [ "ceftriaxon" ]

        ]


    let filter ind med route =
        indications
        |> List.filter (fun (i, m, r, _, _) ->
            (ind |> Option.isNone || ind = Some i) &&
            (med |> Option.isNone || med = Some m) &&
            (route |> Option.isNone || route = Some r)
        )


    let filterIndications med route =
        filter None med route
        |> List.map (fun (i, _, _, _, _) -> i)
        |> List.distinct


    let filterMedications ind route =
        filter ind None route
        |> List.map (fun (_, m, _, _, _) -> m)
        |> List.distinct


    let filterRoutes ind med =
        filter ind med None
        |> List.map (fun (_, _, r, _, _) -> r)
        |> List.distinct


    let mapRoute = function
        | s when s = "oraal" -> "or"
        | s when s = "intraveneus" -> "iv"
        | s when s = "rectaal" -> "rect"
        | _ -> ""


    let translate sc : Scenario =
        let trans s =
            s
            |> String.replace "day" "dag"
            |> String.replace "hour" "uur"
            |> String.replace "piece" "stuk"
            
        { sc with
            Prescription = sc.Prescription |> trans
            Preparation =sc.Preparation |> trans
            Administration =sc.Administration |> trans
        }


    let toString (sc : Scenario) =
        $"""
{sc.No}. {sc.Name} {sc.Shape} {sc.Route}
Voorschrift: {sc.Prescription}
Bereiding: {sc.Preparation}
Toediening: {sc.Administration}
"""


    let create w ind med route =
        filter ind med route
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
        // correct for roundig problem. admin should not be rounded!
        |> List.distinctBy (fun sc -> sc.Route, sc.Administration)
        |> List.map translate
        |> List.mapi (fun i sc -> { sc with No = i + 1 })
        |> List.map toString