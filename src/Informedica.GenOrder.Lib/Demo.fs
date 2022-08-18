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

    let setSolutions sols o =
        sols
        |> List.fold (fun acc sol ->
            acc
            |> DrugOrder.setSolutionLimits sol
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
            // ondansetron infuusvloeistof
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "ondansetron"
                    Products = 
                        [
                            { 
                                DrugOrder.productComponent with 
                                    Name = "ondansetron"
                                    Quantities = [ 2N; 4N ]
                                    TimeUnit = "day"
                                    Substances =
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "ondansetron"
                                                    Concentrations = 
                                                        [ 2N ]
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
            // ondansetron tablet
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "ondansetron"
                    Products = 
                        [
                            { 
                                DrugOrder.productComponent with 
                                    Name = "ondansetron"
                                    Quantities = [ 1N ]
                                    TimeUnit = "day"
                                    Substances =
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "ondansetron"
                                                    Concentrations = 
                                                        [ 2N; 4N ]
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
            // ondansetron drank
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "ondansetron"
                    Products = 
                        [
                            { 
                                DrugOrder.productComponent with 
                                    Name = "ondansetron"
                                    Quantities = [ 50N ]
                                    TimeUnit = "day"
                                    Substances =
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "ondansetron"
                                                    Concentrations = 
                                                        [ 8N / 10N ]
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
                                    Substances = []

                            }
                        ]
                    OrderType = ContinuousOrder
            }
            // midazolam standaard concentraties
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "midazolam pomp"
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
                                    Name = "midazolam"
                                    Quantities = [ 5N; 10N; 50N ]
                                    TimeUnit = "day"
                                    Substances = 
                                        [
                                            {
                                                DrugOrder.substanceItem with
                                                    Name = "midazolam"
                                                    Concentrations = [ 1N; 2N; 5N ]
                                                    OrderableQuantities = [ 25N; 50N; 100N; 250N ]
                                                    Unit = "mg"
                                                    DoseUnit = "mg"
                                                    TimeUnit = "hour"
                                            }
                                        ]

                            }
                            { 
                                DrugOrder.productComponent with
                                    Name = "NaCl 0,9%"
                                    Quantities = [ 5000N ]
                                    TimeUnit = "day"
                                    Substances = []

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
                                    Substances = []

                            }
                        ]
                    OrderType = ContinuousOrder
            }
            // noradrenaline standaard concentraties
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
                                    Substances = []

                            }
                        ]
                    OrderType = ContinuousOrder
            }
            // gentamicine infuusvloeistof
            {
                DrugOrder.drugOrder with
                    Id = "1"
                    Name = "gentamicine"
                    Quantities = [ 5N; 10N; 20N; 50N; 100N ]
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
                                            //{
                                            //    DrugOrder.substanceItem with
                                            //        Name = "natrium"
                                            //        Concentrations = [ 155N / 1000N ]
                                            //        Unit = "mmol"
                                            //        DoseUnit = "mmol"
                                            //        TimeUnit = "day"
                                            //}
                                            //{
                                            //    DrugOrder.substanceItem with
                                            //        Name = "chloride"
                                            //        Concentrations = [ 155N / 1000N ]
                                            //        Unit = "mmol"
                                            //        DoseUnit = "mmol"
                                            //        TimeUnit = "day"
                                            //}
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

    /// calculate the scenarios for a drug order
    /// o with substancenames ns and doses doses
    let calculate weight ns doses sols o =
        let w = weight |> toBigRational

        o
        |> DrugOrder.create
        |> DrugOrder.setAdjust o.Name w
        |> setDoses doses
        |> setSolutions sols
        |> evaluate ns



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


    let inline isBetween (min, max) x =
        match x with
        | None -> true
        | Some x ->
            match min, max with
            | None,     None     -> true
            | Some min, Some max -> x >= min && x <= max
            | Some min, None     -> x >= min
            | None,     Some max -> x <= max


    type Dose =
        {
            Indication : string
            Age : int option * int option
            Weight : float option * float option
            GestAge : int option * int option
            Medication : string
            Route : string
            Substances : string list
            Limits : DoseLimits list
        }


    let emptyDose =
        {
            Indication = ""
            Age = None, None
            Weight = None, None
            GestAge = None, None
            Medication = ""
            Route = ""
            Substances = []
            Limits = []
        }


    type Solution =
        {
            Medication : string
            Weight : float option * float option
            Solution : SolutionLimits
        }


    let solutions =
        [
            {
                Medication = "gentamicine"
                Weight = None, None
                Solution =
                    { DrugOrder.solutionLimits with
                        Name = "gentamicine"
                        MinConcentration = Some (2N - 1N/4N)
                        MaxConcentration = Some (2N)
                        MinTime = Some (1N - 1N/5N)
                        MaxTime = Some (1N)
                        DoseCount = [] //[1N]
                    }
            }
        ]


    let doses =
        [
            // chronische pijn
            { emptyDose with
                Indication = "chronische pijn"
                Medication = "paracetamol"
                Route = "oraal"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "paracetamol"
                            Frequencies = [ 3N; 4N ]            // Allowed frequencies are 3 or 4 per day
                            SubstanceName = "paracetamol"
                            MaxDoseQuantity = Some 1000N        // Max per single dose = 1000 mg
                            MaxDoseTotal = Some 3000N           // Max daily dose = 3000 mg/day
                            MinDoseTotalAdjust = Some 50N       // Min adjusted dose = 50 mg/kg/day
                            MaxDoseTotalAdjust = Some 70N       // Max adjusted daily dose = 70 mg/kg/day
                    }
                ]
                Substances = [ "paracetamol" ]
            }

            { emptyDose with
                Indication = "chronische pijn"
                Medication = "paracetamol"
                Route = "rectaal"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "paracetamol"
                            Frequencies = [ 3N; 4N ]            // Allowed frequencies are 3 or 4 per day
                            SubstanceName = "paracetamol"
                            MaxDoseQuantity = Some 1000N        // Max per single dose = 1000 mg
                            MaxDoseTotal = Some 3000N           // Max daily dose = 3000 mg/day
                            MinDoseTotalAdjust = Some 50N       // Min adjusted dose = 50 mg/kg/day
                            MaxDoseTotalAdjust = Some 70N       // Max adjusted daily dose = 70 mg/kg/day
                    }
                ]
                Substances = [ "paracetamol" ]

            }
            // acute pijn
            { emptyDose with
                Indication = "acute pijn/post operatief"
                Medication = "paracetamol"
                Route = "intraveneus"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "paracetamol"
                            Frequencies = [ 4N ]            // Allowed frequencies are 3 or 4 per day
                            SubstanceName = "paracetamol"
                            MaxDoseQuantity = Some 1000N        // Max per single dose = 1000 mg
                            MaxDoseTotal = Some 4000N           // Max daily dose = 3000 mg/day
                            MinDoseTotalAdjust = Some 50N       // Min adjusted dose = 50 mg/kg/day
                            MaxDoseTotalAdjust = Some 60N       // Max adjusted daily dose = 70 mg/kg/day
                    }
                ]
                Substances = [ "paracetamol" ]
            }

            { emptyDose with
                Indication = "acute pijn/post operatief"
                Medication = "paracetamol"
                Route = "oraal"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "paracetamol"
                            Frequencies = [ 4N ]            // Allowed frequencies are 3 or 4 per day
                            SubstanceName = "paracetamol"
                            MaxDoseQuantity = Some 1000N        // Max per single dose = 1000 mg
                            MaxDoseTotal = Some 4000N           // Max daily dose = 3000 mg/day
                            MinDoseTotalAdjust = Some 70N       // Min adjusted dose = 50 mg/kg/day
                            MaxDoseTotalAdjust = Some 90N       // Max adjusted daily dose = 70 mg/kg/day
                    }
                ]
                Substances = [ "paracetamol" ]
            }

            { emptyDose with
                Indication = "acute pijn/post operatief"
                Medication = "paracetamol"
                Route = "rectaal"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "paracetamol"
                            Frequencies = [ 3N ]            // Allowed frequencies are 3 or 4 per day
                            SubstanceName = "paracetamol"
                            MaxDoseQuantity = Some 1000N        // Max per single dose = 1000 mg
                            MaxDoseTotal = Some 3000N           // Max daily dose = 3000 mg/day
                            MinDoseTotalAdjust = Some 70N       // Min adjusted dose = 50 mg/kg/day
                            MaxDoseTotalAdjust = Some 90N       // Max adjusted daily dose = 70 mg/kg/day
                    }
                ]
                Substances = [ "paracetamol" ]
            }

            { emptyDose with
                Indication = "acute pijn/post operatief"
                Medication = "morfine"
                Route = "intraveneus"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "morfine pomp"
                            SubstanceName = "morfine"
                            Rates = [1N]
                            MinDoseRateAdjust = Some 10N
                            MaxDoseRateAdjust = Some 40N
                    }
                ]
                Substances = [ "morfine" ]
            }

            { emptyDose with
                Indication = "sedatie"
                Medication = "midazolam"
                Route = "intraveneus"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "midazolam pomp"
                            SubstanceName = "midazolam"
                            Rates = [ 1N ]
                            MinDoseRateAdjust = Some (5N / 100N)
                            MaxDoseRateAdjust = Some (1N / 2N)
                            MaxDoseRate = Some 1N
                    }
                ]
                Substances = [ "midazolam" ]
            }

            { emptyDose with
                // bloeddruk verhoging
                Indication = "bloeddruk verhoging"
                Medication = "dopamine"
                Route = "intraveneus"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "dopamine pomp"
                            Rates = [ 1N ]
                            SubstanceName = "dopamine"
                            MinDoseRateAdjust = Some 2N
                            MaxDoseRateAdjust = Some 20N
                    }
                ]
                Substances = [ "dopamine" ]
            }

            { emptyDose with
                Indication = "bloeddruk verhoging"
                Medication = "noradrenaline"
                Route = "intraveneus"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "noradrenaline pomp"
                            Rates = [ 1N ]
                            SubstanceName = "noradrenaline"
                            MinDoseRateAdjust = Some (5N/100N)
                            MaxDoseRateAdjust = Some (5N/10N)
                    }
                ]
                Substances = ["noradrenaline"]
            }
            // infecties
            { emptyDose with
                Indication = "infecties"
                Medication = "cotrimoxazol"
                Route = "oraal"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "cotrimoxazol"
                            Frequencies = [ 2N ]
                            SubstanceName = "sulfamethoxazol"
                            MaxDoseTotal = Some 1600N
                            MinDoseTotalAdjust = Some 25N
                            MaxDoseTotalAdjust = Some 35N
                    }
                ]
                Substances = ["sulfamethoxazol"; "trimethoprim"]
            }

            { emptyDose with
                Indication = "infecties"
                Medication = "cotrimoxazol"
                Route = "oraal"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "cotrimoxazol"
                            Frequencies = [ 2N ]
                            SubstanceName = "sulfamethoxazol"
                            MaxDoseTotal = Some 1600N
                            MinDoseTotalAdjust = Some 25N
                            MaxDoseTotalAdjust = Some 35N
                    }
                ]
                Substances = ["sulfamethoxazol"; "trimethoprim"]
            }

            { emptyDose with
                Indication = "infecties"
                Age = (Some (5 * 364)), None // > 5 jaar
                Medication = "cotrimoxazol"
                Route = "intraveneus"
                Limits = [
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
                        MaxDoseTotal = Some (50N * 20N)
                    }
                ]
                Substances = ["sulfamethoxazol"; "trimethoprim"]
            }

            { emptyDose with
                Indication = "infecties"
                Age = None, (Some (5 * 364)) // < 5 jaar
                Medication = "cotrimoxazol"
                Route = "intraveneus"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "cotrimoxazol"
                            Frequencies = [ 2N ]
                            SubstanceName = "sulfamethoxazol"
                            MaxDoseTotal = Some 1600N
                            MinDoseTotalAdjust = Some 25N
                            MaxDoseTotalAdjust = Some 35N
                    }
                ]
                Substances = ["sulfamethoxazol"; "trimethoprim"]
            }

            { emptyDose with
                Indication = "infecties"
                Age = (Some (5 * 365)), None
                Medication = "cotrimoxazol"
                Route = "intraveneus"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "cotrimoxazol"
                            Frequencies = [ 2N ]
                            SubstanceName = "sulfamethoxazol"
                            MaxDoseTotal = Some 1600N
                            MinDoseTotalAdjust = Some 25N
                            MaxDoseTotalAdjust = Some 35N
                    }
                ]
                Substances = ["sulfamethoxazol"; "trimethoprim"]
            }

            // behandeling PJP
            { emptyDose with
                Indication = "behandeling PJP"
                Medication = "cotrimoxazol"
                Route = "oraal"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "cotrimoxazol"
                            Frequencies = [ 3N; 4N ]
                            SubstanceName = "sulfamethoxazol"
                            MaxDoseTotal = Some 4800N
                            MinDoseTotalAdjust = Some 90N
                            MaxDoseTotalAdjust = Some 100N
                    }
                ]
                Substances = ["sulfamethoxazol"; "trimethoprim"]
            }

            { emptyDose with

                Indication = "behandeling PJP"
                Age = None, (Some (5 * 364)) // < 5 jaar
                Medication = "cotrimoxazol"
                Route = "oraal"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "cotrimoxazol"
                            Frequencies = [ 3N; 4N ]
                            SubstanceName = "sulfamethoxazol"
                            MaxDoseTotal = Some 4800N
                            MinDoseTotalAdjust = Some 90N
                            MaxDoseTotalAdjust = Some 100N
                    }
                ]
                Substances = ["sulfamethoxazol"; "trimethoprim"]
            }

            { emptyDose with
                Indication = "behandeling PJP"
                Age = (Some (5 * 364)), None // > 5 jaar
                Medication = "cotrimoxazol"
                Route = "intraveneus"
                Limits = [
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
                        MaxDoseTotal = Some (50N * 20N)
                    }

                ]
                Substances = ["sulfamethoxazol"; "trimethoprim"]
            }

            { emptyDose with
                Indication = "behandeling PJP"
                Age = None, (Some (5 * 364)) // < 5 jaar
                Medication = "cotrimoxazol"
                Route = "intraveneus"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "cotrimoxazol"
                            Frequencies = [ 3N; 4N ]
                            SubstanceName = "sulfamethoxazol"
                            MaxDoseTotal = Some 4800N
                            MinDoseTotalAdjust = Some 90N
                            MaxDoseTotalAdjust = Some 100N
                    }
                ]
                Substances = ["sulfamethoxazol"; "trimethoprim"]
            }

            { emptyDose with
                Indication = "behandeling PJP"
                Age = (Some (5 * 364)), None // > 5 jaar
                Medication = "cotrimoxazol"
                Route = "intraveneus"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "cotrimoxazol"
                            Frequencies = [ 3N; 4N ]
                            SubstanceName = "sulfamethoxazol"
                            MaxDoseTotal = Some 4800N
                            MinDoseTotalAdjust = Some 90N
                            MaxDoseTotalAdjust = Some 100N
                    }
                ]
                Substances = ["sulfamethoxazol"; "trimethoprim"]
            }
            // profylaxe
            { emptyDose with
                Indication = "profylaxe"
                Medication = "cotrimoxazol"
                Route = "oraal"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "cotrimoxazol"
                            Frequencies = [ 1N ]
                            SubstanceName = "sulfamethoxazol"
                            MaxDoseTotal = Some 800N
                            MinDoseTotalAdjust = Some 15N
                            MaxDoseTotalAdjust = Some 25N
                    }
                ]
                Substances = ["sulfamethoxazol"; "trimethoprim"]
            }

            { emptyDose with
                Indication = "profylaxe"
                Medication = "cotrimoxazol"
                Route = "oraal"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "cotrimoxazol"
                            Frequencies = [ 1N ]
                            SubstanceName = "sulfamethoxazol"
                            MaxDoseTotal = Some 800N
                            MinDoseTotalAdjust = Some 15N
                            MaxDoseTotalAdjust = Some 25N
                    }
                ]
                Substances = ["sulfamethoxazol"; "trimethoprim"]
            }
            // ernstige infecties
            { emptyDose with
                Indication = "ernstige infecties"
                Medication = "gentamicine"
                Route = "intraveneus"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "gentamicine"
                            Frequencies = [ 1N ]
                            SubstanceName = "gentamicine"
                            MinDoseTotalAdjust = Some (65N/10N)
                            MaxDoseTotalAdjust = Some (75N/10N)
                    }
                ]
                Substances = [ "gentamicine" ]
            }

            { emptyDose with
                Indication = "ernstige infecties"
                Medication = "ceftriaxon"
                Route = "intraveneus"
                Limits = [
                    {   DrugOrder.doseLimits with
                            Name = "ceftriaxon"
                            Frequencies = [ 1N ]
                            SubstanceName = "ceftriaxon"
                            MinDoseTotalAdjust = Some (90N)
                            MaxDoseTotalAdjust = Some (100N)
                    }
                ]
                Substances = [ "ceftriaxon" ]
            }

            { emptyDose with
                Indication = "misselijkheid en braken, postoperatief"
                Medication = "ondansetron"
                Route = "oraal"
                Limits = [
                    { DrugOrder.doseLimits with
                        Name = "ondansetron"
                        Frequencies = [ 1N; 2N; 3N ]
                        SubstanceName = "ondansetron"
                        MinDoseQuantityAdjust = Some (4N/100N)
                        MaxDoseQuantityAdjust = Some (1N/10N)
                        MaxDoseQuantity = Some (4N)
                        MaxDoseTotalAdjust = Some (3N/10N)
                    }
                ]
                Substances = [ "ondansetron" ]
            }

            { emptyDose with
                Indication = "misselijkheid en braken, postoperatief"
                Medication = "ondansetron"
                Route = "intraveneus"
                Limits = [
                    { DrugOrder.doseLimits with
                        Name = "ondansetron"
                        Frequencies = [ 1N; 2N; 3N ]
                        SubstanceName = "ondansetron"
                        MinDoseQuantityAdjust = Some (4N/100N)
                        MaxDoseQuantityAdjust = Some (1N/10N)
                        MaxDoseQuantity = Some (4N)
                        MaxDoseTotalAdjust = Some (3N/10N)
                    }
                ]
                Substances = [ "ondansetron" ]
            }

            { emptyDose with
                Indication = "misselijkheid en braken, chemotherapie"
                Medication = "ondansetron"
                Route = "oraal"
                Limits = [
                    { DrugOrder.doseLimits with
                        Name = "ondansetron"
                        Frequencies = [ 1N; 2N; 3N ]
                        SubstanceName = "ondansetron"
                        MinDoseQuantityAdjust = Some (10N)
                        MaxDoseQuantityAdjust = Some (15N)
                        MaxDoseQuantity = Some (8N)
                    }
                ]
                Substances = [ "ondansetron" ]
            }

            { emptyDose with
                Indication = "misselijkheid en braken, chemotherapie"
                Medication = "ondansetron"
                Route = "intraveneus"
                Limits = [
                    { DrugOrder.doseLimits with
                        Name = "ondansetron"
                        Frequencies = [ 1N; 2N; 3N ]
                        SubstanceName = "ondansetron"
                        MinDoseQuantityAdjust = Some (10N)
                        MaxDoseQuantityAdjust = Some (15N)
                        MaxDoseQuantity = Some (16N)
                    }
                ]
                Substances = [ "ondansetron" ]
            }

        ]


    let filter a w ind med route =
        doses
        |> List.filter (fun d ->
            (ind |> Option.isNone || ind = Some d.Indication) &&
            (med |> Option.isNone || med = Some d.Medication) &&
            (route |> Option.isNone || route = Some d.Route) &&
            a |> isBetween d.Age &&
            w |> Some |> isBetween d.Weight
        )


    let filterIndications a w med route =
        filter a w None med route
        |> List.map (fun d -> d.Indication)
        |> List.distinct


    let filterMedications a w ind route =
        filter a w ind None route
        |> List.map (fun d -> d.Medication)
        |> List.distinct


    let filterRoutes a w ind med =
        filter a w ind med None
        |> List.map (fun d -> d.Route)
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


    let create a w ind med route =

        filter a w ind med route
        |> List.collect (fun d ->
            let sols =
                solutions
                |> List.filter (fun s ->
                    w |> Some |> isBetween s.Weight &&
                    med = Some (s.Medication)
                )
                |> List.map (fun s -> s.Solution)

            try

                d.Route
                |> mapRoute
                |> Examples.getOrders d.Medication
                |> List.collect (Examples.calculate w d.Substances d.Limits sols)
                |> List.map (fun sc -> { sc with Route = d.Route })
            with
            | _ ->
                []
        )
        // correct for roundig problem. admin should not be rounded!
        |> List.distinctBy (fun sc -> sc.Route, sc.Administration)
        |> List.map translate
        |> List.mapi (fun i sc -> { sc with No = i + 1 })
