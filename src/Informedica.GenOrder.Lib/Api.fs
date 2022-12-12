namespace Informedica.GenOrder.Lib



module Api =

    open System
    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL
    open Informedica.GenOrder.Lib

    open Informedica.GenForm.Lib

    module DoseRule = Informedica.GenForm.Lib.DoseRule


    let inline isBetween (minMax : MinMax) x =
        match x with
        | None -> true
        | Some x ->
            match minMax.Minimum, minMax.Maximum with
            | None,     None     -> true
            | Some min, Some max -> x >= min && x <= max
            | Some min, None     -> x >= min
            | None,     Some max -> x <= max


    let filter a w ind med shape route  =
        DoseRule.doseRules ()
        |> Array.filter (fun d ->
            (ind |> Option.isNone || ind = Some d.Indication) &&
            (med |> Option.isNone || med = Some d.Generic) &&
            (shape |> Option.isNone || shape = Some d.Shape) &&
            (route |> Option.isNone || route = Some d.Route) &&
            a |> isBetween d.Patient.Age &&
            w |> isBetween d.Patient.Weight
        )


    let filterIndications a w med shape route =
        filter a w None med shape route
        |> Array.map (fun d -> d.Indication)
        |> Array.distinct


    let filterMedications a w ind shape route =
        filter a w ind None shape route
        |> Array.map (fun d -> d.Generic)
        |> Array.distinct


    let filterShapes a w ind med route =
        filter a w ind med None route
        |> Array.map (fun d -> d.Route)
        |> Array.distinct


    let filterRoutes a w ind med shape =
        filter a w ind med shape None
        |> Array.map (fun d -> d.Route)
        |> Array.distinct


    let tryHead m = (Array.map m) >> Array.tryHead >> (Option.defaultValue "")


    let createProductComponent (doseRule : DoseRule) rateUnit noSubst (prods : Product[]) =
        { DrugOrder.productComponent with
            Name = prods |> tryHead (fun p -> p.Label)
            Quantities =
                prods
                |> Array.choose (fun p -> p.ShapeQuantity)
                |> Array.toList
            TimeUnit = doseRule.TimeUnit
            RateUnit =
                rateUnit //sols |> tryHead (fun s -> s.RateUnit)
            Divisible =
                prods
                |> Array.choose (fun p -> p.Divisible)
                |> Array.tryHead
                |> Option.defaultValue 1N
            Substances =
                if noSubst then []
                else
                    prods
                    |> Array.collect (fun p -> p.Substances)
                    |> Array.groupBy (fun s -> s.Name)
                    |> Array.map (fun (n, xs) ->
                        {
                            Name = n
                            Concentrations =
                                xs
                                |> Array.choose (fun s -> s.Quantity)
                                |> Array.distinct
                                |> Array.toList
                            OrderableQuantities = []
                            Unit = xs |> tryHead (fun x -> x.Unit)
                            DoseUnit = doseRule.DoseUnit
                            TimeUnit = doseRule.TimeUnit
                            RateUnit = doseRule.RateUnit
                            Dose =
                                doseRule.DoseLimits
                                |> Array.tryFind (fun l -> l.Substance = n)
                                |> Option.defaultValue DoseRule.DoseLimit.limit
                            Solution = None
                        }
                    )
                    |> Array.toList
        }


    let createDrugOrder rateUnit (doseRule : DoseRule) =
        { DrugOrder.drugOrder with
                Id = Guid.NewGuid().ToString()
                Name = doseRule.Generic
                Products =
                    doseRule.Products
                    |> createProductComponent doseRule rateUnit false
                    |> List.singleton
                Quantities = []
                Frequencies = doseRule.Frequencies |> Array.toList
                Unit =
                    doseRule.Products
                    |> tryHead (fun p -> p.ShapeUnit)
                TimeUnit = doseRule.TimeUnit
                RateUnit = rateUnit //sols |> tryHead (fun s -> s.RateUnit)
                Route = doseRule.Route
                OrderType =
                    match doseRule.DoseType with
                    | Continuous -> ContinuousOrder
                    | _ when rateUnit |> String.isNullOrWhiteSpace -> DiscontinuousOrder
                    | _ -> TimedOrder
        }


    let createDrugOrders (solutionRule: SolutionRule option) (doseRule : DoseRule) =
 
        match solutionRule with
        | None -> [ createDrugOrder "" doseRule ]
        | Some solRule ->
            let rateUnit = solRule.RateUnit
            solRule.Solutions
            |> Array.map (fun s ->
                createDrugOrder rateUnit doseRule
                |> fun drugOrder ->
                    { drugOrder with
                        Quantities = solRule.Volumes |> Array.toList
                        Products =
                            doseRule.Products
                            |> Array.filter (fun p -> p.Generic = s)
                            |> createProductComponent doseRule rateUnit true
                            |> List.singleton
                            |> List.append drugOrder.Products
                    }
            )
            |> Array.toList


    // print an order list
    let toScenarios ind sn (sc : Order list) =
        sc
        |> List.mapi (fun i o ->
            o
            |> Order.Print.printPrescription sn
            |> fun (pres, prep, adm) ->
                {
                    No = i
                    Indication = ind
                    Name = o.Orderable.Name |> Informedica.GenSolver.Lib.Variable.Name.toString
                    Shape = o.Orderable.Components[0].Shape
                    Route = o.Route
                    Prescription = pres
                    Preparation = prep
                    Administration = adm
                }
        )


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

