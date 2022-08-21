namespace Informedica.GenOrder.Lib


module Api =

    open System
    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL
    open Informedica.GenOrder.Lib


    let inline isBetween (min, max) x =
        match x with
        | None -> true
        | Some x ->
            match min, max with
            | None,     None     -> true
            | Some min, Some max -> x >= min && x <= max
            | Some min, None     -> x >= min
            | None,     Some max -> x <= max


    let filter a w ind med shape route  =
        Data.getDoseRules ()
        |> List.filter (fun d ->
            (ind |> Option.isNone || ind = Some d.Indication) &&
            (med |> Option.isNone || med = Some d.Medication) &&
            (shape |> Option.isNone || shape = Some d.Shape) &&
            (route |> Option.isNone || route = Some d.Route) &&
            a |> isBetween d.Age &&
            w |> isBetween d.Weight
        )


    let filterIndications a w med shape route =
        filter a w None med shape route
        |> List.map (fun d -> d.Indication)
        |> List.distinct


    let filterMedications a w ind shape route =
        filter a w ind None shape route
        |> List.map (fun d -> d.Medication)
        |> List.distinct

    let filterShapes a w ind med route =
        filter a w ind med None route
        |> List.map (fun d -> d.Route)
        |> List.distinct


    let filterRoutes a w ind med shape =
        filter a w ind med shape None
        |> List.map (fun d -> d.Route)
        |> List.distinct


    let createDrugOrder (doseRule : DoseRule) =
        let prods =
            Data.getProducts ()
            |> List.filter (fun p ->
                p.Name = doseRule.Medication &&
                p.Shape = doseRule.Shape
            )

        let sols =
            Data.getSolutions ()
            |> List.filter (fun s ->
                s.Medication = doseRule.Medication
            )

        let tryHead m = (List.map m) >> List.tryHead >> (Option.defaultValue "")

        { DrugOrder.drugOrder with
                Id = Guid.NewGuid().ToString()
                Name = doseRule.Medication
                Products = [
                    { DrugOrder.productComponent with
                        Name = prods |> tryHead (fun p -> p.Name)
                        Quantities = prods |> List.collect (fun p -> p.Quantities)
                        TimeUnit = doseRule.TimeUnit
                        RateUnit = sols |> tryHead (fun s -> s.RateUnit)
                        Divisible =
                            prods
                            |> List.choose (fun p -> p.Divisible)
                            |> List.tryHead
                            |> Option.defaultValue 1N
                        Substances =
                            prods
                            |> List.collect (fun p -> p.Substances)
                            |> List.groupBy (fun s -> s.Name)
                            |> List.map (fun (n, xs) ->
                                        {
                                            Name = n
                                            Concentrations =
                                                xs
                                                |> List.collect (fun s -> s.Concentrations)
                                                |> List.distinct
                                            OrderableQuantities = []
                                            Unit = xs |> tryHead (fun x -> x.Unit)
                                            DoseUnit = doseRule.DoseUnit
                                            TimeUnit = doseRule.TimeUnit
                                            RateUnit = doseRule.RateUnit
                                        }
                            )
                    }

                    // if sols |> List.isEmpty |> not then
                    //     sols
                    //     |> List.map (fun s ->
                    //     )
                ]
                Quantities = prods |> List.collect (fun p -> p.Quantities)
                Unit = prods |> tryHead (fun p -> p.Unit)
                TimeUnit = doseRule.TimeUnit
                RateUnit = sols |> tryHead (fun s -> s.RateUnit)
                Shape = doseRule.Shape
                Route = doseRule.Route
                OrderType = doseRule.OrderType
        }


    // print an order list
    let toScenarios ind sn (sc : Order list) =
        sc
        |> List.mapi (fun i o ->
            o
            |> Order.printPrescription sn
            |> fun (pres, prep, adm) ->
                {
                    No = i
                    Indication = ind
                    Name = o.Orderable.Name |> Informedica.GenSolver.Lib.Variable.Name.toString
                    Shape = o.Orderable.Shape
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


    let evaluate age weight doseRule =
        doseRule
        |> createDrugOrder
        |> DrugOrder.toConstrainedOrder
        |> DrugOrder.setDoseRule doseRule
        |> DrugOrder.setAdjust doseRule.Medication weight
        |> DrugOrder.evaluate OrderLogger.logger.Logger
        |> toScenarios doseRule.Indication (doseRule.Limits |> List.map (fun l -> l.SubstanceName))
