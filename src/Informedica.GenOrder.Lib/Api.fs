namespace Informedica.GenOrder.Lib



module Api =

    open System
    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL
    open Informedica.GenForm.Lib
    open Informedica.GenOrder.Lib


    let tryHead m = (Array.map m) >> Array.tryHead >> (Option.defaultValue "")


    let createProductComponent noSubst freqUnit (doseLimits : DoseLimit []) (prods : Product[]) =
        { DrugOrder.productComponent with
            Name = prods |> tryHead (fun p -> p.Label)
            Quantities =
                prods
                |> Array.choose (fun p -> p.ShapeQuantity)
                |> Array.distinct
                |> Array.toList
            TimeUnit = freqUnit
            RateUnit = "uur" //doseRule.RateUnit
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
                            TimeUnit = freqUnit
                            Dose =
                                doseLimits
                                |> Array.tryFind (fun l -> l.Substance = n)
                                |> Option.defaultValue DoseRule.DoseLimit.limit
                            Solution = None
                        }
                    )
                    |> Array.toList
        }


    let createDrugOrder (doseRule : DoseRule) =
        { DrugOrder.drugOrder with
            Id = Guid.NewGuid().ToString()
            Name = doseRule.Generic
            Products =
                doseRule.Products
                |> createProductComponent false doseRule.FreqUnit doseRule.DoseLimits
                |> List.singleton
            Quantities = []
            Frequencies = doseRule.Frequencies |> Array.toList
            FreqUnit = doseRule.FreqUnit
            Unit =
                doseRule.Products
                |> tryHead (fun p -> p.ShapeUnit)
            TimeUnit = doseRule.TimeUnit
            RateUnit = "uur"
            Route = doseRule.Route
            DoseCount =
                if doseRule.Products |> Array.length = 1 then Some 1N
                else None
            OrderType =
                match doseRule.DoseType with
                | Informedica.GenForm.Lib.Types.Continuous -> ContinuousOrder
                | _ when doseRule.TimeUnit |> String.isNullOrWhiteSpace -> DiscontinuousOrder
                | _ -> TimedOrder
        }


    let createDrugOrders (solutionRule: SolutionRule option) (doseRule : DoseRule) =
        let parent = Product.getParenteral ()

        match solutionRule with
        | None -> [ createDrugOrder doseRule ]
        | Some solRule ->
            solRule.Solutions
            |> Array.map (fun s ->
                createDrugOrder doseRule
                |> fun drugOrder ->
                    { drugOrder with
                        Quantities = solRule.Volumes |> Array.toList
                        DoseCount =
                            solRule.DosePerc.Maximum
                        Products =
                            parent
                            |> Array.filter (fun p -> p.Generic |> String.startsWith s)
                            |> createProductComponent true doseRule.FreqUnit [||]
                            |> List.singleton
                            |> List.append drugOrder.Products
                    }
            )
            |> Array.toList


    let setAdjust wght (drugOrd : DrugOrder) =
        let wght = wght |> BigRational.fromFloat

        { drugOrd with
            Adjust = wght
        }


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



