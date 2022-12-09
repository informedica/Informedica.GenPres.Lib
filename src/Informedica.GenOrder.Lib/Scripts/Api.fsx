

#load "load.fsx"



#time


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


    let tryHead m = (List.map m) >> List.tryHead >> (Option.defaultValue "")


    let createProductComponent (doseRule : DoseRule) rateUnit noSubst (prods : Product list) =
        { DrugOrder.productComponent with
            Name = prods |> tryHead (fun p -> p.Name)
            Quantities = prods |> List.collect (fun p -> p.Quantities)
            TimeUnit = doseRule.TimeUnit
            RateUnit = rateUnit //sols |> tryHead (fun s -> s.RateUnit)
            Divisible =
                prods
                |> List.choose (fun p -> p.Divisible)
                |> List.tryHead
                |> Option.defaultValue 1N
            Substances =
                if noSubst then []
                else
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
                            Dose = 
                                doseRule.Limits
                                |> List.tryFind (fun l -> l.SubstanceName = n)
                                |> Option.defaultValue DrugOrder.DoseRule.limit

                        }
                    )
        }


    let createDrugOrder rateUnit (prods : Product list) (doseRule : DoseRule) =
        { DrugOrder.drugOrder with
                Id = Guid.NewGuid().ToString()
                Name = doseRule.Medication
                Products = [ prods |> createProductComponent doseRule rateUnit false ]
                Quantities = []
                Frequencies = doseRule.Frequencies
                Unit = prods |> tryHead (fun p -> p.Unit)
                TimeUnit = doseRule.TimeUnit
                RateUnit = rateUnit //sols |> tryHead (fun s -> s.RateUnit)
                Route = doseRule.Route
                OrderType = doseRule.OrderType |> Order.OrderType.map
        }


    let createDrugOrders (solutionRule: SolutionRule option) (doseRule : DoseRule) =
        let prods =
            Data.getProducts ()
            |> List.filter (fun p ->
                p.Name = doseRule.Medication &&
                p.Shape = doseRule.Shape
            )

        match solutionRule with
        | None -> [ createDrugOrder "" prods doseRule ]
        | Some solRule ->
            let rateUnit = solRule.RateUnit
            solRule.Solutions
            |> List.map (fun s ->
                createDrugOrder rateUnit prods doseRule
                |> fun drugOrder ->
                    { drugOrder with
                        Quantities = solRule.Quantities
                        Products =
                            Data.getProducts ()
                            |> List.filter (fun p -> p.Name = s)
                            |> createProductComponent doseRule rateUnit true
                            |> List.singleton
                            |> List.append drugOrder.Products
                    }
            )


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



open MathNet.Numerics
open Informedica.GenSolver.Lib
open Informedica.GenOrder.Lib



let path = Some $"{__SOURCE_DIRECTORY__}/log.txt"
let startLogger () =
    // Start the logger at an informative level
    OrderLogger.logger.Start path Logging.Level.Informative


startLogger()
Api.filter None None None (Some "paracetamol") None None
|> List.item 2
|> Api.createDrugOrders None
|> List.item 0
|> DrugOrder.toOrder
|> Order.Dto.fromDto
|> Order.solveMinMax { Log = ignore }
|> Order.toString
|> List.iter (printfn "%s")

startLogger()
Api.filter None None None (Some "noradrenaline") (Some "infusievloeistof") None
|> List.item 0


// report output to the fsi
OrderLogger.logger.Report ()


