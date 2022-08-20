
#load "load.fsx"

open System
open System.Data
open System.IO
open System.Net
open System.Net.Http
open MathNet.Numerics

open Informedica.Utils.Lib.BCL
open Informedica.GenOrder.Lib
open Types



let odto = Order.Dto.discontinuous "1" "paracetamol" "zetpil" "rectaal"
odto.Orderable.Components <- [
    let cdto =  Orderable.Component.Dto.dto "1" "paracetamol"
    cdto.Items <- [
        Orderable.Item.Dto.dto "1" "paracetamol"
    ]
    cdto
]

odto
|> Order.Dto.fromDto
|> Order.toEqs
|> fun (vs1, vs2) -> vs1 @ vs2
|> List.collect id
|> List.map (fun v -> v.Variable.Name)
|> List.distinct
|> List.sort
|> List.iter (printfn "%A")

let getConstraints n =
    Web.getDataFromSheet "General"
    |> fun data ->
        let getColum =
            data
            |> Array.head
            |> Csv.getStringColumn

        data
        |> Array.tail
        |> Array.map (fun r ->
            let get = getColum r
            {
                Name = n
                Mapping =
                    get "Mapping" |> Order.Mapping.fromString
                Property =
                    get "Values"
                    |> DrugOrder.Props.fromString (get "Property")
                Limit = Informedica.GenSolver.Lib.Types.NoLimit
                RouteShape = AnyRouteShape
                OrderType =
                        get "OrderType" |> DrugOrder.OrderType.fromString
                        |> Option.defaultValue AnyOrder
            }
        )


let toBrs s =
    s
    |> String.splitAt ';'
    |> Array.choose Double.tryParse
    |> Array.choose BigRational.fromFloat


let toBrOpt brs = brs |> Array.tryHead


let tupleBrOpt brs1 brs2 =
    brs1 |> Array.tryHead,
    brs2 |> Array.tryHead


let inline isBetween (min, max) x =
    match x with
    | None -> true
    | Some x ->
        match min, max with
        | None,     None     -> true
        | Some min, Some max -> x >= min && x <= max
        | Some min, None     -> x >= min
        | None,     Some max -> x <= max


let getSolutions () =
    Web.getDataFromSheet "Solutions"
    |> fun data ->
        let getColumn =
            data
            |> Array.head
            |> Csv.getStringColumn

        data
        |> Array.tail
        |> Array.map (fun r ->
            let get = getColumn r
            {|
                Medication = get "Medication"
                MinAge = get "MinAge" |> toBrs
                MaxAge = get "MaxAge" |> toBrs
                MinWeight = get "MinWeight" |> toBrs
                MaxWeight = get "MaxWeight" |> toBrs
                Solution = get "Solution"
                SolQuantities = get "SolQuantities" |> toBrs
                RateUnit = get "RateUnit"
                DoseCount = get "DoseCount" |> toBrs
                SubstName = get "SubstName"
                SubstQuantities = get "SubstQuantities" |> toBrs
                MinConcentration = get "MinConcentration" |> toBrs
                MaxConcentration = get "MaxConcentration" |> toBrs
            |}
        )
        |> Array.groupBy (fun r ->
            {
                Medication = r.Medication
                Age = tupleBrOpt r.MinAge r.MaxAge
                Weight = tupleBrOpt r.MinWeight r.MaxWeight
                Solution = r.Solution
                Quantities = r.SolQuantities |> Array.toList
                RateUnit = r.RateUnit
                DoseCount = r.DoseCount |> Array.toList
                Limits = []
            }
        )
        |> Array.map (fun (sr, rs) ->
            { sr with
                Limits =
                    rs
                    |> Array.map (fun r ->
                        let minC, maxC = tupleBrOpt r.MinConcentration r.MaxConcentration
                        {
                            SubstanceName = r.SubstName
                            Quantities = r.SubstQuantities |> Array.toList
                            MinConcentration = minC
                            MaxConcentration = maxC
                        }
                    )
                    |> Array.toList

            }
        )
        |> Array.toList


let getDoseRules () =
    Web.getDataFromSheet "DoseRules"
    |> fun data ->
        let getColumn =
            data
            |> Array.head
            |> Csv.getStringColumn

        data
        |> Array.tail
        |> Array.map (fun r ->
            let get = getColumn r
            let toBrOpt = toBrs >> toBrOpt
            {|
                Indication = get "Indication"
                Medication = get "Medication"
                Shape = get "Shape"
                Route = get "Route"
                MinAge = get "MinAge" |> toBrOpt
                MaxAge = get "MaxAge" |> toBrOpt
                MinWeight = get "MinWeight" |> toBrOpt
                MaxWeight = get "MaxWeight" |> toBrOpt
                MinGest = get "MinGest" |> toBrOpt
                MaxGest = get "MaxGest" |> toBrOpt
                MinPost = get "MinPost" |> toBrOpt
                MaxPost = get "MaxPost" |> toBrOpt
                OrderType = get "OrderType"
                Frequencies = get "Frequencies" |> toBrs
                Rates = get "Rates" |> toBrs
                MinTime = get "MinTime" |> toBrOpt
                MaxTime = get "MaxTime" |> toBrOpt
                DoseUnit = get "DoseUnit"
                AdjUnit = get "AdjUnit"
                TimeUnit = get "TimeUnit"
                RateUnit = get "RateUnit"
                Substance = get "Substance"
                MinDoseQty = get "MinDoseQty" |> toBrOpt
                MaxDoseQty = get "MaxDoseQty" |> toBrOpt
                MinDoseQtyAdj = get "MinDoseQtyAdj" |> toBrOpt
                MaxDoseQtyAdj = get "MaxDoseQtyAdj" |> toBrOpt
                MinDoseTot = get "MinDoseTot" |> toBrOpt
                MaxDoseTot = get "MaxDoseTot" |> toBrOpt
                MinDoseTotAdj = get "MinDoseTotAdj" |> toBrOpt
                MaxDoseTotAdj = get "MaxDoseTotAdj" |> toBrOpt
                MinDoseRate = get "MinDoseRate" |> toBrOpt
                MaxDoseRate = get "MaxDoseRate" |> toBrOpt
                MinDoseRateAdj = get "MinDoseRateAdj" |> toBrOpt
                MaxDoseRateAdj = get "MaxDoseRateAdj" |> toBrOpt
            |}
        )
        |> Array.groupBy (fun r ->
            {
                Indication = r.Indication
                Medication = r.Medication
                Shape = r.Shape
                Route = r.Route
                Age = r.MinAge, r.MaxAge
                Weight = r.MinWeight, r.MaxWeight
                GestAge = r.MinGest, r.MaxGest
                PostAge = r.MinPost, r.MaxPost
                OrderType =
                    r.OrderType
                    |> DrugOrder.OrderType.fromString
                    |> Option.defaultValue AnyOrder
                Frequencies = r.Frequencies |> Array.toList
                Rates = r.Rates |> Array.toList
                MinTime = r.MinTime
                MaxTime = r.MaxTime
                DoseUnit = r.DoseUnit
                AdjUnit = r.AdjUnit
                TimeUnit = r.TimeUnit
                RateUnit = r.RateUnit
                Limits = []
            }
        )
        |> Array.map (fun (dr, rs) ->
            { dr with
                Limits =
                    rs
                    |> Array.map (fun r ->
                        {
                            SubstanceName = r.Substance
                            MinDoseQuantity = r.MinDoseQty
                            MaxDoseQuantity = r.MaxDoseQty
                            MinDoseQuantityAdjust = r.MinDoseQtyAdj
                            MaxDoseQuantityAdjust = r.MaxDoseQtyAdj
                            MinDoseTotal = r.MinDoseTot
                            MaxDoseTotal = r.MaxDoseTot
                            MinDoseTotalAdjust = r.MinDoseTotAdj
                            MaxDoseTotalAdjust = r.MaxDoseTotAdj
                            MinDoseRate = r.MinDoseRate
                            MaxDoseRate = r.MaxDoseRate
                            MinDoseRateAdjust = r.MinDoseRateAdj
                            MaxDoseRateAdjust = r.MaxDoseRateAdj
                        }
                    )
                    |> Array.toList
            }
        )
        |> Array.toList


let getProducts () =
    Web.getDataFromSheet "Products"
    |> fun data ->
        let getColumn =
            data
            |> Array.head
            |> Csv.getStringColumn

        data
        |> Array.tail
        |> Array.map (fun r ->
            let get = getColumn r
            let toBrOpt = toBrs >> toBrOpt

            {|
                Name = get "Name"
                Shape = get "Shape"
                Divisible = "Divisible" |> toBrOpt
                Unit = get "Unit"
                Quantities = get "Quantities" |> toBrs
                SubstanceName = get "SubstanceName"
                SubstanceConcentrations = get "SubstanceConcentrations" |> toBrs
                SubstanceUnit = get "SubstanceUnit"
            |}
        )
        |> Array.groupBy (fun r ->
            {
                Name = r.Name
                Shape = r.Shape
                Unit = r.Unit
                Divisible = r.Divisible
                Quantities = r.Quantities |> Array.toList
                Substances = []
            }
        )
        |> Array.map (fun (p, rs) ->
            { p with
                Substances =
                    rs
                    |> Array.map (fun r ->
                        {
                            Name = r.SubstanceName
                            Unit = r.Unit
                            Concentrations = r.SubstanceConcentrations |> Array.toList
                            Quantities = []
                        }
                    )
                    |> Array.toList
            }
        )
        |> Array.toList


let filter a w ind med shape route rules =
    rules
    |> List.filter (fun d ->
        (ind |> Option.isNone || ind = Some d.Indication) &&
        (med |> Option.isNone || med = Some d.Medication) &&
        (shape |> Option.isNone || shape = Some d.Shape) &&
        (route |> Option.isNone || route = Some d.Route) &&
        a |> isBetween d.Age &&
        w |> isBetween d.Weight
    )


let filterIndications a w med shape route rules =
    rules
    |> filter a w None med shape route
    |> List.map (fun d -> d.Indication)
    |> List.distinct


let filterMedications a w ind shape route rules =
    rules
    |> filter a w ind None shape route
    |> List.map (fun d -> d.Medication)
    |> List.distinct

let filterShapes a w ind med route rules =
    rules
    |> filter a w ind med None route
    |> List.map (fun d -> d.Route)
    |> List.distinct


let filterRoutes a w ind med shape rules =
    rules
    |> filter a w ind med shape None
    |> List.map (fun d -> d.Route)
    |> List.distinct


let createDrugOrder (doseRule : DoseRule) =
    let prods =
        getProducts ()
        |> List.filter (fun p ->
            p.Name = doseRule.Medication &&
            p.Shape = doseRule.Shape
        )

    let sols =
        getSolutions ()
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
    |> DrugOrder.create
    |> DrugOrder.setDoseRule doseRule


// print an order list
let toScenarios sn (sc : Order list) =
    sc
    |> List.mapi (fun i o ->
        o
        |> Order.printPrescription sn
        |> fun (pres, prep, adm) ->
            {
                No = i
                Name = o.Orderable.Name |> Informedica.GenSolver.Lib.Variable.Name.toString
                Shape = o.Orderable.Shape
                Route = o.Route
                Prescription = pres
                Preparation = prep
                Administration = adm
            }
    )


getDoseRules ()
|> filter None None None (Some "paracetamol") (Some "zetpil") None
|> List.head
|> createDrugOrder
|> DrugOrder.setAdjust "paracetamol" 10N
|> DrugOrder.evaluate OrderLogger.logger.Logger
|> toScenarios [ "paracetamol" ]

