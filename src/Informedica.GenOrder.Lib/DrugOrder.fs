namespace Informedica.GenOrder.Lib

open System.Runtime.InteropServices.ObjectiveC


// Creating a drug order
module DrugOrder =

    open MathNet.Numerics
    open Informedica.Utils.Lib
    open Informedica.GenUnits.Lib


    open Types


    module OrderType =

        let map (o : Order) =
            match o.Prescription with
            | Prescription.Process         -> ProcessOrder
            | Prescription.Discontinuous _ -> DiscontinuousOrder
            | Prescription.Continuous      -> ContinuousOrder
            | Prescription.Timed _         -> TimedOrder


        let fromString = function
            | s when s = "proces" -> ProcessOrder |> Some
            | s when s = "ProcessOrder" -> ProcessOrder |> Some
            | s when s = "discontinu" -> DiscontinuousOrder |> Some
            | s when s = "DiscontinuousOrder" -> DiscontinuousOrder |> Some
            | s when s = "continu" -> ContinuousOrder |> Some
            | s when s = "ContinuousOrder" -> ContinuousOrder |> Some
            | s when s = "inlooptijd" -> TimedOrder |> Some
            | s when s = "TimedOrder" -> TimedOrder |> Some
            | s when s = "AnyOrder" -> AnyOrder |> Some
            | _ -> None


    module RouteShape =

        let fromString s =
            match s with
            | s when s = "RectalSolid" -> RectalSolid
            | s when s = "OralSolid" -> OralSolid
            | s when s = "OralFluid" -> OralFluid
            | s when s = "IntravenousFluid" -> IntravenousFluid
            | _ -> AnyRouteShape


        let mapping =
            Web.getDataFromSheet "RouteShape"
            |> fun data ->
                let getColum =
                    data
                    |> Array.head
                    |> Csv.getStringColumn

                data
                |> Array.tail
                |> Array.map (fun r ->
                    let get = getColum r
                    get "Route", get "Shape", get "Mapping" |> fromString
                )
                |> Array.toList


        let map route shape =
            mapping
            |> List.find (fun (r, s, _) -> r = route && s = shape )
            |> fun (_, _, x) -> x


    module Props =

        open Informedica.Utils.Lib.BCL

        module Property = Informedica.GenSolver.Lib.Variable.ValueRange.Property

        let minIncl = Property.createMinInclProp

        let minExcl = Property.createMinExclProp

        let maxIncl = Property.createMaxInclProp

        let maxExcl = Property.createMaxExclProp

        let singleIncr v = set [v] |> Property.createIncrProp

        let incr vs = vs |> Set.ofSeq |> Property.createIncrProp

        let singleVal v = set [v] |> Property.createValsProp

        let vals vs = vs |> Set.ofSeq |> Property.createValsProp

        let toString = Property.toString true

        let fromString s vs =
            let p =
                match s with
                | s when s = "MinIncl" -> fun vs -> vs |> List.head |> minIncl
                | s when s = "MaxIncl" -> fun vs -> vs |> List.head |> maxIncl
                | s when s = "Vals" -> vals
                | s when s = "Incr" -> incr
                | _ -> $"cannot parse {s} to a Property" |> failwith

            vs
            |> String.split ";"
            |> List.choose Double.tryParse
            |> List.choose BigRational.fromFloat
            |> p


    module DrugConstraint =

        module Name = WrappedString.Name
        module Mapping = Order.Mapping
        module Property = Informedica.GenSolver.Lib.Variable.ValueRange.Property
        module Constraint = Informedica.GenSolver.Lib.Constraint
        module ValueSet = Informedica.GenSolver.Lib.Variable.ValueRange.ValueSet
        module Increment = Informedica.GenSolver.Lib.Variable.ValueRange.Increment

        open Informedica.GenSolver.Lib.Types


        let create n m p rs ps =
            {
                Name = n
                Mapping = m
                Property = p
                RouteShape = rs
                OrderType = ps
            }


        let mapToConstraint o (dc : DrugConstraint) : Constraint =
            {
                Name =
                    Order.mapName dc.Name dc.Mapping o
                Property = dc.Property
            }


        let toString (c : DrugConstraint) =
            $"{c.Name} {c.Mapping} {c.Property |> Props.toString}"


        let constraints n =
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
                            |> Props.fromString (get "Property")
                        RouteShape = get "RouteShape" |> RouteShape.fromString
                        OrderType =
                                get "OrderType" |> OrderType.fromString
                                |> Option.defaultValue AnyOrder
                    }
                )
                |> Array.toList


        let filter (o : Order) (cs : DrugConstraint list) =
            let rs = RouteShape.map o.Route o.Orderable.Shape
            let ot = o |> OrderType.map

            let propHasVals = function
            | ValsProp vs -> vs |> ValueSet.isEmpty |> not
            | IncrProp vs -> vs |> Increment.isEmpty |> not
            | _ -> true

            cs
            |> List.filter(fun c ->
                (c.Property |> propHasVals) &&
                (c.RouteShape = AnyRouteShape || c.RouteShape = rs) &&
                (c.OrderType =  AnyOrder      || c.OrderType = ot)
            )


    module DoseRule =

        let limit =
            {
                SubstanceName = ""
                MinDoseQuantity = None
                MaxDoseQuantity = None
                MinDoseQuantityAdjust = None
                MaxDoseQuantityAdjust = None
                MinDoseTotal = None
                MaxDoseTotal = None
                MinDoseTotalAdjust = None
                MaxDoseTotalAdjust = None
                MinDoseRate = None
                MaxDoseRate = None
                MinDoseRateAdjust = None
                MaxDoseRateAdjust = None
            }


        let rule =
            let noneNone = None, None
            {
                Indication = ""
                Medication = ""
                Shape = ""
                Route = ""
                Age = noneNone
                Weight = noneNone
                GestAge = noneNone
                PostAge = noneNone
                OrderType = OrderType.AnyOrder
                Frequencies = []
                Rates = []
                MinTime = None
                MaxTime = None
                DoseUnit = ""
                AdjUnit = ""
                TimeUnit = ""
                RateUnit = ""
                Limits = []
            }


    module SolutionRule =

        let limit =
            {
                SubstanceName = ""
                Quantities = []
                MinConcentration = None
                MaxConcentration = None
            }

        let rule =
            let noneNone = None, None
            {
                Medication = ""
                Age = noneNone
                Weight = noneNone
                Solutions = []
                Quantities = []
                RateUnit = ""
                DoseCount = []
                Limits = []
            }



    module Item = Orderable.Item
    module IDto = Item.Dto
    module Component = Orderable.Component
    module CDto = Component.Dto
    module ODto = Orderable.Dto

    module Mapping = Order.Mapping
    module Property = Informedica.GenSolver.Lib.Variable.ValueRange.Property
    module Constraint = Informedica.GenSolver.Lib.Constraint
    module Name = WrappedString.Name

    open Informedica.GenSolver.Lib.Types

    let (>|>) (cs, o) c = (c |> List.append cs, o)


    let drugOrder =
        {
            Id = ""
            Name = ""
            Products = []
            Quantities = []
            Unit = ""
            TimeUnit = ""
            RateUnit = ""
            Shape = ""
            Route = ""
            OrderType = AnyOrder
        }


    let productComponent =
        {
            Name = ""
            Quantities = []
            TimeUnit = ""
            RateUnit = ""
            Divisible = 1N
            Substances = []
        }


    let substanceItem =
        {
            Name = ""
            Concentrations = []
            OrderableQuantities = []
            Unit = ""
            DoseUnit = ""
            TimeUnit = ""
            RateUnit = ""
        }


    let unitGroup u =
        ValueUnit.Units.units
        |> List.filter (fun ud ->
            ud.Group <> ValueUnit.Group.WeightGroup
        )
        |> List.tryFind (fun ud ->
            [
                ud.Abbreviation.Dut
                ud.Abbreviation.Eng
                ud.Name.Dut
                ud.Name.Eng
            ]
            |> List.append ud.Synonyms
            |> List.exists((=) u)
        )
        |> function
        | Some ud ->
            ud.Group
            |> ValueUnit.Group.toString
        | None -> "General"
        |> sprintf "%s[%s]" u


    let toConstrainedOrder fixedDose (d : DrugOrder) : ConstrainedOrder =
        let ou = d.Unit |> unitGroup
        let odto = ODto.dto d.Id d.Name d.Shape

        odto.OrderableQuantity.Unit <- ou
        odto.OrderQuantity.Unit <- ou

        match d.OrderType with
        | AnyOrder
        | ProcessOrder -> ()

        | ContinuousOrder ->
            odto.DoseRate.Unit <-
                d.RateUnit
                |> unitGroup
                |> sprintf "%s/%s" ou
            odto.DoseRateAdjust.Unit <-
                d.RateUnit
                |> unitGroup
                |> sprintf "%s/kg[Weight]/%s" ou

        | DiscontinuousOrder ->
            odto.DoseQuantity.Unit <- ou
            odto.DoseQuantityAdjust.Unit <- $"{ou}/kg[Weight]"
            odto.DoseTotal.Unit <-
                d.TimeUnit
                |> unitGroup
                |> sprintf "%s/%s" ou

        | TimedOrder ->
            odto.DoseQuantity.Unit <- ou
            odto.DoseQuantityAdjust.Unit <- $"{ou}/kg[Weight]"
            odto.DoseTotal.Unit <-
                d.TimeUnit
                |> unitGroup
                |> sprintf "%s/%s" ou
            odto.DoseRate.Unit <-
                d.RateUnit
                |> unitGroup
                |> sprintf "%s/%s" ou
            odto.DoseRateAdjust.Unit <-
                d.RateUnit
                |> unitGroup
                |> sprintf "%s/kg[Weight]/%s" ou

        odto.Components <-
            [
                for p in d.Products do
                    let cdto = CDto.dto d.Id p.Name

                    cdto.Items <- [
                        for s in p.Substances do
                            let su = s.Unit |> unitGroup
                            let du = s.DoseUnit |> unitGroup

                            let idto = IDto.dto d.Id s.Name

                            idto.ComponentConcentration.Unit <- $"{su}/{ou}"
                            idto.ComponentQuantity.Unit <- su

                            match d.OrderType with
                            | AnyOrder -> ()
                            | ProcessOrder -> ()
                            | ContinuousOrder ->
                                idto.DoseRateAdjust.Unit <-
                                    s.RateUnit
                                    |> unitGroup
                                    |> sprintf "%s/kg[Weight]/%s" du
                            | DiscontinuousOrder ->
                                idto.DoseQuantity.Unit <- du
                                idto.DoseTotalAdjust.Unit <-
                                    s.TimeUnit
                                    |> unitGroup
                                    |> sprintf "%s/kg[Weight]/%s" du
                            | TimedOrder ->
                                idto.DoseQuantity.Unit <- du
                                idto.DoseTotalAdjust.Unit <-
                                    s.TimeUnit
                                    |> unitGroup
                                    |> sprintf "%s/kg[Weight]/%s" du
                                idto.DoseRateAdjust.Unit <-
                                    s.TimeUnit
                                    |> unitGroup
                                    |> sprintf "%s/kg[Weight]/%s" du

                            idto
                    ]

                    cdto.OrderableQuantity.Unit <- ou
                    cdto.OrderableConcentration.Unit <- "x[Count]"
                    cdto.OrderQuantity.Unit <- ou

                    cdto
            ]

        let dto =
            match d.OrderType with
            | AnyOrder ->
                "the order type cannot by 'Any'"
                |> failwith
            | ProcessOrder ->
                Order.Dto.``process`` d.Id d.Name d.Shape d.Route
            | ContinuousOrder ->
                Order.Dto.continuous d.Id d.Name d.Shape d.Route
            | DiscontinuousOrder ->
                Order.Dto.discontinuous d.Id d.Name d.Shape d.Route
            | TimedOrder ->
                Order.Dto.timed d.Id d.Name d.Shape d.Route

        dto.Orderable <- odto

        dto.Prescription.Frequency.Unit <-
            $"x[Count]/{(d.TimeUnit |> unitGroup)}"
        dto.Adjust.Unit <- "kg[Weight]"

        let cstr m p rs ot =
            DrugConstraint.create d.Name m p rs ot

        dto
        |> Order.Dto.fromDto
        |> fun o ->
            // first add all general orderable constraints
            let co =
                DrugConstraint.constraints (o.Orderable.Name |> Name.toString)
                // filter out OrderableDoseCount if fixedDose
                |> List.map (fun c ->
                    match c.Mapping with
                    | OrderableDoseCount when fixedDose ->
                        cstr OrderableDoseCount
                            (1N |> Props.minIncl)
                            AnyRouteShape AnyOrder
                    | _ -> c
                ), o
            // adding orderable constraints
            co
            >|> [
                    if d.Quantities |> List.isEmpty |> not then
                        // ALL set possible orderable quantities
                        cstr OrderableOrderableQty
                            (d.Quantities |> Props.vals)
                            AnyRouteShape AnyOrder
                    else
                        // set the divisible
                        let divs =
                            d.Products
                            |> List.map (fun p -> p.Divisible)
                            |> List.distinct
                            |> Props.incr

                        cstr OrderableOrderableQty
                             divs
                             AnyRouteShape AnyOrder

                ]
        |> fun co ->
            d.Products
            |> Seq.fold (fun co p ->
                let n = p.Name
                // adding component constraints
                let co =
                    co
                    >|> [
                            // ALL set possible component quantities
                            DrugConstraint.create n
                                ComponentComponentQty
                                (p.Quantities |> Props.vals)
                                AnyRouteShape AnyOrder

                            if not fixedDose then
                                DrugConstraint.create n
                                    ComponentOrderableQty
                                    (p.Divisible |> Props.singleIncr)
                                    AnyRouteShape AnyOrder


                            // SINGLE COMPONENT
                            if d.Products |> List.length = 1 then
                                DrugConstraint.create n
                                    ComponentOrderableConc
                                    (1N |> Props.singleVal)
                                    AnyRouteShape AnyOrder
                        ]

                p.Substances
                |> Seq.fold (fun co s ->
                    let n = s.Name
                    // adding item constraints
                    co
                    >|> [
                            // ALL set concentrations and quanties
                            if s.Concentrations |> List.isEmpty |> not then
                                DrugConstraint.create n
                                    ItemComponentConc
                                    (s.Concentrations |> Props.vals)
                                    AnyRouteShape AnyOrder
                            if s.OrderableQuantities |> List.isEmpty |> not then
                                DrugConstraint.create n
                                    ItemOrderableQty
                                    (s.OrderableQuantities |> Props.vals)
                                    AnyRouteShape AnyOrder
                            if d.Products |> List.length = 1 && s.Concentrations |> List.isEmpty |> not then
                                DrugConstraint.create n
                                    ItemOrderableConc
                                    (s.Concentrations |> Props.vals)
                                    AnyRouteShape AnyOrder

                        ]
                ) co
            ) co
            |> fun (cs, o) -> cs |> DrugConstraint.filter o, o


    let setDoseRule (dr : DoseRule) (co : ConstrainedOrder) : ConstrainedOrder =
        let cr sn m c v co =
            match v with
            | Some v ->
                co
                >|> [ DrugConstraint.create sn m (c v) AnyRouteShape AnyOrder ]
            | None -> co

        co
        |> function
        | cs, o ->
            if dr.Rates |> List.isEmpty then (cs, o)
            else
                let drc =
                    DrugConstraint.create dr.Medication
                        OrderableDoseRate
                        (dr.Rates |> Props.vals)
                        AnyRouteShape ContinuousOrder

                cs
                |> List.replace (fun c ->
                    c.Mapping = OrderableDoseRate &&
                    c.OrderType = ContinuousOrder
                ) drc
                , o
        >|> [
                if dr.Frequencies |> List.isEmpty |> not then
                    DrugConstraint.create dr.Medication
                        PresFreq
                        (dr.Frequencies |> Props.vals)
                        AnyRouteShape DiscontinuousOrder
                    DrugConstraint.create dr.Medication
                        PresFreq
                        (dr.Frequencies |> Props.vals)
                        AnyRouteShape TimedOrder
                if dr.MinTime.IsSome then
                    DrugConstraint.create dr.Medication
                        PresTime
                        (dr.MinTime.Value |> Props.minIncl)
                        AnyRouteShape TimedOrder
                if dr.MaxTime.IsSome then
                    DrugConstraint.create dr.Medication
                        PresTime
                        (dr.MaxTime.Value |> Props.maxIncl)
                        AnyRouteShape TimedOrder
        ]
        |> function
        | cs, o ->
            dr.Limits
            |> List.fold (fun acc l ->
                let sn = l.SubstanceName
                acc
                |> cr sn ItemDoseQty Props.minIncl l.MinDoseQuantity
                |> cr sn ItemDoseQty Props.maxIncl l.MaxDoseQuantity
                |> cr sn ItemDoseAdjustQtyAdjust Props.minIncl l.MinDoseQuantityAdjust
                |> cr sn ItemDoseAdjustQtyAdjust Props.maxIncl l.MaxDoseQuantityAdjust
                |> cr sn ItemDoseTotal Props.minIncl l.MinDoseTotal
                |> cr sn ItemDoseTotal Props.maxIncl l.MaxDoseTotal
                |> cr sn ItemDoseAdjustTotalAdjust Props.minIncl l.MinDoseTotalAdjust
                |> cr sn ItemDoseAdjustTotalAdjust Props.maxIncl l.MaxDoseTotalAdjust
                |> cr sn ItemDoseRate Props.minIncl l.MinDoseRate
                |> cr sn ItemDoseRate Props.maxIncl l.MaxDoseRate
                |> cr sn ItemDoseAdjustRateAdjust Props.minIncl l.MinDoseRateAdjust
                |> cr sn ItemDoseAdjustRateAdjust Props.maxIncl l.MaxDoseRateAdjust

            ) (cs,o)
            |> fun (cs, o) -> cs |> DrugConstraint.filter o, o


    let setSolutionRule fixedDose
                        (sr : SolutionRule)
                        (co : ConstrainedOrder) : ConstrainedOrder =
        let set n m c v co =
            match v with
            | Some v ->
                co
                >|> [ DrugConstraint.create n m (c v) AnyRouteShape AnyOrder ]
            | None -> co

        co
        >|> [
                if sr.DoseCount |> List.isEmpty |> not && (not fixedDose) then
                    DrugConstraint.create
                        sr.Medication
                        OrderableDoseCount
                        (sr.DoseCount |> Props.vals)
                        AnyRouteShape AnyOrder
            ]
        |> function
        | cs, o ->
            sr.Limits
            |> List.fold (fun acc l ->
                let sn = l.SubstanceName
                acc
                |> set sn ItemOrderableConc Props.minIncl l.MinConcentration
                |> set sn ItemOrderableConc Props.maxIncl l.MaxConcentration
            ) (cs, o)
            |> fun (cs, o) -> cs |> DrugConstraint.filter o, o


    let setAdjust n a (co : ConstrainedOrder) : ConstrainedOrder =
        co
        >|> [
                DrugConstraint.create
                    n
                    OrderAdjustQty
                    (a |> Props.singleVal)
                    AnyRouteShape AnyOrder
            ]


    let evaluate log (co : ConstrainedOrder) =
        let cs, o = co

        o
        |> Events.OrderSolvedStarted
        |> Logging.logInfo log

        let cs = cs |> List.map (DrugConstraint.mapToConstraint o)

        o
        |> Order.solveUnits log
        |> Order.solveConstraints log cs
        |> fun x -> printfn "solved constraints"; x
        |> Order.calcScenarios log

