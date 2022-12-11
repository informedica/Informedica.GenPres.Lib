namespace Informedica.GenOrder.Lib


module DrugOrder =

    open MathNet.Numerics
    open Informedica.Utils.Lib
    open Informedica.GenUnits.Lib


    module MinMax =

        let none = { Minimum = None; Maximum = None }


        let setConstraints (minMax : MinMax) (dto: OrderVariable.Dto.VarDto) =
            dto.MinIncl <- minMax.Minimum.IsSome
            dto.Min <-minMax.Minimum
            dto.MaxIncl <- minMax.Maximum.IsSome
            dto.Max <- minMax.Maximum

            dto



    module SolutionRule =

        let limit =
            {
                SubstanceName = ""
                Quantities = []
                MinConcentration = None
                MaxConcentration = None
            }



    module DoseRule =


        let limit =
            {
                Substance = ""
                NormQuantity = None
                Quantity = MinMax.none
                NormQuantityAdjust = None
                QuantityAdjust = MinMax.none
                NormPerTime = None
                PerTime = MinMax.none
                NormPerTimeAdjust = None
                PerTimeAdjust = MinMax.none
                NormRate = None
                Rate = MinMax.none
                NormRateAdjust = None
                RateAdjust = MinMax.none
            }



    let drugOrder =
        {
            Id = ""
            Name = ""
            Products = []
            Quantities = []
            Unit = ""
            TimeUnit = ""
            RateUnit = ""
            Route = ""
            OrderType = AnyOrder
            Frequencies = []
            Rates = []
            DoseQuantities = []
            DoseCount = None
        }


    let productComponent =
        {
            Name = ""
            Shape = ""
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
            Dose = DoseRule.limit
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


    let toOrder (d : DrugOrder) =
        let ou = d.Unit |> unitGroup
        let orbDto = Order.Orderable.Dto.dto d.Id d.Name

        orbDto.OrderableQuantity.Constraints.Vals <- d.Quantities
        orbDto.OrderableQuantity.Unit <- ou
        orbDto.OrderQuantity.Unit <- ou

        orbDto.Dose.Quantity.Constraints.Vals <-
            d.DoseQuantities
        orbDto.DoseCount.Constraints.Vals <-
            d.DoseCount
            |> Option.map List.singleton
            |> Option.defaultValue []

        match d.OrderType with
        | AnyOrder
        | ProcessOrder -> ()

        | ContinuousOrder ->
            orbDto.Dose.Rate.Constraints.Vals <-
                [1N/10N..1N/10N..1000N]
            orbDto.Dose.Rate.Unit <-
                d.RateUnit
                |> unitGroup
                |> sprintf "%s/%s" ou
            orbDto.Dose.RateAdjust.Unit <-
                d.RateUnit
                |> unitGroup
                |> sprintf "%s/kg[Weight]/%s" ou

        | DiscontinuousOrder ->
            orbDto.Dose.Quantity.Unit <- ou
            orbDto.Dose.QuantityAdjust.Unit <- $"{ou}/kg[Weight]"
            orbDto.Dose.PerTime.Unit <-
                d.TimeUnit
                |> unitGroup
                |> sprintf "%s/%s" ou

        | TimedOrder ->
            orbDto.Dose.Rate.Constraints.Vals <-
                [1N/10N..1N/10N..1000N]
            orbDto.Dose.Quantity.Unit <- ou
            orbDto.Dose.QuantityAdjust.Unit <- $"{ou}/kg[Weight]"
            orbDto.Dose.PerTime.Unit <-
                d.TimeUnit
                |> unitGroup
                |> sprintf "%s/%s" ou
            orbDto.Dose.Rate.Unit <-
                d.RateUnit
                |> unitGroup
                |> sprintf "%s/%s" ou
            orbDto.Dose.RateAdjust.Unit <-
                d.RateUnit
                |> unitGroup
                |> sprintf "%s/kg[Weight]/%s" ou

        orbDto.Components <-
            [
                for p in d.Products do
                    let cdto = Order.Orderable.Component.Dto.dto d.Id d.Name p.Name p.Shape

                    cdto.ComponentQuantity.Constraints.Vals <- p.Quantities
                    cdto.OrderableConcentration.Unit <- "x[Count]"
                    cdto.Dose.Quantity.Constraints.Incr <- [ 1N / p.Divisible ]

                    cdto.Items <- [
                        for s in p.Substances do
                            let su = s.Unit |> unitGroup
                            let du = s.DoseUnit |> unitGroup

                            let itmDto =
                                Order.Orderable.Item.Dto.dto d.Id d.Name p.Name s.Name

                            itmDto.ComponentConcentration.Constraints.Vals <- s.Concentrations
                            itmDto.ComponentConcentration.Unit <- $"{su}/{ou}"
                            itmDto.OrderableQuantity.Constraints.Vals <- s.OrderableQuantities
                            itmDto.OrderableQuantity.Unit <- su
                            itmDto.ComponentQuantity.Unit <- su

                            match d.OrderType with
                            | AnyOrder -> ()
                            | ProcessOrder -> ()
                            | ContinuousOrder ->
                                itmDto.Dose.RateAdjust.Unit <-
                                    s.RateUnit
                                    |> unitGroup
                                    |> sprintf "%s/kg[Weight]/%s" du

                                itmDto.Dose.Rate.Constraints <-
                                    itmDto.Dose.Rate.Constraints
                                    |> MinMax.setConstraints s.Dose.Rate

                                itmDto.Dose.RateAdjust.Constraints <-
                                    itmDto.Dose.RateAdjust.Constraints
                                    |> MinMax.setConstraints s.Dose.RateAdjust

                            | DiscontinuousOrder ->
                                itmDto.Dose.Quantity.Unit <- du

                                itmDto.Dose.Quantity.Constraints <-
                                    itmDto.Dose.Quantity.Constraints
                                    |> MinMax.setConstraints s.Dose.Quantity

                                itmDto.Dose.QuantityAdjust.Constraints <-
                                    itmDto.Dose.QuantityAdjust.Constraints
                                    |> MinMax.setConstraints s.Dose.QuantityAdjust

                                itmDto.Dose.PerTimeAdjust.Unit <-
                                    s.TimeUnit
                                    |> unitGroup
                                    |> sprintf "%s/kg[Weight]/%s" du

                                itmDto.Dose.PerTime.Constraints <-
                                    itmDto.Dose.PerTime.Constraints
                                    |> MinMax.setConstraints s.Dose.PerTime

                                itmDto.Dose.PerTimeAdjust.Constraints <-
                                    itmDto.Dose.PerTimeAdjust.Constraints
                                    |> MinMax.setConstraints s.Dose.PerTimeAdjust

                            | TimedOrder ->
                                itmDto.Dose.Quantity.Unit <- du

                                itmDto.Dose.Quantity.Constraints <-
                                    itmDto.Dose.Quantity.Constraints
                                    |> MinMax.setConstraints s.Dose.Quantity

                                itmDto.Dose.QuantityAdjust.Constraints <-
                                    itmDto.Dose.QuantityAdjust.Constraints
                                    |> MinMax.setConstraints s.Dose.QuantityAdjust

                                itmDto.Dose.PerTimeAdjust.Unit <-
                                    s.TimeUnit
                                    |> unitGroup
                                    |> sprintf "%s/kg[Weight]/%s" du

                                itmDto.Dose.PerTime.Constraints <-
                                    itmDto.Dose.PerTime.Constraints
                                    |> MinMax.setConstraints s.Dose.PerTime

                                itmDto.Dose.PerTimeAdjust.Constraints <-
                                    itmDto.Dose.PerTimeAdjust.Constraints
                                    |> MinMax.setConstraints s.Dose.PerTimeAdjust

                                itmDto.Dose.RateAdjust.Unit <-
                                    s.TimeUnit
                                    |> unitGroup
                                    |> sprintf "%s/kg[Weight]/%s" du

                            itmDto
                    ]

                    cdto
            ]

        let dto =
            match d.OrderType with
            | AnyOrder ->
                "the order type cannot by 'Any'"
                |> failwith
            | ProcessOrder ->
                "the order type cannot by 'Any'"
                |> failwith
            | ContinuousOrder ->
                Order.Dto.continuous d.Id d.Name d.Route []
            | DiscontinuousOrder ->
                Order.Dto.discontinuous d.Id d.Name d.Route []
            | TimedOrder ->
                Order.Dto.timed d.Id d.Name d.Route []

        dto.Orderable <- orbDto

        dto.Prescription.Frequency.Unit <-
            $"x[Count]/{(d.TimeUnit |> unitGroup)}"
        dto.Prescription.Frequency.Constraints.Vals <- d.Frequencies

        dto.Adjust.Constraints.Min <- Some (200N /1000N)
        dto.Adjust.Constraints.Max <- Some 150N
        dto.Adjust.Unit <- "kg[Weight]"

        dto


