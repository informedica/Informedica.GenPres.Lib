namespace Informedica.GenOrder.Lib


module DrugOrder =

    open MathNet.Numerics
    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL
    open Informedica.GenUnits.Lib

    type MinMax = Informedica.GenForm.Lib.Types.MinMax


    module DoseRule = Informedica.GenForm.Lib.DoseRule
    module DoseLimit = DoseRule.DoseLimit



    module MinMax =

        let setConstraints (br : BigRational option) (minMax : MinMax) (dto: OrderVariable.Dto.VarDto) =
            let min =
                match minMax.Minimum, br with
                | Some min, _ -> Some min
                | None, _     -> br |> Option.map (fun br -> br - br / 10N)

            let max =
                match minMax.Maximum, br with
                | Some max, _ -> Some max
                | None, _     -> br |> Option.map (fun br -> br + br / 10N)

            dto.MinIncl <- min.IsSome
            dto.Min <- min
            dto.MaxIncl <- max.IsSome
            dto.Max <- max

            dto


    let drugOrder =
        {
            Id = ""
            Name = ""
            Products = []
            Quantities = []
            Unit = ""
            FreqUnit = ""
            TimeUnit = ""
            RateUnit = ""
            Route = ""
            OrderType = AnyOrder
            Frequencies = []
            Rates = []
            DoseQuantities = []
            DoseCount = None
            Adjust = None
            AdjustUnit = ""
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
            TimeUnit = ""
            Dose = DoseLimit.limit
            Solution = None
        }


    let unitGroup u =
        if u = "kg" then "kg[Weight]"
        else
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
                |> List.exists(String.equalsCapInsens u)
            )
            |> function
            | Some ud ->
                ud.Group
                |> ValueUnit.Group.toString
            | None -> "General"
            |> sprintf "%s[%s]" u


    let toOrder (d : DrugOrder) =
        let setDoseRate (orbDto : Order.Orderable.Dto.Dto) =
            orbDto.Dose.Rate.Constraints.Incr <- [ 1N/10N ]
            orbDto.Dose.Rate.Constraints.MinIncl <- true
            orbDto.Dose.Rate.Constraints.Min <- 1N/10N |> Some
            orbDto.Dose.Rate.Constraints.MaxIncl <- true
            orbDto.Dose.Rate.Constraints.Max <- 1000N |> Some

        let ou = d.Unit |> unitGroup
        let au = d.AdjustUnit |> unitGroup
        let orbDto = Order.Orderable.Dto.dto d.Id d.Name

        orbDto.DoseCount.Constraints.Vals <-
            d.DoseCount
            |> Option.map List.singleton
            |> Option.defaultValue []

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
            orbDto |> setDoseRate
            orbDto.Dose.Rate.Unit <-
                d.RateUnit
                |> unitGroup
                |> sprintf "%s/%s" ou
            orbDto.Dose.RateAdjust.Unit <-
                d.RateUnit
                |> unitGroup
                |> sprintf "%s/%s/%s" ou au

        | DiscontinuousOrder ->
            orbDto.Dose.Quantity.Unit <- ou
            orbDto.Dose.QuantityAdjust.Unit <- $"{ou}/{au}"
            orbDto.Dose.PerTime.Unit <-
                d.FreqUnit
                |> unitGroup
                |> sprintf "%s/%s" ou

        | TimedOrder ->
            orbDto |> setDoseRate
            orbDto.Dose.Rate.Unit <-
                d.RateUnit
                |> unitGroup
                |> sprintf "%s/%s" ou
            orbDto.Dose.RateAdjust.Unit <-
                d.RateUnit
                |> unitGroup
                |> sprintf "%s/%s/%s" ou au

            orbDto.Dose.Quantity.Unit <- ou
            orbDto.Dose.QuantityAdjust.Unit <- $"{ou}/{au}"
            orbDto.Dose.PerTime.Unit <-
                d.FreqUnit
                |> unitGroup
                |> sprintf "%s/%s" ou

        orbDto.Components <-
            [
                for p in d.Products do
                    let cdto = Order.Orderable.Component.Dto.dto d.Id d.Name p.Name p.Shape

                    cdto.ComponentQuantity.Constraints.Vals <- p.Quantities
                    cdto.ComponentQuantity.Unit <- ou

                    cdto.OrderableConcentration.Unit <- "x[Count]"
                    cdto.OrderableQuantity.Constraints.Incr <- [ 1N / p.Divisible ]
                    cdto.OrderableQuantity.Unit <- ou

                    cdto.Dose.Quantity.Constraints.Incr <- [ 1N / p.Divisible ]
                    cdto.Dose.Quantity.Unit <- ou

                    cdto.Items <- [
                        for s in p.Substances do
                            let su = s.Unit |> unitGroup
                            let du = s.Dose.DoseUnit |> unitGroup

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
                                    s.Dose.RateUnit
                                    |> unitGroup
                                    |> sprintf "%s/%s/%s" du au

                                itmDto.Dose.Rate.Constraints <-
                                    itmDto.Dose.Rate.Constraints
                                    |> MinMax.setConstraints s.Dose.NormRate s.Dose.Rate

                                itmDto.Dose.RateAdjust.Constraints <-
                                    itmDto.Dose.RateAdjust.Constraints
                                    |> MinMax.setConstraints s.Dose.NormRateAdjust s.Dose.RateAdjust

                            | DiscontinuousOrder ->
                                itmDto.Dose.Quantity.Unit <- du

                                itmDto.Dose.Quantity.Constraints <-
                                    itmDto.Dose.Quantity.Constraints
                                    |> MinMax.setConstraints s.Dose.NormQuantity s.Dose.Quantity

                                itmDto.Dose.QuantityAdjust.Constraints <-
                                    itmDto.Dose.QuantityAdjust.Constraints
                                    |> MinMax.setConstraints s.Dose.NormQuantityAdjust s.Dose.QuantityAdjust

                                itmDto.Dose.PerTime.Unit <-
                                    s.TimeUnit
                                    |> unitGroup
                                    |> sprintf "%s/%s" du

                                itmDto.Dose.PerTime.Constraints <-
                                    itmDto.Dose.PerTime.Constraints
                                    |> MinMax.setConstraints s.Dose.NormPerTime s.Dose.PerTime

                                itmDto.Dose.PerTimeAdjust.Unit <-
                                    s.TimeUnit
                                    |> unitGroup
                                    |> sprintf "%s/%s/%s" du au

                                itmDto.Dose.PerTimeAdjust.Constraints <-
                                    itmDto.Dose.PerTimeAdjust.Constraints
                                    |> MinMax.setConstraints s.Dose.NormPerTimeAdjust s.Dose.PerTimeAdjust

                            | TimedOrder ->
                                itmDto.Dose.Quantity.Unit <- du

                                itmDto.Dose.Quantity.Constraints <-
                                    itmDto.Dose.Quantity.Constraints
                                    |> MinMax.setConstraints s.Dose.NormQuantity s.Dose.Quantity

                                itmDto.Dose.QuantityAdjust.Constraints <-
                                    itmDto.Dose.QuantityAdjust.Constraints
                                    |> MinMax.setConstraints s.Dose.NormQuantityAdjust s.Dose.QuantityAdjust

                                itmDto.Dose.PerTimeAdjust.Unit <-
                                    s.TimeUnit
                                    |> unitGroup
                                    |> sprintf "%s/%s/%s" du au

                                itmDto.Dose.PerTime.Unit <-
                                    s.TimeUnit
                                    |> unitGroup
                                    |> sprintf "%s/%s" du

                                itmDto.Dose.PerTime.Constraints <-
                                    itmDto.Dose.PerTime.Constraints
                                    |> MinMax.setConstraints s.Dose.NormPerTime s.Dose.PerTime

                                itmDto.Dose.PerTimeAdjust.Constraints <-
                                    itmDto.Dose.PerTimeAdjust.Constraints
                                    |> MinMax.setConstraints s.Dose.NormPerTimeAdjust s.Dose.PerTimeAdjust

                                itmDto.Dose.RateAdjust.Unit <-
                                    s.TimeUnit
                                    |> unitGroup
                                    |> sprintf "%s/%s/%s" du au

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
            $"x[Count]/{(d.FreqUnit |> unitGroup)}"
        dto.Prescription.Frequency.Constraints.Vals <- d.Frequencies

        dto.Adjust.Constraints.Min <- Some (200N /1000N)
        dto.Adjust.Constraints.Max <- Some 150N
        dto.Adjust.Constraints.Vals <-
            d.Adjust
            |> Option.map List.singleton
            |> Option.defaultValue []
        dto.Adjust.Unit <- au

        dto


