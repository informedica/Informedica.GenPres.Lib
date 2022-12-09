

#load "load.fsx"

#time


fsi.AddPrinter<System.DateTime> (fun dt -> dt.ToShortDateString())


open Informedica.GenOrder.Lib
open MathNet.Numerics


let gentaDto =
    // gentamicin
    {
        DrugOrder.drugOrder with
            Id = "1"
            Name = "gentamicin"
            Quantities = [ ]
            Unit = "ml"
            TimeUnit = "day"
            Route = "iv"

            Products =
                [
                    {
                        DrugOrder.productComponent with
                            Name = "gentamicin"
                            Shape = "infusion fluid"
                            Quantities = [ 2N; 10N ]
                            Divisible = 1N
                            TimeUnit = "day"
                            Substances =
                                [
                                    {
                                        DrugOrder.substanceItem with
                                            Name = "gentamicin"
                                            Concentrations = [ 10N; 40N ]
                                            Unit = "mg"
                                            DoseUnit = "mg"
                                            TimeUnit = "day"
                                    }
                                ]

                    }
                    {
                        DrugOrder.productComponent with
                            Name = "saline"
                            Shape = "infusion fluid"
                            Quantities = [ 5000N ]
                            Divisible = 1N
                            TimeUnit = "day"
                            Substances =
                                [
                                    {
                                        DrugOrder.substanceItem with
                                            Name = "sodium"
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
    |> DrugOrder.toOrder




gentaDto
|> Order.Dto.fromDto
|> Order.solveMinMax { Log = ignore }



let pcmDto =
    // gentamicin
    {
        DrugOrder.drugOrder with
            Id = "1"
            Name = "paracetamol"
            Quantities = [ 1N ]
            Unit = "piece"
            TimeUnit = "day"
            Route = "rect"
            Frequencies = [1N; 2N; 3N ; 4N; 6N]
            DoseQuantities = [1N]
            DoseCount = Some 1N
            Products =
                [
                    {
                        DrugOrder.productComponent with
                            Name = "paracetamol"
                            Shape = "supp"
                            Quantities = [ 1N ]
                            Divisible = 1N
                            TimeUnit = "day"
                            Substances =
                                [
                                    {
                                        DrugOrder.substanceItem with
                                            Name = "paracetamol"
                                            Concentrations = [ 60N; 120N; 240N; 500N; 1000N ]
                                            Unit = "mg"
                                            DoseUnit = "mg"
                                            TimeUnit = "day"
                                            Dose =
                                                { DrugOrder.DoseRule.limit  with
                                                    MaxDoseQuantity = Some 1000N
                                                    MaxDosePerTime = Some 4000N
                                                    MaxDosePerTimeAdjust = Some 90N
                                                    MinDosePerTimeAdjust = Some 20N
                                                }
                                    }
                                ]

                    }

                ]
            OrderType = DiscontinuousOrder
        }
    |> DrugOrder.toOrder


pcmDto
|> Order.Dto.fromDto
|> Order.toString
|> List.length


pcmDto
|> Order.Dto.fromDto
|> Order.applyConstraints
|> Order.toString
|> List.iteri (printfn "%i. %s")


let run dto =
    let ord =
        dto
        |> Order.Dto.fromDto
        |> Order.solveMinMax { Log = ignore }
    ord
    |> Order.toString
    |> List.iteri (printfn "%i. %s")

    ord
    |> Order.Dto.toDto


pcmDto
|> run
|> fun dto ->
    dto.Adjust.Constraints.Incr <- [ 1N/10N ]
    dto
    |> run

