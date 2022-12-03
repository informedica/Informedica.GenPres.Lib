
#load "load.fsx"

open MathNet.Numerics

open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

#time

let path = Some $"{__SOURCE_DIRECTORY__}/gentamicin.txt"

let startLogging = fun () -> OrderLogger.logger.Start path OrderLogger.Level.Informative


startLogging ()



// gentamicin
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "gentamicin"
        Quantities = [ ]
        Unit = "ml"
        TimeUnit = "day"
        Shape = "infusion fluid"
        Route = "iv"
        Products =
            [
                {
                    DrugOrder.productComponent with
                        Name = "gentamicin"
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
|> DrugOrder.toConstrainedOrder false
|> DrugOrder.setAdjust "gentamicin" 4N
|> DrugOrder.setDoseRule
    {   DrugOrder.DoseRule.rule with
            Medication = "gentamicin"
            Frequencies = [ 1N ]
            Limits = [
                { DrugOrder.DoseRule.limit with
                    SubstanceName = "gentamicin"
                    MinDoseTotalAdjust = Some 4N
                    MaxDoseTotalAdjust = Some 6N
                }
            ]
    }
|> DrugOrder.setSolutionRule false
    { DrugOrder.SolutionRule.rule with
        Medication = "gentamicin"
        Solutions = ["saline"]
        Quantities = [5N;10N;20N;50N;100N;200N]
        RateUnit = "hour"
        Limits = [
            { DrugOrder.SolutionRule.limit with
                SubstanceName = "gentamicin"
                MinConcentration = Some 1N
                MaxConcentration = Some 2N
            }
        ]

    }
|> DrugOrder.evaluate OrderLogger.logger.Logger
|> List.map (Order.printPrescription ["gentamicin"])

