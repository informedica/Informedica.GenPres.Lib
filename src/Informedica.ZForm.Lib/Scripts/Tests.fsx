

#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "nuget: Unquote"



#load "../../../scripts/Expecto.fsx"


#load "load.fsx"


module Tests =

    open Expecto
    open Expecto.Flip

    open Informedica.GenCore.Lib.Ranges
    open Informedica.ZForm.Lib


    module MinIncrMaxTests =

        open MathNet.Numerics

        open Informedica.Utils.Lib.BCL
        open Informedica.GenUnits.Lib
        open Informedica.GenCore.Lib
        open Informedica.GenCore.Lib.Ranges



        let fromDecimal (v: decimal) u =
            v
            |> BigRational.fromDecimal
            |> ValueUnit.createSingle u


        let ageInMo =  (fun n -> fromDecimal n ValueUnit.Units.Time.month)


        let ageInYr =  (fun n -> fromDecimal n ValueUnit.Units.Time.year)



        let ageInclOneMo, ageExclOneYr =
            1m |> ageInMo |> Inclusive,
            1m |> ageInYr |> Exclusive


        let ageRange =
            MinIncrMax.empty
            |> MinIncrMax.Optics.setMin ageInclOneMo
            |> MinIncrMax.Optics.setMax ageExclOneYr


        let tests = testList "MinIncrMax" [
            test "ageToString" {
                ageRange
                |> MinIncrMax.ageToString
                |> Expect.equal "should equal" "van 1 mnd - tot 1 jr"
            }
        ]


    module PatientTests =

        open PatientCategory

        let processDto f dto =
            dto |> f
            dto

        let setMinAge = 
            fun (dto : Dto.Dto) ->
                dto.Age.HasMin <- true
                dto.Age.Min.Value <- [|1m|]
                dto.Age.Min.Unit <- "maand"
                dto.Age.Min.Group <- "Time"
                dto.Age.Min.Language <- "dutch"
                dto.Age.Min.Short <- true
                dto.Age.MinIncl <- true


        let setWrongUnit =        
            fun (dto : Dto.Dto) ->
                // group defaults to general when no unit can be found in group
                // ToDo: need to fix this behaviour
                dto.Age.HasMin <- true
                dto.Age.Min.Value <- [|1m|]
                dto.Age.Min.Unit <- "m"
                dto.Age.Min.Group <- "Time"
                dto.Age.Min.Language <- "dutch"
                dto.Age.Min.Short <- true
                dto.Age.MinIncl <- true

        let setWrongGroup =
            fun (dto : Dto.Dto) ->
                // need to check for the correct units
                // ToDo!!
                dto.Age.HasMin <- true
                dto.Age.Min.Value <- [|1m|]
                dto.Age.Min.Unit <- "g"
                dto.Age.Min.Group <- "Mass"
                dto.Age.Min.Language <- "dutch"
                dto.Age.Min.Short <- true
                dto.Age.MinIncl <- true



        let tests = testList "Patient" [

            test "an 'empty patient'" {
                Dto.dto ()
                |> Dto.fromDto
                |> function
                | None -> "false"
                | Some p -> 
                    p |> PatientCategory.toString
                |> Expect.equal "should be an empty string" ""    
            }

            test "a patient with a min age" {
                Dto.dto ()
                |> processDto setMinAge
                |> Dto.fromDto
                |> function
                | None -> "false"
                | Some p -> 
                    p |> PatientCategory.toString
                |> Expect.equal "should be 'Leeftijd: van 1 mnd'" "Leeftijd: van 1 mnd"    
            }

            test "a patient with a min age wrong unit" {
                Dto.dto ()
                |> processDto setWrongUnit
                |> Dto.fromDto
                |> function
                | None -> "false"
                | Some p -> 
                    p |> PatientCategory.toString
                |> Expect.equal "should be an empty string" ""    
            }

            test "a patient with a min age wrong group" {
                Dto.dto ()
                |> processDto setWrongGroup
                |> Dto.fromDto
                |> function
                | None -> "false"
                | Some p -> 
                    p |> PatientCategory.toString
                |> Expect.equal "should be an empty string" ""    
            }

        ]



    module DoseRangeTests =

        module Dto = DoseRule.DoseRange.Dto

        let (|>!) x f =
            printfn $"%A{x}"
            x |> f


        let processDto f dto = dto |> f; dto


        let addValues  = 
            fun (dto : Dto.Dto) ->
                dto.Norm.HasMin <- true
                dto.Norm.Min.Value <- [|1m|]
                dto.Norm.Min.Unit <- "mg"
                dto.Norm.Min.Group <- "mass"

                dto.Norm.HasMax <- true
                dto.Norm.Max.Value <- [|10m|]
                dto.Norm.Max.Unit <- "mg"
                dto.Norm.Max.Group <- "mass"

                dto.NormWeight.HasMin <- true
                dto.NormWeight.Min.Value <- [|0.01m|]
                dto.NormWeight.Min.Unit <- "mg"
                dto.NormWeight.Min.Group <- "mass"
                dto.NormWeightUnit <- "kg"

                dto.NormWeight.HasMax <- true
                dto.NormWeight.Max.Value <- [|1m|]
                dto.NormWeight.Max.Unit <- "mg"
                dto.NormWeight.Max.Group <- "mass"
                dto.NormWeightUnit <- "kg"


        let print () =
            Dto.dto ()
            |> processDto addValues
            |>! Dto.fromDto
            |>! Dto.toDto
            |>! Dto.fromDto
            |>! ignore

        let tests = testList "DoseRange" [
            
            test "there and back again empty doserange dto" {
                let expct =
                    Dto.dto ()
                    |> Dto.fromDto

                expct
                |> Dto.toDto
                |> Dto.fromDto
                |> Expect.equal "should be equal" expct
            }
            
            test "there and back again with filled doserange dto" {
                let expct =
                    Dto.dto ()
                    |> processDto addValues
                    |> Dto.fromDto

                expct
                |> Dto.toDto
                |> Dto.fromDto
                |> Expect.equal "should be equal" expct
            }

        ]



    module DoseRuleTests =
        
        module Dto = DoseRule.Dto


        let (|>!) x f =
            printfn $"%A{x}"
            x |> f



        let processDto f dto = dto |> f; dto


        let print () =

            let dto = Dto.dto ()


            dto
            |>! Dto.fromDto
            |>! ignore

        let tests = testList "DoseRule" [
            test "there and back again with an empty doserule" {
                let doseRule = 
                    Dto.dto ()
                    |> Dto.fromDto
                
                doseRule
                |> Dto.toDto
                |> Dto.fromDto
                |> Expect.equal "should be equal" doseRule

            }
        ]


    let tests =  
        [   
            MinIncrMaxTests.tests
            PatientTests.tests
            DoseRangeTests.tests
            DoseRuleTests.tests
        ]
//        |> List.skip 1
        |> testList "ZForm"


open Expecto


Tests.tests
|> Expecto.run


open Informedica.GenCore.Lib.Ranges
open Informedica.ZForm.Lib


MinIncrMax.empty |> MinIncrMax.ageToString