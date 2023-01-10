

#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: Aether"
#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "nuget: Unquote"

#time

open Expecto
open Expecto.Logging
open Expecto.Flip
open MathNet.Numerics


[<AutoOpen>]
module Types =

    open System


    type VenousAccess =
        | Peripheral
        | Central
        | AnyLocation
        | UnknownLocation


    type EnteralAccess =
        | Oral
        | Gastric
        | Duodenal
        | AnyAccess
        | UnknownAccess


    type Gender = Male | Female | AnyGender | UnknownGender


    type Age =
        {
            Years: int
            Months: int
            Weeks: int
            Days: int
        }


    type BirthDate = { Year : int; Month : int option; Day : int option }


    type WeightValue = | Kilogram of decimal | Gram of int


    type HeightValue = | Meter of decimal | Centimeter of int


    type Weight =
        {
            Actual : WeightAtDate list
            Calculation : WeightAtDate option
            Birth : WeightAtDate option
            Admission : WeightAtDate list
        }
    and WeightAtDate = { Date : DateTime; Weight : WeightValue }


    type EstimatedWeight = { Weight: WeightValue; SD : WeightValue }


    type Height =
        {
            Actual : HeightAtDate list
            Birth : HeightAtDate option
        }
    and HeightAtDate = { Date : DateTime; Height : HeightValue }


    type  EstimatedHeight = { Height : HeightValue; SD : HeightValue }


    type Patient =
        {
            Department : string option
            Diagnoses : string []
            Gender : Gender
            Age : Age
            Weight : Weight
            Height : Height
            GestationalAge : AgeWeeksDays option
            PostMenstrualAge : AgeWeeksDays option
            EnteralAccess : EnteralAccess
            VenousAccess : VenousAccess
        }
    and AgeWeeksDays = { Weeks: int; Days : int }



module Generators =

    open FsCheck

    let createBigR (n, d) =
        if d = 0I then None
        else
            (n |> BigRational.FromBigInt) / (d |> BigRational.FromBigInt)
            |> Some


    let bigRGen =
        gen {
            let! n = Arb.generate<bigint>
            let! d = Arb.generate<bigint>
            return createBigR(n, d)
        }


    type BigR = BigR of BigRational

    let bigRArb () =
        bigRGen
        |> Gen.filter Option.isSome
        |> Gen.map Option.get
        |> Arb.fromGen
        |> Arb.convert BigR (fun (BigR br) -> br)


    type MinMax = MinMax of BigRational * BigRational

    let minMaxArb () =
        bigRGen
        |> Gen.filter Option.isSome
        |> Gen.map (Option.get >> abs)
        |> Gen.two
        |> Gen.map (fun (br1, br2) ->
            let br1 = br1.Numerator |> BigRational.FromBigInt
            let br2 = br2.Numerator |> BigRational.FromBigInt
            if br1 >= br2 then br2, br1 else br1, br2
            |> fun (br1, br2) ->
                if br1 = br2 then br1, br2 + 1N else br1, br2
        )
        |> Arb.fromGen
        |> Arb.convert MinMax (fun (MinMax (min, max)) -> min, max)


    let addToConfig config =
        {
            config with
                maxTest = 1000
                arbitrary = typeof<BigR>.DeclaringType::config.arbitrary
        }


open FsCheck
open Swensen.Unquote


Generators.bigRGen
//|> Gen.filter (fun br -> br > 0N)
|> Gen.sample 100 100


module Tests =

    type MarkMe = interface end

    let testBigR n =
        let (Generators.BigR n) = n
        try
            test <@ n > 0N @>
            true
        with
        | e ->
            printfn $"{e}"
            false


let config =
    { FsCheck.Config.Default with
        MaxTest = 10000
        Arbitrary =
            typeof<Generators.BigR>.DeclaringType::FsCheck.Config.Default.Arbitrary

    }



FsCheck.Check.All(config, typeof<Tests.MarkMe>.DeclaringType)




