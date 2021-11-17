
#r "nuget: Expecto, 9.0.4"
#r "nuget: Expecto.FsCheck, 9.0.4"
#r "nuget: Unquote"

#load "../../../src/Informedica.GenSolver.Lib/Scripts/load.fsx"

#r "nuget: MathNet.Numerics, 4.15"

open Expecto
open Expecto.Logging
open Expecto.Flip

module SU =
    
    open Swensen.Unquote
    let test = test

let run = runTestsWithCLIArgs [ CLIArguments.Verbosity LogLevel.Info ] [||]


open MathNet.Numerics
open Informedica.GenSolver.Lib
open FsCheck


/// Create the necessary test generators
module Generators =

    open FsCheck

    let bigRGen (n, d) = 
            let d = if d = 0 then 1 else d
            let n' = abs(n) |> BigRational.FromInt
            let d' = abs(d) |> BigRational.FromInt
            n'/d'

    let bigRGenerator =
        gen {
            let! n = Arb.generate<int>
            let! d = Arb.generate<int>
            return bigRGen(n, d)
        }

    type MyGenerators () =
        static member BigRational () =
            { new Arbitrary<BigRational>() with
                override x.Generator = bigRGenerator }


[<Tests>]
let tests =

    let config = 
        { FsCheckConfig.defaultConfig with 
            maxTest = 10000 }


    testList "Given an empty set of increments" [
        let act =
            10N
            |> BigRational.maxInclMultipleOf Set.empty
        let exp = (true, 10N)        

        test "any max will remain the same max" {
            Expect.equal "always true" exp act
        }   

        testPropertyWithConfig config "with any max" <| fun (m : BigRational) ->
            m
            |> BigRational.maxInclMultipleOf Set.empty
            |> Expect.equal "should return the same" (true, m)
    ]


tests
|> run

