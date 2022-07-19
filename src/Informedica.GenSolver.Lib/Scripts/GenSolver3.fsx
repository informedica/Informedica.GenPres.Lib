
#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: FParsec"

#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "nuget: Unquote"

#r "../../Informedica.Utils.Lib/bin/Debug/net5.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.GenUnits.Lib/bin/Debug/net5.0/Informedica.GenUnits.Lib.dll"

#load "../Utils.fs"
#load "../Types.fs"
#load "../Logging.fs"
#load "../Variable.fs"
#load "../Equation.fs"
#load "../Solver.fs"
#load "../Constraint.fs"
#load "../Api.fs"
#load "../SolverLogging.fs"

#time


open Informedica.GenSolver.Lib


/// Create the necessary test generators
module Generators =

    open Expecto
    open FsCheck
    open MathNet.Numerics


    let bigRGen (n, d) = 
        let d = if d = 0 then 1 else d
        let n = abs(n) |> BigRational.FromInt
        let d = abs(d) |> BigRational.FromInt
        n/d

    let bigRGenerator =
        gen {
            let! n = Arb.generate<int>
            let! d = Arb.generate<int>
            return bigRGen(n, d)
        }


    type BigRGenerator () =
        static member BigRational () =
            { new Arbitrary<BigRational>() with
                override x.Generator = bigRGenerator
            }


    let config = { 
        FsCheckConfig.defaultConfig with 
            arbitrary = [typeof<BigRGenerator>]
            maxTest = 1000
        }


    let testProp testName prop =
        prop |> testPropertyWithConfig config testName


    let run = runTestsWithCLIArgs [] [|"--summary" |]


module Tests =

    open MathNet.Numerics
    open Expecto
    open Informedica.GenSolver.Lib


    module UtilsTests =


        module BigRationalTests =

            [<Tests>]
            let tests = testList "bigrational" [

                testList "multiples" [
                    fun incr min ->
                        let mult = min |> BigRational.toMinMultipleOf incr

                        if mult >= min then
                            true
                        else
                            printfn $"||{mult} >= {min}?||"
                            false
                    |> Generators.testProp
                        $"multiple of incr should be >= min"

                    fun incr min ->
                        let mult =
                            min |> BigRational.minInclMultipleOf incr |> snd

                        if mult >= min then
                            true
                        else
                            printfn $"||{mult} >= {min}?||"
                            false
                    |> Generators.testProp
                        "multiple incrs should be >= min incl"

                    fun incr min ->
                        let incr = incr |> Set.filter ((<) 0N)
                        let mult =
                            min |> BigRational.minExclMultipleOf incr |> snd
                        if incr |> Set.count = 0 || mult > min then
                            true
                        else
                            printfn $"||{mult} > {min}?||"
                            false
                    |> Generators.testProp
                        "multiple incrs should be > min excl"

                    fun incr max ->
                        let mult = max |> BigRational.toMaxMultipleOf incr
                        if mult <= max then
                            true
                        else
                            printfn $"||{mult} <= {max}?||"
                            false
                    |> Generators.testProp
                        $"multiple of incr should be <= max"

                    fun incr max ->
                        let mult =
                            max |> BigRational.maxInclMultipleOf incr |> snd
                        if mult <= max then
                            true
                        else
                            printfn $"||{mult} <= {max}?||"
                            false
                    |> Generators.testProp
                        "multiple incrs should be <= max incl"

                    fun incr max ->
                        let incr = incr |> Set.filter ((<) 0N)
                        let mult =
                            max |> BigRational.maxExclMultipleOf incr |> snd
                        if incr |> Set.count = 0 || mult < max then
                            true
                        else
                            printfn $"||{mult} < {max}?||"
                            false
                    |> Generators.testProp
                        "multiple incrs should be < max excl"
                ]
            ]

            let run () = tests |> Generators.run


        module SetTests =

            [<Tests>]
            let tests = testList "set" [
                testList "remove multiples" [
                    fun xs ->
                        let result =
                            xs
                            |> Set.filter ((<) 0N)
                            |> Set.removeBigRationalMultiples
                        Seq.allPairs result result
                        |> Seq.forall (fun (x1, x2) ->
                            if x1 = x2 then true
                            else
                                (x1 / x2).Denominator <> 1I &&
                                (x2 / x1).Denominator <> 1I
                        )
                    |> Generators.testProp "no multiples"
                ]
            ]


            let run () = tests |> Generators.run


        module ListTests =


            let testReplace () =
                [ 1..10 ] |> List.replace (fun x -> x % 2 = 0) 0


            let testReplaceOrAdd () = [ 1..10 ] |> List.replaceOrAdd ((=) 11) 0


            [<Tests>]
            let tests =
                testList "list" [
                    testList "replace" [
                        fun xs ->
                            let isEven x = x % 2 = 0
                            let even = xs |> List.replace isEven 2

                            (xs |> List.filter isEven |> List.length) = (even |> List.filter ((=) 2) |> List.length)
                        |> testProperty
                            "replace count should equel predicate count"
                    ]
                ]


            let run () = tests |> Generators.run


    module VariableTests =


        module ValueRangeTests =

            
            module MinimumTests =
            
                open Variable.ValueRange.Minimum
            
                let tests =
                    testList "minimum" [
            
                        fun b m1 m2 ->
                            let min1 = create b m1
                            let min2 = create b m2
                            m1 > m2 = (min1 |> minGTmin min2)
                        |> Generators.testProp "min1 > min2"
            
                        fun b m1 m2 ->
                            let min1 = create b m1
                            let min2 = create b m2
                            m1 < m2 = (min1 |> minSTmin min2)
                        |> Generators.testProp "min1 < min2"
            
                        fun m1 m2 ->
                            let min1 = create true m1
                            let min2 = create false m2
                            (m1 = m2 || m1 < m2) = (min1 |> minSTmin min2)
                        |> Generators.testProp
                            "min1 incl < min2 excl, also when min1 = min2"
            
                        fun m1 m2 ->
                            let min1 = create false m1
                            let min2 = create true m2
                            m1 < m2 = (min1 |> minSTmin min2)
                        |> Generators.testProp "min1 excl < min2 incl"
            
                        fun b m1 m2 ->
                            let min1 = create b m1
                            let min2 = create b m2
                            m1 >= m2 = (min1 |> minGTEmin min2)
                        |> Generators.testProp "min1 >= min2"
            
                        fun b m1 m2 ->
                            let min1 = create b m1
                            let min2 = create b m2
                            m1 <= m2 = (min1 |> minSTEmin min2)
                        |> Generators.testProp "min1 <= min2"
            
                        fun m1 m2 ->
                            let min1 = create true m1
                            let min2 = create false m2
                            m1 > m2 = (min1 |> minGTmin min2)
                        |> Generators.testProp "min1 incl > min2 excl"
            
                        fun m1 m2 ->
                            let min1 = create false m1
                            let min2 = create true m2
                            (m1 = m2 || m1 > m2) = (min1 |> minGTmin min2)
                        |> Generators.testProp
                            "min1 excl > min2 incl, also when min1 = min2"
            
                        fun b m ->
                            let min = create b m
                            min
                            |> toBoolBigRational
                            |> fun (b, m) -> create b m = min
                        |> Generators.testProp
                            "construct and deconstruct min there and back again"
            
                        fun b m ->
                            let incr = set [1N/3N; 1N/4N; 1N/5N] |> Set.removeBigRationalMultiples
                            let min0 = create b m
                            let min1 = min0 |> multipleOf incr
                            let min2 = min1 |> multipleOf incr
                            if min0 <> min1 then
                                min1 |> minGTmin min0 &&
                                min1 = min2
                            else true
                        |> Generators.testProp "multipleOf run multiple times returns identical"
                    ]
            
                let run () = tests |> Generators.run
            


            module MaximumTests =

                open Variable.ValueRange.Maximum

                let tests =
                    testList "maximum" [
                        fun b m1 m2 ->
                            let max1 = create b m1
                            let max2 = create b m2
                            m1 > m2 = (max1 |> maxGTmax max2)
                        |> Generators.testProp "max1 > max2"

                        fun b m1 m2 ->
                            let max1 = create b m1
                            let max2 = create b m2
                            m1 < m2 = (max1 |> maxSTmax max2)
                        |> Generators.testProp "max1 < max2"

                        fun m1 m2 ->
                            let max1 = create false m1
                            let max2 = create true m2
                            (m1 = m2 || m1 < m2) = (max1 |> maxSTmax max2)
                        |> Generators.testProp
                            "max1 excl < max2 incl, also when max1 = max2"

                        fun m1 m2 ->
                            let max1 = create true m1
                            let max2 = create false m2
                            m1 < m2 = (max1 |> maxSTmax max2)
                        |> Generators.testProp "max1 incl < max2 excl"

                        fun b m1 m2 ->
                            let max1 = create b m1
                            let max2 = create b m2
                            m1 >= m2 = (max1 |> maxGTEmax max2)
                        |> Generators.testProp "max1 >= max2"

                        fun b m1 m2 ->
                            let max1 = create b m1
                            let max2 = create b m2
                            m1 <= m2 = (max1 |> maxSTEmax max2)
                        |> Generators.testProp "max1 <= max2"

                        fun m1 m2 ->
                            let max1 = create false m1
                            let max2 = create true m2
                            m1 > m2 = (max1 |> maxGTmax max2)
                        |> Generators.testProp "max1 excl > max2 incl"

                        fun m1 m2 ->
                            let max1 = create true m1
                            let max2 = create false m2
                            (m1 = m2 || m1 > m2) = (max1 |> maxGTmax max2)
                        |> Generators.testProp
                            "max1 incl > max2 excl, also when max1 = max2"

                        fun b m ->
                            let max = create b m
                            max
                            |> toBoolBigRational
                            |> fun (b, m) -> create b m = max
                        |> Generators.testProp
                            "construct and deconstruct max there and back again"

                        fun b m ->
                            let incr = set [1N/3N; 1N/4N; 1N/5N] |> Set.removeBigRationalMultiples
                            let max0 = create b m
                            let max1 = max0 |> multipleOf incr
                            let max2 = max1 |> multipleOf incr
                            if max0 <> max1 then
                                max1 |> maxSTmax max0 &&
                                max1 = max2
                            else true
                        |> Generators.testProp "multipleOf run multiple times returns identical"
                    ]


                let run () = tests |> Generators.run



            module IncrementTests =

                open Types
                open Variable.ValueRange.Increment


                let validIncr (Increment s) =
                    s |> Set.isEmpty |> not &&
                    s |> Set.contains 0N |> not &&
                    s |> Set.removeBigRationalMultiples
                      |> fun s2 -> s2 |> Set.count = (s |> Set.count)

                [<Tests>]
                let tests = testList "increment" [
                    testList "create" [
                        fun xs ->
                            try
                                xs |> create |> validIncr
                            with
                            | _ ->
                                if xs |> Set.isEmpty then true
                                else
                                    xs |> Increment |> validIncr |> not
                        |> Generators.testProp "only valid incrs can be created"
                    ]

                    testList "calcOp" [
                        fun xs ->
                            try
                                let incr1 = xs |> create |> Some
                                let incr2 = set [1N] |> create |> Some
                                match calcOpt (*) incr1 incr2 with
                                | Some result -> Some result = incr1
                                | None -> false
                            with
                            | _ -> true
                        |> Generators.testProp "calc mult with one gives identical result"

                        fun xs ->
                            try
                                let incr1 = xs |> create |> Some
                                let incr2 = set [1N] |> create |> Some
                                match calcOpt (+) incr1 incr2 with
                                | Some (Increment res) ->
                                    xs
                                    |> Set.filter ((<) 0N)
                                    |> Set.forall (fun x1 ->
                                        res
                                        |> Set.forall (fun x2 -> x2 <= x1)
                                    )
                                | None -> false
                            with
                            | _ -> true
                        |> Generators.testProp "calc add with one gives gcd which is <= original incr"

                    ]
                ]


                let run () = tests |> Generators.run


        

open Tests
open UtilsTests
open VariableTests
open ValueRangeTests


BigRationalTests.run ()
ListTests.run()
SetTests.run()
MinimumTests.run()
MaximumTests.run()
IncrementTests.run()

open MathNet.Numerics
open Informedica.Utils.Lib.BCL

let incr = set [1N/3N; 1N/4N; 1N/5N] |> Set.removeBigRationalMultiples
let min = Variable.ValueRange.Minimum.create false 0N
min |> Variable.ValueRange.Minimum.multipleOf incr
    |> fun min1 -> min1 |> Variable.ValueRange.Minimum.minGTmin min

(1N/2N) |> BigRational.gcd 1N

set [1N] |> Set.removeBigRationalMultiples

let testMinGTMax () =
    let min = Variable.ValueRange.Minimum.create false 1N
    let max = Variable.ValueRange.Maximum.create false 1N

    min |> Variable.ValueRange.minGTmax max
testMinGTMax ()
