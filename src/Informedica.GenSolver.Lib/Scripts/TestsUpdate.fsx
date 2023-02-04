
#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: FParsec"

#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "nuget: Unquote"

#r "../../Informedica.Utils.Lib/bin/Debug/net6.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.GenUnits.Lib/bin/Debug/net6.0/Informedica.GenUnits.Lib.dll"


#load "ValueUnitUpdate.fsx"


#time


open System
open System.IO

open Informedica.Utils.Lib



Environment.CurrentDirectory <- __SOURCE_DIRECTORY__




/// Create the necessary test generators
module Generators =

    open Expecto
    open FsCheck
    open MathNet.Numerics

    open Informedica.Utils.Lib.BCL


    let bigRGen (n, d) =
        let d = if d = 0 then 1 else d
        let n = abs(n) |> BigRational.FromInt
        let d = abs(d) |> BigRational.FromInt
        n / d


    let bigRGenOpt (n, d) = bigRGen (n, 1) |> Some


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



    type MinMax = MinMax of BigRational * BigRational
    let minMaxArb () =
        bigRGenerator
        |> Gen.map abs
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


    type ListOf37<'a> = ListOf37 of 'a List
    let listOf37Arb () =
        Gen.listOfLength 37 Arb.generate
        |> Arb.fromGen
        |> Arb.convert ListOf37 (fun (ListOf37 xs) -> xs)


    let config = {
        FsCheckConfig.defaultConfig with
            arbitrary = [
                typeof<BigRGenerator>
                typeof<ListOf37<_>>.DeclaringType
                typeof<MinMax>.DeclaringType
            ] @ FsCheckConfig.defaultConfig.arbitrary
            maxTest = 1000
        }


    let testProp testName prop =
        prop |> testPropertyWithConfig config testName



module Expecto =

    open Expecto

    let run = runTestsWithCLIArgs [] [| "--summary" |]



open ValueUnitUpdate



module Tests =


    open MathNet.Numerics
    open Expecto
    open Expecto.Flip

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL
    open Informedica.GenUnits.Lib



    module UtilsTests =


        module ArrayTests =

            let tests = testList "Array" [
                testList "remove multiples" [
                    fun xs ->
                        let result =
                            xs
                            |> Array.filter ((<) 0N)
                            |> Array.removeBigRationalMultiples
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



    module VariableTests =

        module Units = ValueUnit.Units


        module ValueRangeTests =

            module Minimum = Variable.ValueRange.Minimum
            module Increment = Variable.ValueRange.Increment
            module Maximum = Variable.ValueRange.Maximum


            module IncrementTests =

                let create brs =
                    Units.Count.times
                    |> ValueUnit.withValue brs
                    |> Increment.create

                let validIncr (Increment s) =
                    s |> ValueUnit.isEmpty |> not &&
                    s |> ValueUnit.gtZero &&
                    s |> ValueUnit.removeBigRationalMultiples
                      |> fun s2 ->
                        s2 |> ValueUnit.valueCount = (s |> ValueUnit.valueCount)

                let tests = testList "increment" [
                    testList "create" [
                        fun xs ->
                            try
                                xs |> create |> validIncr
                            with
                            | _ ->
                                if xs |> Array.isEmpty ||
                                   xs |> Array.distinct = [|0N|] then true
                                else
                                    xs |> create |> validIncr |> not
                        |> Generators.testProp "only valid incrs can be created"
                    ]

                    testList "calcOp" [
                        fun xs ->
                            try
                                let incr1 = xs |> create |> Some
                                let incr2 = [|1N|] |> create |> Some
                                match Increment.calcOpt (*) incr1 incr2 with
                                | Some result -> Some result = incr1
                                | None -> false
                            with
                            | _ -> true
                        |> Generators.testProp "calc mult with one gives identical result"

                        fun xs ->
                            try
                                let incr1 = xs |> create |> Some
                                let incr2 = [|1N|] |> create |> Some
                                match Increment.calcOpt (+) incr1 incr2 with
                                | Some (Increment res) ->
                                    xs
                                    |> Array.filter ((<) 0N)
                                    |> Array.forall (fun x1 ->
                                        res
                                        |> ValueUnit.getValue
                                        |> Array.forall (fun x2 -> x2 <= x1)
                                    )
                                | None -> false
                            with
                            | _ -> true
                        |> Generators.testProp "calc add with one gives gcd which is <= original incr"

                    ]

                    testList "restrict increment" [
                        fun xs ->
                            try
                                let newIncr = xs |> create
                                let oldIncr = xs |> create
                                (oldIncr |> Increment.restrict newIncr) = newIncr
                            with
                            | _ -> true
                        |> Generators.testProp "setting an incr with eq incr"

                        fun xs1 xs2 ->
                            try
                                let newIncr = xs1 |> create
                                let oldIncr = xs2 |> create
                                (oldIncr |> Increment.restrict newIncr |> Increment.count) <=
                                (newIncr |> Increment.count)
                            with
                            | _ -> true
                        |> Generators.testProp "setting an incr with different incr"

                    ]

                ]


            module MinimumTests =


                let create isIncl br =
                    Units.Count.times
                    |> ValueUnit.withSingleValue br
                    |> Minimum.create isIncl


                let tests =
                    testList "minimum" [

                        fun b m1 m2 ->
                            let min1 = create b m1
                            let min2 = create b m2
                            m1 > m2 = (min1 |> Minimum.minGTmin min2)
                        |> Generators.testProp "min1 > min2"

                        fun b m1 m2 ->
                            let min1 = create b m1
                            let min2 = create b m2
                            m1 < m2 = (min1 |> Minimum.minSTmin min2)
                        |> Generators.testProp "min1 < min2"

                        fun m1 m2 ->
                            let min1 = create true m1
                            let min2 = create false m2
                            (m1 = m2 || m1 < m2) = (min1 |> Minimum.minSTmin min2)
                        |> Generators.testProp
                            "min1 incl < min2 excl, also when min1 = min2"

                        fun m1 m2 ->
                            let min1 = create false m1
                            let min2 = create true m2
                            m1 < m2 = (min1 |> Minimum.minSTmin min2)
                        |> Generators.testProp "min1 excl < min2 incl"

                        fun b m1 m2 ->
                            let min1 = create b m1
                            let min2 = create b m2
                            m1 >= m2 = (min1 |> Minimum.minGTEmin min2)
                        |> Generators.testProp "min1 >= min2"

                        fun b m1 m2 ->
                            let min1 = create b m1
                            let min2 = create b m2
                            m1 <= m2 = (min1 |> Minimum.minSTEmin min2)
                        |> Generators.testProp "min1 <= min2"

                        fun m1 m2 ->
                            let min1 = create true m1
                            let min2 = create false m2
                            m1 > m2 = (min1 |> Minimum.minGTmin min2)
                        |> Generators.testProp "min1 incl > min2 excl"

                        fun m1 m2 ->
                            let min1 = create false m1
                            let min2 = create true m2
                            (m1 = m2 || m1 > m2) = (min1 |> Minimum.minGTmin min2)
                        |> Generators.testProp
                            "min1 excl > min2 incl, also when min1 = min2"

                        fun b m ->
                            let min = create b m
                            min
                            |> Minimum.toBoolValueUnit
                            |> fun (b, m) -> Minimum.create b m = min
                        |> Generators.testProp
                            "construct and deconstruct min there and back again"

                        test "100 mg < 1 g" {
                            let min1 =
                                Units.Mass.milliGram
                                |> ValueUnit.withSingleValue 100N
                                |> Minimum.create true
                            let min2 =
                                Units.Mass.gram
                                |> ValueUnit.withSingleValue 1N
                                |> Minimum.create true

                            min1 |> Minimum.minSTmin min2
                            |> Expect.isTrue "should be true"
                        }

                        let incr =
                            Units.Count.times
                            |> ValueUnit.withValue [| 1N/3N; 1N/4N; 1N/5N |]
                            |> Increment.create

                        // Note: min will always become inclusive!!
                        fun b m ->
                            let min0 = create b m
                            //printfn $"min0: {min0 |> Minimum.toString true}"
                            let min1 = min0 |> Minimum.multipleOf incr
                            //printfn $"min1: {min1 |> Minimum.toString true}"
                            let min2 = min1 |> Minimum.multipleOf incr
                            //printfn $"min2: {min2|> Minimum.toString true}"
                            if min0 <> min1 then
                                min1 |> Minimum.minGTmin min0 &&
                                min1 = min2
                            else true
                        |> Generators.testProp "multipleOf run multiple times returns identical"

                        fun b m ->
                            let oldMin = create b m
                            let newMin = create b m
                            oldMin |> Minimum.restrict newMin = oldMin
                        |> Generators.testProp "restrict eq min"


                        fun b1 m1 b2 m2 ->
                            let oldMin = create b1 m1
                            let newMin = create b2 m2
                            oldMin |> Minimum.restrict newMin |> Minimum.minGTEmin oldMin
                        |> Generators.testProp "restrict different min"

                    ]


            module MaximumTests =


                let create isIncl br =
                    Units.Count.times
                    |> ValueUnit.withSingleValue br
                    |> Maximum.create isIncl


                let tests =
                    testList "maximum" [
                        fun b m1 m2 ->
                            let max1 = create b m1
                            let max2 = create b m2
                            m1 > m2 = (max1 |> Maximum.maxGTmax max2)
                        |> Generators.testProp "max1 > max2"

                        fun b m1 m2 ->
                            let max1 = create b m1
                            let max2 = create b m2
                            m1 < m2 = (max1 |> Maximum.maxSTmax max2)
                        |> Generators.testProp "max1 < max2"

                        fun m1 m2 ->
                            let max1 = create false m1
                            let max2 = create true m2
                            (m1 = m2 || m1 < m2) = (max1 |> Maximum.maxSTmax max2)
                        |> Generators.testProp
                            "max1 excl < max2 incl, also when max1 = max2"

                        fun m1 m2 ->
                            let max1 = create true m1
                            let max2 = create false m2
                            m1 < m2 = (max1 |> Maximum.maxSTmax max2)
                        |> Generators.testProp "max1 incl < max2 excl"

                        fun b m1 m2 ->
                            let max1 = create b m1
                            let max2 = create b m2
                            m1 >= m2 = (max1 |> Maximum.maxGTEmax max2)
                        |> Generators.testProp "max1 >= max2"

                        fun b m1 m2 ->
                            let max1 = create b m1
                            let max2 = create b m2
                            m1 <= m2 = (max1 |> Maximum.maxSTEmax max2)
                        |> Generators.testProp "max1 <= max2"

                        fun m1 m2 ->
                            let max1 = create false m1
                            let max2 = create true m2
                            m1 > m2 = (max1 |> Maximum.maxGTmax max2)
                        |> Generators.testProp "max1 excl > max2 incl"

                        fun m1 m2 ->
                            let max1 = create true m1
                            let max2 = create false m2
                            (m1 = m2 || m1 > m2) = (max1 |> Maximum.maxGTmax max2)
                        |> Generators.testProp
                            "max1 incl > max2 excl, also when max1 = max2"

                        test "100 mg < 1 g" {
                            let min1 =
                                Units.Mass.milliGram
                                |> ValueUnit.withSingleValue 100N
                                |> Maximum.create true
                            let min2 =
                                Units.Mass.gram
                                |> ValueUnit.withSingleValue 1N
                                |> Maximum.create true

                            min1 |> Maximum.maxSTmax min2
                            |> Expect.isTrue "should be true"
                        }

                        fun b m ->
                            let max = create b m
                            max
                            |> Maximum.toBoolValueUnit
                            |> fun (b, m) -> Maximum.create b m = max
                        |> Generators.testProp
                            "construct and deconstruct max there and back again"

                        let incr =
                            Units.Count.times
                            |> ValueUnit.withValue [| 1N/3N; 1N/4N; 1N/5N |]
                            |> Increment.create

                        fun b m ->
                            let max0 = create b m
                            let max1 = max0 |> Maximum.multipleOf incr
                            let max2 = max1 |> Maximum.multipleOf incr
                            if max0 <> max1 then
                                max1 |> Maximum.maxSTmax max0 &&
                                max1 = max2
                            else true
                        |> Generators.testProp "multipleOf run multiple times returns identical"

                        fun b m ->
                            let oldMax = create b m
                            let newMax = create b m
                            oldMax
                            |> Maximum.restrict newMax = oldMax
                        |> Generators.testProp "restrict eq max"


                        fun b1 m1 b2 m2 ->
                            let oldMax = create b1 m1
                            let newMax = create b2 m2
                            oldMax
                            |> Maximum.restrict newMax
                            |> Maximum.maxSTEmax oldMax
                        |> Generators.testProp "restrict different max"

                    ]



            module ValueRange = Variable.ValueRange


            let createMin isIncl br =
                Units.Count.times
                |> ValueUnit.withSingleValue br
                |> Minimum.create isIncl

            let createMax isIncl br =
                Units.Count.times
                |> ValueUnit.withSingleValue br
                |> Maximum.create isIncl


            let tests = testList "valuerange" [


                testList "between min and max" [
                    fun bMin minV bMax maxV v ->
                        let min = createMin bMin minV |> Some
                        let max = createMax bMax maxV |> Some
                        let op1 = if bMin then (<=) else (<)
                        let op2 = if bMax then (<=) else (<)
                        (minV |> op1 <| v && v |> op2 <| maxV) = (v |> ValueRange.isBetweenMinMax min max)
                    |> Generators.testProp "v between min and max"

                    fun bMin minV v ->
                        let min = createMin bMin minV |> Some
                        let max = None
                        let op1 = if bMin then (<=) else (<)
                        (minV |> op1 <| v) = (v |> ValueRange.isBetweenMinMax min max)
                    |> Generators.testProp "v between min and none"

                    fun bMax maxV v ->
                        let min = None
                        let max = createMax bMax maxV |> Some
                        let op2 = if bMax then (<=) else (<)
                        (v |> op2 <| maxV) = (v |> ValueRange.isBetweenMinMax min max)
                    |> Generators.testProp "v between none and max"

                    fun v ->
                        let min = None
                        let max = None
                        v |> ValueRange.isBetweenMinMax min max
                    |> Generators.testProp "v between none and none"
                ]

                testList "is multiple of"  [
                    fun v xs ->
                        try
                            let incr = xs |> IncrementTests.create
                            let isMult =
                                xs
                                |> Array.exists (fun i -> (v / i).Denominator = 1I)
                            v |> ValueRange.isMultipleOfIncr (Some incr) = isMult
                        with
                        | _ -> true
                    |> Generators.testProp "v is multiple of one of incr"

                    fun v ->
                        v |> ValueRange.isMultipleOfIncr None
                    |> Generators.testProp "is always multiple of none incr"
                ]
            ]



    [<Tests>]
    let tests =
        [
            //UtilsTests.tests
            //VariableTests.tests
            //EquationTests.tests
            UtilsTests.ArrayTests.tests
            VariableTests.ValueRangeTests.IncrementTests.tests
            VariableTests.ValueRangeTests.MinimumTests.tests
            VariableTests.ValueRangeTests.MaximumTests.tests
            VariableTests.ValueRangeTests.tests

        ]
        |> testList "GenSolver"




Tests.tests
|> Expecto.run


open MathNet.Numerics
open Informedica.GenUnits.Lib

module Increment = ValueUnitUpdate.Variable.ValueRange.Increment

let create = Tests.VariableTests.ValueRangeTests.IncrementTests.create

let newIncr = [|3N|]  |> create
let oldIncr = [|2N; 3N|] |> create


