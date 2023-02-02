

#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: FParsec"

#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "nuget: Unquote"

#r "../../Informedica.Utils.Lib/bin/Debug/net6.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.GenUnits.Lib/bin/Debug/net6.0/Informedica.GenUnits.Lib.dll"

#load "load.fsx"


#time


open System
open System.IO

open Informedica.Utils.Lib
open Informedica.GenSolver.Lib



Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


module Solve =

    module Name = Variable.Name
    module ValueRange = Variable.ValueRange
    module Minimum = ValueRange.Minimum
    module Maximum = ValueRange.Maximum
    module Increment = ValueRange.Increment
    module ValueSet = ValueRange.ValueSet


    let procss s =
        File.AppendAllLines("order.log", [s])
        $"{s} " |> printfn "%s"

    let setProp n p eqs =
        let n = n |> Name.createExc
        match eqs |> Api.setVariableValues true n p with
        | Some var ->
            eqs
            |> List.map (fun e ->
                e |> Equation.replace var
            )
        | None -> eqs

    let setMinIncl n min = min |> Minimum.create true |> MinProp |> setProp n
    let setMinExcl n min = min |> Minimum.create false |> MinProp |> setProp n
    let setMaxIncl n max = max |> Maximum.create true |> MaxProp |> setProp n
    let setMaxExcl n max = max |> Maximum.create true |> MaxProp |> setProp n
    let setValues n vals = vals |> ValueSet.create |> ValsProp |> setProp n


    let printEqs = Solver.printEqs true procss
    let solve n p eqs =
        let logger =
            fun s ->
                File.AppendAllLines("order.log", [s])
            |> SolverLogging.logger
        try
            eqs
            |> Api.solve true Solver.sortQue logger (n |> Name.createExc) p
            |> Result.get
        with
        | :? Exceptions.SolverException as e ->
            printfn $"{e.Data0}"
            raise e

    let solveMinIncl n min = solve n (min |> Minimum.create true |> MinProp)
    let solveMinExcl n min = solve n (min |> Minimum.create false |> MinProp)
    let solveMaxIncl n max = solve n (max |> Maximum.create true |> MaxProp)
    let solveMaxExcl n max = solve n (max |> Maximum.create false |> MaxProp)
    let solveIncr n incr = solve n (set [incr] |> Increment.create |> IncrProp)
    let solveValues n vals = solve n (vals |> ValueSet.create |> ValsProp)

    let init     = Api.init
    let nonZeroNegative = Api.nonZeroNegative


    let findValidValues n (eqs : Equation list) =
        let var =
            eqs
            |> List.collect Equation.toVars
            |> List.tryFind (fun v ->
                v
                |> Variable.getName
                |> Name.toString
                |> fun x -> x = n
            )
            |> Option.get

        match var.Values |> ValueRange.getValSet with
        | None    -> ()
        | Some (ValueSet vs) ->
            for v in vs do
                try
                    eqs
                    |> solveValues n [v]
                    |> ignore
                    printfn $"can set {v}"
                with
                | _ ->
                    printfn $"cannot set {v}"



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


    let varGenerator =
        let mapToBr min max xs =
            xs
            |> List.map (fun (x1, x2) -> x1, 1)
            |> List.map bigRGen
            |> List.filter (fun x ->
                x > 0N &&
                match min, max with
                | None, None -> x > 0N
                | Some min, None -> x > min
                | Some min, Some max -> x > min && x < max
                | None, Some max -> x < max
            ) |> List.distinct

        let minMax min max minIncl maxIncl =
            match min |> bigRGenOpt, max |> bigRGenOpt with
            | Some min, Some max when min > max -> Some max, Some min
            | Some min, Some max when min = max ->
                if minIncl && maxIncl then Some min, Some max
                else None, None
            | min, max -> min, max

        gen {
            let! minIsNone = Arb.generate<bool>
            let! maxIsNone = Arb.generate<bool>
            let! n = Arb.generate<int>
            let! min = Arb.generate<int * int>
            let! max = Arb.generate<int * int>
            let! incr = Arb.generate<(int * int) list>
            let! vs = Arb.generate<(int * int) list>
            let! minIncl = Arb.generate<bool>
            let! maxIncl = Arb.generate<bool>
            let min, max = minMax min max minIncl maxIncl
            let incr = incr |> mapToBr min max
            let vs =
                vs
                |> mapToBr min max
                |> List.filter (fun x ->
                    incr
                    |> List.exists (fun i ->
                        x |> BigRational.isMultiple i
                    )
                )

            return
                Variable.Dto.createDto
                    $"var_{(abs n).ToString()}"
                    (if minIsNone then None else min)
                    minIncl
                    incr
                    (if maxIsNone then None else max)
                    maxIncl
                    vs
        }


    type VarDtoGenerator () =
        static member Variable () =
            { new Arbitrary<Variable.Dto.Dto>() with
                override x.Generator = varGenerator
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
                typeof<VarDtoGenerator>
            ] @ FsCheckConfig.defaultConfig.arbitrary
            maxTest = 1000
        }


    let testProp testName prop =
        prop |> testPropertyWithConfig config testName



module Expecto =

    open Expecto

    let run = runTestsWithCLIArgs [] [| "--summary" |]




module Tests =


    open MathNet.Numerics
    open Expecto

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL


    module UtilsTests =


        module BigRationalTests =

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


        module SetTests =

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

        let tests = testList "Utils" [ BigRationalTests.tests; SetTests.tests; ListTests.tests ]



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

                        fun b m ->
                            let oldMin = create b m
                            let newMin = create b m
                            oldMin |> restrict newMin = oldMin
                        |> Generators.testProp "restrict eq min"


                        fun b1 m1 b2 m2 ->
                            let oldMin = create b1 m1
                            let newMin = create b2 m2
                            oldMin |> restrict newMin |> minGTEmin oldMin
                        |> Generators.testProp "restrict different min"
                    ]


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

                        fun b m ->
                            let oldMax = create b m
                            let newMax = create b m
                            oldMax |> restrict newMax = oldMax
                        |> Generators.testProp "restrict eq max"


                        fun b1 m1 b2 m2 ->
                            let oldMax = create b1 m1
                            let newMax = create b2 m2
                            oldMax |> restrict newMax |> maxSTEmax oldMax
                        |> Generators.testProp "restrict different max"

                    ]


            module IncrementTests =

                open Variable.ValueRange.Increment


                let validIncr (Increment s) =
                    s |> Set.isEmpty |> not &&
                    s |> Set.contains 0N |> not &&
                    s |> Set.removeBigRationalMultiples
                      |> fun s2 -> s2 |> Set.count = (s |> Set.count)

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

                    testList "restrict increment" [
                        fun xs ->
                            try
                                let newIncr = xs |> create
                                let oldIncr = xs |> create
                                (oldIncr |> restrict newIncr) = newIncr
                            with
                            | _ -> true
                        |> Generators.testProp "setting an incr with eq incr"

                        fun xs1 xs2 ->
                            try
                                let newIncr = xs1 |> create
                                let oldIncr = xs2 |> create
                                (oldIncr |> restrict newIncr |> count) <= (newIncr |> count)
                            with
                            | _ -> true
                        |> Generators.testProp "setting an incr with different incr"

                    ]

                ]


            open Variable.ValueRange


            let tests = testList "valuerange" [

                MinimumTests.tests

                MaximumTests.tests

                IncrementTests.tests


                testList "between min and max" [
                    fun bMin minV bMax maxV v ->
                        let min = Minimum.create bMin minV |> Some
                        let max = Maximum.create bMax maxV |> Some
                        let op1 = if bMin then (<=) else (<)
                        let op2 = if bMax then (<=) else (<)
                        (minV |> op1 <| v && v |> op2 <| maxV) = (v |> isBetweenMinMax min max)
                    |> Generators.testProp "v between min and max"

                    fun bMin minV v ->
                        let min = Minimum.create bMin minV |> Some
                        let max = None
                        let op1 = if bMin then (<=) else (<)
                        (minV |> op1 <| v) = (v |> isBetweenMinMax min max)
                    |> Generators.testProp "v between min and none"

                    fun bMax maxV v ->
                        let min = None
                        let max = Maximum.create bMax maxV |> Some
                        let op2 = if bMax then (<=) else (<)
                        (v |> op2 <| maxV) = (v |> isBetweenMinMax min max)
                    |> Generators.testProp "v between none and max"

                    fun v ->
                        let min = None
                        let max = None
                        v |> isBetweenMinMax min max
                    |> Generators.testProp "v between none and none"
                ]

                testList "is multiple of"  [
                    fun v xs ->
                        try
                            let incr = xs |> Increment.create
                            let isMult =
                                xs
                                |> Set.exists (fun i -> (v / i).Denominator = 1I)
                            v |> isMultipleOfIncr (Some incr) = isMult
                        with
                        | _ -> true
                    |> Generators.testProp "v is multiple of one of incr"

                    fun v ->
                        v |> isMultipleOfIncr None
                    |> Generators.testProp "is always multiple of none incr"
                ]
            ]




        open Variable

        let tests = testList "variable dto" [

            ValueRangeTests.tests

            fun (dto: Dto.Dto) ->
                try
                    dto
                    |> Dto.fromDto
                    |> ignore
                    true
                with
                | e ->
                    printfn $"cannot create var from because {e.ToString()}"
                    false
            |> Generators.testProp "can create random"

        ]



    module EquationTests =


        module Equations =


            type Orb =
                {
                    Components : Component list
                }
            and Component =
                {
                    Name : string
                    Items : string list
                }


            let create (orb : Orb) =
                [
                    "itm_cmp_qty = itm_cmp_cnc * cmp_qty"
                    "itm_orb_qty = itm_orb_cnc * orb_qty"
                    "itm_orb_qty = itm_cmp_cnc * cmp_orb_qty"
                    "itm_dos_qty = itm_cmp_cnc * cmp_dos_qty"
                    "itm_dos_qty = itm_orb_cnc * orb_dos_qty"
                    "itm_dos_qty = itm_dos_rte * pres_time"
                    "itm_dos_qty = itm_dos_qty_adj * ord_adj"
                    "itm_dos_tot = itm_cmp_cnc * cmp_dos_tot"
                    "itm_dos_tot = itm_orb_cnc * orb_dos_tot"
                    "itm_dos_tot = itm_dos_qty * pres_freq"
                    "itm_dos_tot = itm_dos_tot_adj * ord_adj"
                    "itm_dos_rte = itm_cmp_cnc * cmp_dos_rte"
                    "itm_dos_rte = itm_orb_cnc * orb_dos_rte"
                    "itm_dos_rte = itm_dos_rte_adj * ord_adj"
                    "itm_dos_ord = itm_dos_tot * ord_time"
                    "itm_dos_ord = itm_dos_rte * ord_time"
                    "itm_dos_qty_adj = itm_cmp_cnc * cmp_dos_qty_adj"
                    "itm_dos_qty_adj = itm_orb_cnc * orb_dos_qty_adj"
                    "itm_dos_qty_adj = itm_dos_rte_adj * pres_time"
                    "itm_dos_tot_adj = itm_cmp_cnc * cmp_dos_tot_adj"
                    "itm_dos_tot_adj = itm_orb_cnc * orb_dos_tot_adj"
                    "itm_dos_tot_adj = itm_dos_qty_adj * pres_freq"
                    "itm_dos_rte_adj = itm_cmp_cnc * cmp_dos_rte_adj"
                    "itm_dos_rte_adj = itm_orb_cnc * orb_dos_rte_adj"
                    "cmp_orb_qty = cmp_orb_cnc * orb_qty"
                    "cmp_orb_qty = cmp_qty * cmp_orb_cnt"
                    "cmp_ord_qty = cmp_qty * cmp_ord_cnt"
                    "cmp_ord_qty = cmp_dos_tot * ord_time"
                    "cmp_ord_qty = cmp_dos_rte * ord_time"
                    "cmp_dos_qty = cmp_orb_cnc * orb_dos_qty"
                    "cmp_dos_qty = cmp_dos_rte * pres_time"
                    "cmp_dos_qty = cmp_dos_qty_adj * ord_adj"
                    "cmp_dos_tot = cmp_orb_cnc * orb_dos_tot"
                    "cmp_dos_tot = cmp_dos_qty * pres_freq"
                    "cmp_dos_tot = cmp_dos_tot_adj * ord_adj"
                    "cmp_dos_rte = cmp_orb_cnc * orb_dos_rte"
                    "cmp_dos_rte = cmp_dos_rte_adj * ord_adj"
                    "cmp_dos_qty_adj = cmp_orb_cnc * orb_dos_qty_adj"
                    "cmp_dos_qty_adj = cmp_dos_rte_adj * pres_time"
                    "cmp_dos_tot_adj = cmp_orb_cnc * orb_dos_tot_adj"
                    "cmp_dos_tot_adj = cmp_dos_qty_adj * pres_freq"
                    "cmp_dos_rte_adj = cmp_orb_cnc * orb_dos_rte_adj"
                    "orb_ord_qty = orb_ord_cnt * orb_qty"
                    "orb_ord_qty = orb_dos_tot * ord_time"
                    "orb_ord_qty = orb_dos_rte * ord_time"
                    "orb_dos_qty = orb_dos_rte * pres_time"
                    "orb_dos_qty = orb_dos_qty_adj * ord_adj"
                    "orb_dos_tot = orb_dos_qty * pres_freq"
                    "orb_dos_tot = orb_dos_tot_adj * ord_adj"
                    "orb_dos_rte = orb_dos_rte_adj * ord_adj"
                    "orb_dos_qty_adj = orb_dos_rte_adj * pres_time"
                    "orb_dos_tot_adj = orb_dos_qty_adj * pres_freq"
                    "orb_qty = orb_qty_adj * ord_adj"
                    // "orb_qty = sum(cmp_orb_qty)"
                    // only add these when single values are used!!
                    // "orb_dos_qty = sum(cmp_dos_qty)"
                    // "orb_dos_tot = sum(cmp_dos_tot)"
                    // "orb_dos_rte = sum(cmp_dos_rte)"
                    // "orb_dos_qty_adj = sum(cmp_dos_qty_adj)"
                    // "orb_dos_tot_adj = sum(cmp_dos_tot_adj)"
                    // "orb_dos_rte_adj = sum(cmp_dos_rte_adj)"
                ]
                |> fun eqs ->
                    let eqs = eqs |> List.map (fun s -> $" {s}")
                    let sumEqs =
                        eqs
                        |> List.filter (fun e ->
                            e.Contains("sum")
                        )
                    let eqs = eqs |> List.filter (fun e -> e.Contains("sum") |> not)
                    let itmEqs =
                        eqs
                        |> List.filter (fun e ->
                            e.Contains("itm")
                        )
                    let cmpEqs =
                        eqs
                        |> List.filter (fun e ->
                            itmEqs
                            |> List.exists ((=) e)
                            |> not &&
                            e.Contains("cmp")
                        )
                    let orbEqs =
                        eqs
                        |> List.filter (fun e ->
                            itmEqs
                            |> List.exists ((=) e)
                            |> not &&
                            cmpEqs
                            |> List.exists((=) e)
                            |> not
                        )

                    orb.Components
                    |> List.fold (fun acc c ->
                        let itms =
                            c.Items
                            |> List.collect (fun i ->
                                itmEqs
                                |> List.map (fun s -> s.Replace(" cmp", $" {c.Name}").Replace(" itm", $" {i}"))
                            )
                        let cmps =
                            cmpEqs
                            |> List.map (fun s1 ->
                                s1.Replace(" cmp", $" {c.Name}")
                            )
                        itms @ cmps @ acc
                    ) []
                    |> fun es ->
                        let sumEqs =
                            sumEqs
                            |> List.map (fun e ->
                                match e.Replace("sum(", "").Replace(")", "").Split(" = ") with
                                | [|lv; rv|] ->
                                    orb.Components
                                    |> List.map(fun c -> rv.Replace("cmp", c.Name))
                                    |> String.concat(" + ")
                                    |> fun s -> $"{lv} = {s}"
                                | _ -> ""
                            )
                        es @ orbEqs @ sumEqs
                |> Api.init



        open Solve
        open Expecto.Flip
        open Equations
        module Name = Informedica.GenSolver.Lib.Variable.Name


        let eqs =
            [
                "a = b + c"
                "d = e * a"
                "d = f * b"
            ]
            |> Api.init


        let twoCompEqs =
            {
                Components =
                    [
                        {
                            Name = "CMPA"
                            Items = ["ITMA"]
                        }
                        {
                            Name = "CMPB"
                            Items = ["ITMB"]
                        }
                    ]
            }
            |> create


        let tests = testList "EquationTests" [
            testList "test setting min and max to abcdef eqs" [

                let config = Generators.config
                let abdef = ["a"; "b"; "d"; "e"; "f"]

                testPropertyWithConfig config "can set any"
                <| fun (i1: int) i2 i3 i4 i5
                    (Generators.MinMax (min1, max1))
                    (Generators.MinMax (min2, max2))
                    (Generators.MinMax (min3, max3))
                    (Generators.MinMax (min4, max4)) ->
                    let x1, x2, x3, x4 =
                        [i1; i2; i3; i4; i5]
                        |> List.mapi (fun i x -> i, x)
                        |> List.sortBy snd
                        |> List.map fst
                        |> function
                        | i1::i2::i3::i4::_ ->
                            (abdef[i1],abdef[i2], abdef[i3], abdef[i4])
                        | _ -> failwith "cannot process"

                    try
            //            printfn $"solving {x1}, {x2}, {x3}, {x4}"
                        eqs
            //            |> nonZeroNegative
                        |> solveMinIncl x1 min1
                        |> solveMaxIncl x1 max1
                        |> solveMinIncl x2 min2
                        |> solveMaxIncl x2 max2
                        |> solveMinIncl x3 min3
                        |> solveMaxIncl x3 max3
                        |> solveMinIncl x4 min4
                        |> solveMaxIncl x4 max4
                        // |> solveMinIncl x5 min5
                        // |> solveMaxIncl x5 max5
                        // |> printEqs
                        |> ignore
                        Expect.isTrue "true is true" true
                    with
                    | :? Exceptions.SolverException as e ->
                        match e.Data0 with
                        | [Exceptions.SolverTooManyLoops (n, xs)] ->
                            printfn "ran into a loop with:"
                            xs
                            |> printEqs
                            |> ignore
                            Expect.isTrue "not true" false
                        | _ ->
                            Expect.isTrue "true is true" true
                    | _ -> Expect.isTrue "true is true" true
            ]

            testList "can set any var of compA or qty or cnc of compB" [
                let config = Generators.config

                let vars =
                    twoCompEqs
                    |> List.collect Equation.toVars
                    |> List.map (fun v -> v.Name |> Name.toString)
                    |> List.distinct
                    |> fun vars ->
                        let cmps =
                            vars
                            |> List.filter (fun s ->
                                s.Contains("CMPB") || s.Contains("ITMB")
                            )
                            |> List.filter (fun s ->
                                s.Contains("cmp_cnc") || s.Contains("CMPB_qty")
                            )
                        vars
                        |> List.filter (fun s ->
                            s.Contains("CMPB") |> not &&
                            s.Contains("ITMB") |> not &&
                            s.Contains("ord") |> not
                        )

                testPropertyWithConfig config "can set any"
                <| fun (Generators.ListOf37 (indxs : int list))
                    (Generators.ListOf37 (xs : Generators.MinMax list))
                    ->
                    let vars =
                        indxs
                        |> List.take vars.Length
                        |> List.mapi (fun i x -> i, x)
                        |> List.sortBy snd
                        |> List.map fst
                        |> List.take 12
                        |> List.map (fun i -> vars[i])

                    try
                        vars
                        |> List.fold (fun acc v ->
                            let i, eqs = acc
                            let eqs =
                                let (Generators.MinMax(min, max)) =
                                    xs[i]
                                eqs
                                |> solveMinIncl v min
                                |> solveMaxIncl v max
                            (i + 1, eqs)
                        ) (0, twoCompEqs)
                        // |> snd
                        // |> printEqs
                        |> ignore
                        Expect.isTrue "true is true" true
                    with
                    | :? Exceptions.SolverException as e ->
                        match e.Data0 with
                        | [Exceptions.SolverTooManyLoops (n, xs)] ->
                            vars
                            |> String.concat ", "
                            |> printfn "ran into a loop with:%s\n"
                            xs |> printEqs |> ignore
                            Expect.isTrue "not true" false
                        | _ ->
                            Expect.isTrue "true is true" true
                    | _ -> Expect.isTrue "true is true" true
            ]

        ]





    [<Tests>]
    let tests =         
        [
            UtilsTests.tests
            VariableTests.tests
            EquationTests.tests

        ]
        |> testList "GenSolver" 




Tests.tests
|> Expecto.run



module Temp =


    open MathNet.Numerics

    open Expecto
    open FsCheck
    open Variable.Operators

    module Property = Variable.ValueRange.Property


    let generateVars n =
        (Generators.varGenerator |> Arb.fromGen).Generator.Sample(10, n)
        |> List.filter (fun dto ->
            dto.Vals |> List.length < 5
        )
        |> List.map Variable.Dto.fromDto
        |> List.filter (fun var ->
            var |> Variable.count <= 5
        )
        |> List.map (fun var ->
            try
                var |> Variable.setNonZeroOrNegative
            with
            | _ -> var
        )
        |> List.distinctBy (fun var ->
            var.Values
        )


    let vars1 = generateVars 100
    let vars2 = generateVars 100
    let vars3 = generateVars 100

    let printTest1 () =
        Seq.allPairs vars1 vars2
        |> Seq.collect (fun (var1, var2) ->
            try
                [
                    var1, var2, var1 @* var2, "x"
                    var1, var2, var1 @/ var2, "/"
                    var1, var2, var1 @+ var2, "+"
                    var1, var2, var1 @- var2, "-"
                ]
            with
            | _ ->
        //        printfn $"cannot calculate {var1}, {var2}"
                []
        )
        |> Seq.distinctBy (fun (x1, x2, _, o) -> set [x1, x2], o)
        |> Seq.iteri (fun i (var1, var2, res, op) ->
            let toStr = Variable.ValueRange.toString false
            let x = var1.Values |> toStr
            let z = var2.Values |> toStr
            let y = res.Values |> toStr
            printfn $@"{i}. {y} = {x} {op} {z}"
        )


    let printTest2 () =
        Seq.allPairs vars1 vars2
        |> Seq.allPairs vars3
        |> Seq.distinct
        |> Seq.collect (fun (y, (x1, x2)) ->
            try
                [
                    y, y @<- (x1 ^* x2), x1, x2, "x"
                    y, y @<- (x1 ^/ x2), x1, x2, "/"
                    y, y @<- (x1 ^+ x2), x1, x2, "+"
                    y, y @<- (x1 ^- x2), x1, x2, "-"
                ]
            with
            | _ -> []
        )
        |> Seq.filter (fun (y0, y1, _, _, _) -> y0.Values <> y1.Values)
        |> Seq.distinctBy (fun (y0, y1, _, _, _) -> y0.Values, y1.Values)
        //|> Seq.take 5000
        |> Seq.iteri (fun i (y0, y1, x1, x2, o) ->
            let toStr = Variable.ValueRange.toString true
            let x = x1.Values |> toStr
            let z = x2.Values |> toStr
            let y0 = y0.Values |> toStr
            let y1 = y1.Values |> toStr
            printfn $@"{i}. {y0} -> {y1} = {x} {o} {z}"
        )


    let printTest4 () =
        vars1
        |> Seq.map (fun vr ->
            vr.Values, vr.Values |> Variable.ValueRange.toProperties
        )
        |> Seq.distinct
        |> Seq.iteri (fun i (vr, props) ->
            printfn @$"{i}. {vr |> Variable.ValueRange.toString true} -> {props}"
        )


    let printTest5 () =
        vars1
        |> Seq.collect (fun var ->
            try
            let max = Variable.ValueRange.Maximum.create true 2N
            let v =
                var.Values
                |> Variable.ValueRange.setMax true max
                |> Variable.setValueRange true var
            [ var.Values, v.Values, v.Values |> Variable.ValueRange.diffWith var.Values ]
            with
            | _ -> []
        )
        |> Seq.distinct
        |> Seq.iteri (fun i (vr1, vr2, props) ->
            printfn $@"{i}. {vr1 |> Variable.ValueRange.toString true} -> {vr2 |> Variable.ValueRange.toString true} diff = {props}"
        )


    let resultToString = function
        | eq, cs ->
            $@"{eq |> Equation.toString true} {cs |> Equation.SolveResult.toString}"


    let logger : Types.Logging.Logger =
        {
            Log =
                fun { TimeStamp = _; Level = _; Message = msg } ->
                    match msg with
                    | :? Logging.SolverMessage as m ->
                        match m with
                        | Logging.SolverMessage m ->
                            match m with
                            | Events.EquationStartCalculation (op1, op2, x, y, xs) -> printfn $"{Equation.calculationToString op1 op2 x y xs}"
                            | _ -> ()
                        | _ -> ()
                    | _ -> ()
        }


    let printTest6 () =
        vars1
        |> Seq.allPairs vars2
        |> Seq.allPairs vars3
        |> Seq.choose (fun (y, (x1, x2)) ->
            try
                Equation.create ProductEquation Some (fun _ -> None) (y, [x1; x2])
                |> function
                | Some eq ->
                    printfn $"== start solving equation"
                    (eq, eq |> Equation.solve true logger) |> Some
                | None -> None
            with
            | _ ->
                printfn "== cannot solve equation"
                None
        )
        |> Seq.take 2000
        |> Seq.iteri (fun i (eq, res) ->
            printfn $"== finished solving equation {i}"
            $"""{eq |> Equation.toString true} ==>
        {res |> resultToString}"""
            |> printfn "%s"
        )


    //Property.createMinInclProp 1N
    //|> Property.toString true


    let var0 =
        Variable.Dto.createNew "y"
        |> Variable.Dto.setMin (Some 3N) true
        |> Variable.Dto.setMax (Some 4N) true
        |> Variable.Dto.fromDto


    let var1 =
        Variable.Dto.createNew "x1"
        |> Variable.Dto.setIncr [2N]
    //    |> Variable.Dto.setMax (Some 0N) true
        |> Variable.Dto.fromDto


    let var2 =
        Variable.Dto.createNew "x2"
        |> Variable.Dto.setMin (Some 3N) false
        |> Variable.Dto.fromDto

    let printTest7 () =
        // var_8 [3..4] = var_10 <..2..> * var_7 <3..> ==> [var_10: ..0]]
        Equation.createProductEqExc (var0, [var1; var2])
        |> fun eq ->
            printfn $"{eq |> Equation.toString true}"
            eq
        |> Equation.solve true { Log = printfn "%A" }
        |> resultToString
        |> printfn "%s"


    let printTest8 () =
        let varA = Variable.Dto.createNew "a" |> Variable.Dto.setMin (Some 0N) false |> Variable.Dto.fromDto
        let varB = Variable.Dto.createNew "c" |> Variable.Dto.setMin (Some 3600N) true |> Variable.Dto.fromDto
        let varC =
            Variable.Dto.createNew "b"
            |> Variable.Dto.setMin (Some 0N) false
            |> Variable.Dto.setMax (Some (5N/18N)) false
            |> Variable.Dto.fromDto
        Equation.createProductEqExc (varA, [varB; varC])
        |> fun eq ->
            printfn $"{eq |> Equation.toString true}"
            eq
        |> Equation.solve true { Log = printfn "%A" }
        |> resultToString
        |> printfn "%s"

