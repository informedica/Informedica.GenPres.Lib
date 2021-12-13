// A brand new go at the GenSolver 

#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"

#load "./../../Informedica.Utils.Lib/Scripts/load.fsx"

#time


module Set =

    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL

    let removeBigRationalMultiples xs =
        xs
        |> Set.fold (fun acc x1 ->
            acc 
            |> Set.filter (fun x2 ->
                x1 = x2 ||
                x2 |> BigRational.isMultiple x1 |> not
            )
        ) xs

    let private nextSmallestStep min1 incr1 max2 incr2 min max n =
        let min2 = if max2 - (n * incr2) <= 0N then incr2 else max2 - (n * incr2)
        
        List.allPairs [min1..incr1..(min1 + n * incr1)] [min2..incr2..max2] 
        |> List.sortBy (fun (x1, x2) -> x1 / x2)
        |> List.tryFind (fun (x1, x2) -> (x1 / x2) > (min/max)) 
        |> function
        | None            -> min, max
        | Some (min, max) -> min, max


    let calcSingleDivisor1 min1 incr1 max1 min2 incr2 max2 =
        let next = nextSmallestStep min1 incr1 max2 incr2 
        let rec calc n min max acc =
            match acc with
            | [] -> 
                let incr = min / max
                let n = n + 1N
                let min, max = next min max n

                [incr]
                |> calc n min max
            | h::_ ->
                let incr = min / max
                if (min >= max1 && max <= min2) ||
                   incr |> BigRational.isMultiple h then acc
                else
                    let n = n + 1N
                    let min, max = next min max n
                    acc @ [incr]
                    |> calc n min max
        
        calc 0N min1 max2 []
        |> Set.ofList


    //    let inline (/) x1 x2 = (?<-) Div x1 x2
    let calcSingleDivisor2 min1 incr1 min2 incr2 max2 =
        let max_orig = max2
        let rec calc max2 acc =
            match acc with
            | _ when acc |> Set.isEmpty ->
                set [min1/ max2]
                |> calc (max2 - incr2)
            | _ ->
                if (max2 <= min2) ||
                   ((min1 / max2) >= ((min1 + incr1) / max_orig)) then acc
                else
                    acc 
                    |> Set.add (min1 / max2)
                    |> calc (max2 - incr2)
        
        calc max2 Set.empty



    let calcDivisorsOfIncrs  min1 incrs1 max1 min2 incrs2 max2 =
        set [
            for incr1 in incrs1 do
                for incr2 in incrs2 do
                    if min1 > incr1 then
                        yield! calcSingleDivisor1 min1 incr1 max1 min2 incr2 max2
                    else
                        yield! calcSingleDivisor2 min1 incr1 min2 incr2 max2
        ]



module Tests =

    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL

    open Expecto
    open Expecto.Flip
    open FsCheck

    // create nonzero positief BigRationals
    let bigRGen (n, d) = 
        let d = if d = 0 then 1 else d
        let n = if n = 0 then 1 else n
        let n' = abs(n) |> BigRational.FromInt
        let d' = abs(d) |> BigRational.FromInt
        n'/d'

    let bigRGenerator =
        gen {
            let! n = Arb.generate<int>
            let! d = Arb.generate<int>
            return bigRGen(n, d)
        }

    type BigRGenerator () =
        static member BigRational () =
            { new Arbitrary<BigRational>() with
                override x.Generator = bigRGenerator }

    let config = { 
        FsCheckConfig.defaultConfig with 
            arbitrary = [typeof<BigRGenerator>] 
        }


    let tests =
    
        let notEmpty brs =
            brs
            |> function
            | [] -> set [1N]
            | xs -> xs |> Set.ofList

        let oneSetTests =

            let testProp n f = 
                let f = notEmpty >> f
                testPropertyWithConfig config n f


            testList "Any not empty Set with values > 0" [
                
                testProp "sort order is ascending" <| fun brs ->
                    brs
                    |> Set.toSeq
                    |> Expect.isAscending "order is not ascending"

                testProp "when multiples are removed, leaves a list of increments" <| fun brs ->
                    let incrs =
                        brs 
                        |> Set.removeBigRationalMultiples

                    brs
                    |> Set.forall (fun br ->
                        incrs
                        |> Set.exists (fun incr -> (br / incr).Denominator = 1I)
                    )
                    |> Expect.isTrue "there should be at least 1 increment"

                testProp "can be expressed as min, incrs, max" <| fun brs ->
                    let min = brs |> Set.minElement
                    let max = brs |> Set.maxElement
                    let incrs =
                        brs
                        |> Set.removeBigRationalMultiples
                    brs
                    |> Set.forall (fun br ->
                        br = min ||
                        br = max ||
                        incrs |> Set.exists (fun incr -> (br / incr).Denominator = 1I)
                    )
                    |> Expect.isTrue "set cannot be expressed in min, incrs and max" 
            ]

        let incrementTests =

            let testProp n f = 
                let f =
                    fun (brs1, brs2) ->
                        ((brs1 |> notEmpty), (brs2 |> notEmpty))
                        |> f
                let config = {
                    config with
                        maxTest = 10000
                }
                testPropertyWithConfig config n f

            let multiplying =
                testList "Multiplying 2 not Empty Sets with values > 0" [
                    testProp "the result has the product of the increments as increments" <| fun (brs1, brs2) ->
                        let exp =
                            Seq.allPairs brs1 brs2
                            |> Set.ofSeq
                            |> Set.map (fun (br1, br2) -> br1 * br2)
                            |> Set.removeBigRationalMultiples
                        let act =
                            let incrs1 = brs1 |> Set.removeBigRationalMultiples
                            let incrs2 = brs2 |> Set.removeBigRationalMultiples
                            Seq.allPairs incrs1 incrs2
                            |> Set.ofSeq
                            |> Set.map (fun (incr1, incr2) -> incr1 * incr2)
                            |> Set.removeBigRationalMultiples
                        Expect.equal "not equal" exp act
                ]

            let calcSingleDivisorTests =
                testList "Single Divisor Increments" [
                    testProp "calculates the superset of increments when dividing when min = incr" <| fun (brs1, brs2) ->
                        let incr1 = brs1 |> Set.removeBigRationalMultiples |> Set.minElement // just take the one element
                        let incr2 = brs2 |> Set.removeBigRationalMultiples |> Set.minElement // just take the one element
                        let min1 = brs1 |> Set.filter (BigRational.isMultiple incr1) |> Set.minElement
                        let max1 = 
                            brs1
                            |> Set.maxElement
                            |> fun x -> (x.Numerator |> BigRational.FromBigInt) * incr1
                        let min2 = brs2 |> Set.filter (BigRational.isMultiple incr2) |> Set.minElement
                        let max2 = 
                            brs2
                            |> Set.maxElement
                            |> fun x -> (x.Numerator |> BigRational.FromBigInt) * incr2

                        let brs1 = set [min1..incr1..max1]
                        let brs2 = set [min2..incr2..max2]

                        let y =
                            Seq.allPairs brs1 brs2
                            |> Set.ofSeq
                            |> Set.map (fun (br1, br2) -> br1 / br2)

                        let exp = y |> Set.removeBigRationalMultiples

                        let act =
                            Set.calcSingleDivisor2 min1 incr1 max1 min2 incr2 max2
                        
                        let msg = $"""
                            calculating divisors of {brs1} / {brs2}
                            which equals {y}
                            {act} is not a superset of {exp}
                            """
                        Expect.isTrue msg (Set.isSubset exp act)

                    testProp "calculates the superset of increments when dividing and min > incr" <| fun (brs1, brs2) ->
                        let incr1 = brs1 |> Set.removeBigRationalMultiples |> Set.minElement // just take the one element
                        let incr2 = brs2 |> Set.removeBigRationalMultiples |> Set.minElement // just take the one element
                        let min1 = brs1 |> Set.filter (BigRational.isMultiple incr1) |> Set.minElement
                        let max1 = 
                            brs1
                            |> Set.maxElement
                            |> fun x -> (x.Numerator |> BigRational.FromBigInt) * incr1
                        let min2 = brs2 |> Set.filter (BigRational.isMultiple incr2) |> Set.minElement
                        let max2 = 
                            brs2
                            |> Set.maxElement
                            |> fun x -> (x.Numerator |> BigRational.FromBigInt) * incr2

                        let brs1 = set [min1..incr1..max1]
                        let brs2 = set [min2..incr2..max2]

                        let y =
                            Seq.allPairs brs1 brs2
                            |> Set.ofSeq
                            |> Set.map (fun (br1, br2) -> br1 / br2)

                        let exp = y |> Set.removeBigRationalMultiples

                        let act =
                            Set.calcSingleDivisor1 min1 incr1 max1 min2 incr2 max2
                        
                        let msg = $"""
                            calculating divisors of [{min1}..{incr1}..{max1}] / [{min2}..{incr1}..{max2}]]
                            which equals {y}
                            {act} is not a superset of {exp}
                            """
                        Expect.isTrue msg (Set.isSubset exp act)



                ]

            let division = 
                testList "Division of 2 not Empty Sets with values > 0" [
                    testProp "the result has increments that can be calculated" <| fun (brs1, brs2) ->
                        let exp =
                            Seq.allPairs brs1 brs2
                            |> Set.ofSeq
                            |> Set.map (fun (br1, br2) -> br1 / br2)
                            |> Set.removeBigRationalMultiples
                        let act =
                            let min1 = brs1 |> Set.minElement
                            let max1 = brs1 |> Set.maxElement
                            let min2 = brs2 |> Set.minElement
                            let max2 = brs2 |> Set.maxElement
                            let incrs1 = brs1 |> Set.removeBigRationalMultiples
                            let incrs2 = brs2 |> Set.removeBigRationalMultiples
                            Set.calcDivisorsOfIncrs min1 incrs1 max1 min2 incrs2 max2
                        Expect.isTrue "not subset" (Set.isSubset exp act)
                ]

            testList "Calculation" [
                calcSingleDivisorTests
                multiplying
//                division
            ]

        testList "all tests" [
            oneSetTests
            incrementTests
        ]


open Expecto


let run = runTestsWithCLIArgs [] [|"--summary" |]



Tests.tests
|> run


open MathNet.Numerics

Set.calcSingleDivisor2 2N 2N 3N 1N (1N/2N) 1N

Set.calcSingleDivisor1 1N 1N  1N 1N

//: [31/25000N..[31/200000]..31/16000N] / [1/1000N..[1/1000]..29/2000N]
Set.calcSingleDivisor1 
    (31N/25000N) 
    (31N/200000N) 
    (12N * 31N/200000N) 
    (1N/100N)    
    (1N/1000N)    
    (14N * 1N/1000N)    
|> Seq.iteri (printfn "%i. %A")


let x1 = set [(8N * 31N/200000N) .. (31N/200000N) .. (200N * 31N/200000N)]
let x2 = set [(1N/100N) .. (1N/1000N) .. (100N * 1N/1000N)]
Seq.allPairs x1 x2
|> Seq.map (fun (x1, x2) -> x1 / x2)
|> fun x -> printfn $"total: {x |> Seq.length}"; x
|> Set.ofSeq
|> fun x -> printfn $"total: {x |> Set.count}"; x
|> Set.removeBigRationalMultiples
|> Set.count |> (printfn "%A")


let calc op (a : BigRational Set) (b : BigRational Set) =
    let a = a |> Set.toSeq
    let b = b |> Set.toSeq
    Seq.allPairs a b
    |> Seq.map (fun (a, b) -> a |> op <| b)
    |> Set.ofSeq

let div = calc (/)
let mult = calc (*)
let add = calc (+)
let subtr = calc (-)


let a = set [4N .. 2N .. 8000N]
let b = set [3N .. 3N .. 9000N]

// Check division

let max = 
    (a |> Set.maxElement) / (b |> Set.minElement)


div a b
|> fun c -> printfn "exp:\t\t%A" c; c
|> Set.removeBigRationalMultiples
|> fun xs ->
    set [
        for x in xs do
            yield! [x .. x .. max]
    ]
|> fun c -> printfn "set div:\t%A" c


mult a (div (set [1N]) b)
|> Set.removeBigRationalMultiples
|> fun xs ->
    set [
        for x in xs do
            yield! [x .. x .. max]
    ]
|> fun c -> printfn "a * 1/b:\t%A" c


mult a (Set.calcSingleDivisor2 1N 1N 1N (b |> Set.minElement) 3N (b |> Set.maxElement))
|> fun xs ->
    set [
        for x in xs do
            yield! [x .. x .. max]
    ]
|> fun c -> printfn "a * div:\t%A" c


Set.calcSingleDivisors3 (a |> Set.minElement) 2N (a |> Set.maxElement) (b |> Set.minElement) 3N (b |> Set.maxElement)
|> fun c -> printfn "a * incr:\t%A" c


Set.calcSingleDivisor2 2N 2N 8000N (b |> Set.minElement) 3N (b |> Set.maxElement)
|> Set.count

// Check multiplication


mult a b
|> fun c -> printfn "exp: %A" c; c
|> Set.removeBigRationalMultiples
|> fun xs ->
    set [
        for x in xs do
            yield! [x .. x .. (8N * 9N)]
    ]
|> fun c -> printfn "act: %A" c