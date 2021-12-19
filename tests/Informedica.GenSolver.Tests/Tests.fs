module Tests

open MathNet.Numerics

open Expecto
open Expecto.Flip

open Informedica.Utils.Lib.BCL
open Informedica.GenSolver.Lib
open Types


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

    type BigRGenerator () =
        static member BigRational () =
            { new Arbitrary<BigRational>() with
                override x.Generator = bigRGenerator }

    let config = { 
        FsCheckConfig.defaultConfig with 
            arbitrary = [typeof<BigRGenerator>] 
        }




module Name =

    open Informedica.GenSolver.Lib.Variable.Name    

    [<Tests>]
    let tests =
        testList "Variable Name Tests" [
        
            testCase "Handles null value in failure function" <| fun _ ->
                null 
                |> create (fun _ -> false) (fun _ -> true)
                |> Expect.isTrue "should handle null in failure function"
            
            create (fun _ -> true) (fun _ -> true)
            |> testProperty "Can throw any string at the name create function"
        
            fun s -> 
                let succ s (Name n) = n = (s |> String.trim) && n  |> String.length <= 1000
                let fail = function 
                    | Exceptions.NullOrWhiteSpaceException -> true
                    | Exceptions.LongerThan1000 x -> x |> String.length > 1000

                s 
                |> create (succ s) fail
            |> testProperty "The succ and fail function will catch all exceptions"
        ]





module ValueRange =

    
    open Swensen.Unquote
    open Variable.ValueRange.Operators

    module ValueRange = Variable.ValueRange
    module Minimum = ValueRange.Minimum
    module Maximum = ValueRange.Maximum
    module DTO = Variable.Dto

    let getMin  = ValueRange.getMin >> Option.bind (Minimum.minToBigRational >> Some)
    let getMax  = ValueRange.getMax >> Option.bind (Maximum.maxToBigRational >> Some)

    let createMinIncl = Minimum.createMin true
    let createMinExcl = Minimum.createMin false
    let createMaxIncl = Maximum.createMax true
    let createMaxExcl = Maximum.createMax false

    let contains v vr = vr |> ValueRange.contains v

    let isBetweenMinMax min max  = ValueRange.isBetween min max 

    let createMinMax succ fail vs min max = 
        try 
            ValueRange.create vs min max
            |> succ
        with
        | e -> 
            vs
            |> function
            | Some s ->
                s
                |> Seq.map string
                |> String.concat ", "
            | None -> "No values"
            |> printfn "\n\n=== Could not create valueset with %A %A %s ===\n\n" min max

            e.ToString()
            |> printfn "%s" 
            e |> fail

    [<Tests>]    
    let tests =

        let testProp n prop =
            prop
            |> testPropertyWithConfig Generators.config n

        testList "ValueRange" [

            testList "Given Min is None Incr is None and Max is None" [
                // Test fixture
                let min = None
                let max = None
                let vs  = None

                isBetweenMinMax min max
                |> testProp "The isBetween function always returns true"
    
                fun n ->
                    let vs = 
                        if n > 0 then
                            [1..n] 
                            |> List.map BigRational.fromInt
                            |> Set.ofList
                            |> Some
                        else None

                    let succ vr = 
                        if n > 0 then vr |> ValueRange.count = n
                        else vr |> ValueRange.count = 0
                    let fail _ = false

                    createMinMax succ fail vs min max
                |> testProp "The resulting ValueSet contains an equal amount"

                fun _ ->
                    let succ vr = 
                        test <@ vr |> getMin = None && vr |> getMax = None @>
                    let fail _  = test <@ false @>
                    createMinMax succ fail vs min max
                |> testCase "The Min and Max are None"

                fun _ ->
                    let succ vr = test <@ vr |> ValueRange.isUnrestricted @>
                    let fail _  = test <@ false @>
                    createMinMax succ fail vs min max
                |> testCase "Creating a ValueRange returns an unrestricted ValueRange"        
    
                let succ vr = test <@ vr |> ValueRange.count = 0 @>
                let fail _  = test <@ false @>

                fun _ ->
                    createMinMax succ fail vs min max
                |> testCase "Counting returns zero"
    
                fun  x -> 
                    let vr = ValueRange.create None None None
                    vr |> contains x
                |> testProp "The ValueRange can contain any Value"
            ]

            testList "Given one Value Min is None Incr is None Max is None" [

                let min = None
                let max = None
                // List with one value
                let v = 1N 
                let vs = 
                    Set.empty
                    |> Set.add v
                    |> Some

                fun _ ->
                    let succ vr = 
                        test <@ vr |> getMin = Some v && vr |> getMax = Some v @>
                    let fail _  = test <@ false @>
                    createMinMax succ fail vs min max 
                |> testCase "Both Min and Max are the Inclusive and that Value"
    
                fun _ ->
                    let succ vr = test <@ vr |> ValueRange.count = 1 @>
                    let fail _  = test <@ false @>
                    createMinMax succ fail vs min max
                |> testCase "Counting returns one"
    
                fun x ->
                    let vs = Set.empty |> Set.add x |> Some

                    let succ vr = 
                        vr |> contains x && 
                        vr |> contains (x + BigRational.three) |> not
                    let fail _ = false
                    createMinMax succ fail vs min max
                |> testProp "The result can only contain that Value"
                

            ]

            testList "Given a ValueRange with Min Incl is 1 and Max is None" [

                let min = 1N |> createMinIncl |> Some
                let max = None
                let vs = None

    
                fun v ->    
                    let inBetween = v |> isBetweenMinMax min max
                    if v >= 1N then inBetween else inBetween |> not
                |> testProp "The isBetween function returns true for any Value LTE to one"

                fun _ ->    
                    let minExcl = 1N |> createMinExcl
                    let maxExcl = 2N |> createMaxExcl
                    test <@ min |> Option.get |> Minimum.minSTEmin minExcl @>                     // min incl 1 < min incl 1
                    test <@ min |> Option.get |> ValueRange.minSTEmax maxExcl  @>                    // min incl 1 < max incl 2
                    test <@ min |> Option.get |> ValueRange.minLTmax (1N |> createMaxExcl) @>        // min incl 1 > max incl 1

                    test <@ min |> Option.get |> ValueRange.minLTmax (1N |> createMaxIncl) |> not @> // min incl 1 > max incl 1 is false
                    test <@ min |> Option.get |> Minimum.minLTmin (1N |> createMinIncl) |> not @> // min incl 1 > min incl 1 is false
                |> testCase "Min is ST Min Excl 1 and ST Max Incl 2 but LT Max Excl 1"
    
                fun _ ->
                    let succ vr = test <@ vr |> ValueRange.count = 0 @>
                    let fail _  = test <@ false @>
                    createMinMax succ fail vs min max
                |> testCase "The count is zero"
    
                fun _ ->
                    let succ vr = 
                        test <@ vr |> ValueRange.getMin = min && 
                                vr |> ValueRange.getMin 
                                   |> Option.get 
                                   |> Minimum.isMinExcl 
                                   |> not @>
                    let fail _  = test <@false@>
                    createMinMax succ fail vs min max
                |> testCase "Min is one and is Incl"
    
                fun v ->
                        let vs = 
                            vs 
                            |> Option.bind (fun s -> s |> Set.add v |> Some)
            
                        let succ vr =
                            let contains = vr |> contains v
                            if v >= 1N then contains else contains |> not
                        let fail _ = printfn "fail"; false
            
                        createMinMax succ fail vs min max

                |> testProp "The result can contain any Value GTE one"
            ]
          
            testList "Given Min is None Incr is None Max Incl is 1" [
            
                let min = None
                let max = 1N |> createMaxIncl |> Some
                let vs = None

    
                fun v ->
                    let inBetween = v |> isBetweenMinMax min max
                    if v <= 1N then inBetween else inBetween |> not
                |> testProp "The isBetween function returns true for any Value STE to 1"
    
                fun _ ->
                    let succ vr = test <@ vr |> ValueRange.count = 0 @>
                    let fail _  = test <@ false @>
                    createMinMax succ fail vs min max
                |> testCase "Count returns zero"
    
                fun _ ->
                    let succ vr = 
                        test <@ vr |> ValueRange.getMax = max && 
                                vr |> ValueRange.getMax 
                                   |> Option.get 
                                   |> Maximum.isMaxExcl 
                                   |> not @>
                    let fail _  = ()
                    createMinMax succ fail vs min max
                |> testCase "Max Incl is one"
    
                fun v ->
                    let vs = 
                        vs 
                        |> Option.bind (fun s -> s |> Set.add v |> Some)
            
                    let succ vr = 
                        let contains = vr |> ValueRange.contains v
                        if v <= 1N then contains else contains |> not
                    let fail _  = false
                    
                    createMinMax succ fail vs min max

                |> testProp "The result can contain any Value LTE to one"
            
            ]

            testList "Given Min Incl is 2 and Max Incl is 4" [

                let min = 2N |> createMinIncl |> Some
                let max = 4N |> createMaxIncl |> Some
                let vs = None
    
                fun v ->
                    if v >= 2N && v <= 4N then v |> isBetweenMinMax min max
                    else v |> isBetweenMinMax min max |> not
                |> testProp "The isBetween function returns true any Value LTE 2 and STE 4"

    
                fun _ ->
                    let succ vr = test <@ vr |> ValueRange.count = 0 @>
                    let fail _  = test <@ false @>
                    createMinMax succ fail vs min max
                |> testCase "Count returns zero"
    
                fun v ->
                    let vs = 
                        vs 
                        |> Option.bind (fun s -> s |> Set.add v |> Some)
            
                    let succ vr = 
                        if v >= 2N && v <= 4N then vr |> ValueRange.contains v
                        else vr |> ValueRange.contains v |> not
                    let fail _  = false
                    
                    createMinMax succ fail vs min max
                |> testProp "The ValueRange can only any Value equal to or between 2 and 4"
            
            ]

            testList "Given a ValueRange with a Min and a ValueRange with a Min" [

                let create incl v = 
                    ValueRange.create None 
                        (v |> Minimum.createMin incl |> Some) None

                let test op pred v1 incl1 v2 incl2 =
                    let vr1 = v1 |> create incl1 
                    let vr2 = v2 |> create incl2
                    (vr1 |> op <| vr2) 
                    |> ValueRange.getMin 
                    |> pred v1 v2 incl1 incl2                
    
    
                fun v1 incl1 v2 incl2 ->
                    let pred v1 v2 incl1 incl2 m = m |> Option.get = ((v1 * v2) |> Minimum.createMin (incl1 && incl2))
                    test (^*) pred v1 incl1 v2 incl2
                |> testProp "When multiplied the result has min that is the multiple"
    
                fun v1 incl1 v2 incl2 ->
                    let pred _ _ _ _ m = m = None 
                    test (^/) pred v1 incl1 v2 incl2
                |> testProp "When divided the result has a Min None"

                fun v1 incl1 v2 incl2 ->
                    let pred v1 v2 incl1 incl2 m = m |> Option.get = ((v1 + v2) |> Minimum.createMin (incl1 && incl2))
                    test (^+) pred v1 incl1 v2 incl2
                |> testProp "When added the result has min that is the addition"
    
                fun v1 incl1 v2 incl2 ->
                    let pred _ _ _ _ m = m = None
                    test (^-) pred v1 incl1 v2 incl2
                |> testProp "When subtracting the result has min that is None"

            ]

            testList "Given a calculation with ValueRange with a Min and a ValueRange with a Max" [

                let createVrMin incl v = ValueRange.create None (v |> Minimum.createMin incl |> Some) None
                let createVrMax incl v = ValueRange.create None None (v |> Maximum.createMax incl |> Some)

                let prop op predMin predMax v1 incl1 v2 incl2 =
                    let vr1 = v1 |> createVrMin incl1 
                    let vr2 = v2 |> createVrMax incl2
                    (vr1 |> op <| vr2) |> ValueRange.getMin |> predMin v1 v2 incl1 incl2 &&
                    (vr1 |> op <| vr2) |> ValueRange.getMax |> predMax v1 v2 incl1 incl2
                                        
                // <1..> * <..1] = <..>
                fun v1 v2 incl1 incl2 ->
                        let pred v1 v2 _ _ m =
                            if v1 <= 0N || v2 <= 0N then true
                            else
                                if m <> None then 
                                    printf "expected none but got: %A" m
                                m = None
                        prop (^*) pred pred v1 incl1 v2 incl2
                |> testProp "When Min and Max > 0, multiplied the result has Min None and Max None"
        
                fun v1 v2 incl1 incl2 ->
                    let predMin v1 v2 _ _ m =
                        if v2 >= 0N then m = None
                        else
                            m |> Option.get = (v1 / v2 |> Minimum.createMin (incl1 && incl2))
                    let predMax _ v2 _ _ m =
                        if v2 > 0N then true else m = None
                    prop (^/) predMin predMax v1 incl1 v2 incl2
                |> testProp "When divided with Max < 0, the result has Min of Min/Max and Max is None"
    
                fun v1 v2 incl1 incl2 ->
                        let pred _ _ _ _ m = m = None
                        prop (^+) pred pred v1 incl1 v2 incl2
                |> testProp "When added the result has Min is None and Max None"
    
                fun v1 v2 incl1 incl2 ->
                        let predMin v1 v2 incl1 incl2 m =
                            m = (v1 - v2 |> Minimum.createMin (incl1 && incl2) |> Some) 
                        let predMax _ _ _ _ m = m = None
                        prop (^-) predMin predMax v1 incl1 v2 incl2
                |> testProp "When subtracting the result has Min is Max - Min and Max None"        
            
                fun _ ->
                    let v1, v2 = 1N, 1N

                    let vr1 = v1 |> createVrMin true 
                    let vr2 = v2 |> createVrMax true
                    
                    let vr = vr1 ^/ vr2

                    test<@ vr |> ValueRange.getMin = None @>
                    test<@ vr |> ValueRange.getMax = None @>
                |> testCase "Failing case 1, when divided the result has Min is None and Max is None"

                fun _ ->
                    let v1, v2 = 2N, 3N

                    let vr1 = v1 |> createVrMin true 
                    let vr2 = v2 |> createVrMax true
    
                    let vr = vr1 ^* vr2

                    test<@ vr |> ValueRange.getMin = None @>
                    test<@ vr |> ValueRange.getMax = None @>
                |> testCase "Failing case 2, when mutliplied the result has Min is None and Max is None"

            ]
            
            testList "Given calculation with ValueRange with a Max and a ValueRange with a Min" [

                let createVrMin incl v = ValueRange.create None (v |> Minimum.createMin incl |> Some) None
                let createVrMax incl v = ValueRange.create None None (v |> Maximum.createMax incl |> Some)

                let prop op predMin predMax v1 incl1 v2 incl2 =
                    let vr1 = v1 |> createVrMax incl1 
                    let vr2 = v2 |> createVrMin incl2
                    (vr1 |> op <| vr2) |> ValueRange.getMin |> predMin v1 v2 incl1 incl2 &&
                    (vr1 |> op <| vr2) |> ValueRange.getMax |> predMax v1 v2 incl1 incl2
    
    
                fun v1 v2 incl1 incl2 ->
                    let pred _ _ _ _ m = m = None
                    prop (^*) pred pred v1 incl1 v2 incl2
                |> testProp "When multiplied the result has Min None and Max None"

    
                fun v1 v2 incl1 incl2 ->
                    let pred _ min _ _ m = 
                        if min <= 0N then m = None else true
                    prop (^/) pred pred v1 incl1 v2 incl2
                |> testProp "When divided the result has Max of None and Min is None when Min <= 0N"

                fun v1 v2 incl1 incl2 ->    
                    let pred _ _ _ _ m = m = None
                    prop (^+) pred pred v1 incl1 v2 incl2
                |> testProp "When added the result has Min is None and Max None"
    
                fun v1 v2 incl1 incl2 ->
                    let predMax v1 v2 incl1 incl2 m =
                        m = (v1 - v2 |> Maximum.createMax (incl1 && incl2) |> Some)
                    let predMin _ _ _ _ m = m = None
                    prop (^-) predMin predMax v1 incl1 v2 incl2
                |> testProp "When subtracting the result has Max is Max - Min and Min is None"        
            
            ]

            testList "Given addition multiplication or division of two non empty positive Value Sets" [

                let createVals ns =
                    ns
                    |> List.map BigRational.fromInt
                    |> Set.ofList

                let create = createVals >> ValueRange.ValueSet

                let checkAdd l1 l2 =
                    // Only values > 0
                    let l1 = l1 |> List.filter ((<) 0)
                    let l2 = l2 |> List.filter ((<) 0)

                    match l1, l2 with
                    | [], [] 
                    | _ , []
                    | [], _ -> true
                    |_ ->
                        let add =
                            [ for x1 in l1 do
                                for x2 in l2 do
                                    yield x1 + x2 ]
                            |> List.toSeq
                            // List will only contain distinct values
                            |> Seq.distinct
                            |> Seq.toList
                        let l1' = l1 |> create
                        let l2' = l2 |> create
                        (l1' ^+ l2') |> ValueRange.count = add.Length

                let checkMult l1 l2 =
                    // Only values > 0
                    let l1 = l1 |> List.filter ((<) 0)
                    let l2 = l2 |> List.filter ((<) 0)

                    match l1, l2 with
                    | [], [] 
                    | _ , []
                    | [], _ -> true
                    |_ ->
                        let mult =
                            [ for x1 in l1 do
                                for x2 in l2 do
                                    yield x1 * x2 ]
                            |> List.toSeq
                            // List will only contain distinct values
                            |> Seq.distinct
                            |> Seq.toList
                        let l1' = l1 |> create
                        let l2' = l2 |> create
                        (l1' ^* l2') |> ValueRange.count = mult.Length

                let checkDiv l1 l2 =
                    // Only values > 0
                    let l1 = l1 |> List.filter ((<) 0) |> List.map BigRational.FromInt
                    let l2 = l2 |> List.filter ((<) 0) |> List.map BigRational.FromInt

                    match l1, l2 with
                    | [], [] 
                    | _ , []
                    | [], _ -> true
                    |_ ->

                        let create = Set.ofList >> ValueRange.ValueSet

                        let div =
                            [ for x1 in l1 do
                                for x2 in l2 do
                                    yield x1 / x2 ]
                            |> List.toSeq
                            // List will only contain distinct values
                            |> Seq.distinct
                            |> Seq.toList
                        let l1' = l1 |> create
                        let l2' = l2 |> create

                        (l1' ^/ l2') |> ValueRange.count = div.Length

                testProp "With addition, resultset will be a distinct set of calculated values" checkAdd
                testProp "With multiplication, resultset will be a distinct set of calculated values" checkMult
                testProp "With division, resultset will be a distinct set of calculated values" checkDiv

            ]

            testList "Given subtraction of two value sets" [
            
                let ff = fun _ -> failwith "Cannot create"

                let createVals ns =
                    let create vs = createMinMax id ff vs None None

                    ns
                    |> List.map BigRational.fromInt
                    |> Set.ofList
                    |> Some
                    |> create

    
                fun  l1 l2  ->
                    // Only values > 0
                    let l1 = l1 |> List.filter ((<) 0)
                    let l2 = l2 |> List.filter ((<) 0)

                    match l1, l2 with
                    | [], _ 
                    | _, [] -> true
                    | _ ->
                        let create = createVals
                        let subtr =
                            [ for x1 in l1 do
                                for x2 in l2 do
                                    yield x1 + x2 ]
                            |> List.toSeq
                            // List will only contain distinct values
                            |> Seq.distinct
                            |> Seq.toList
                        let l1' = l1 |> create
                        let l2' = l2 |> create
                        (l1' ^+ l2') |> ValueRange.count = subtr.Length
                |> testProp "The resultset will be a distinct set of only positive values"

            ]
            
            testList "Dto, There and back again" [

                fun vs min minincl max maxincl ->
        
                    let fromDto = DTO.fromDtoOpt
                    let toDto   = DTO.toDto

                    let setMin m = DTO.setMin m minincl
                    let setMax m = DTO.setMax m maxincl
        
                    let dto = 
                        let dto = DTO.createNew "test"
                        try 
                            let dto = 
                                match vs with 
                                | [] -> dto
                                | _  -> dto |> DTO.setVals vs
                            let dto = if min <= max then dto |> setMin min else dto
                            let dto = if max >= min then dto |> setMax max else dto
                            dto
                        with _ -> dto
                        
                    match dto |> DTO.fromDtoOpt with
                    | Some vr -> 
                        try
                            let dto'  = vr |> toDto |> fromDto |> Option.get |> toDto
                            let dto'' = dto' |> fromDto |> Option.get |> toDto
                            dto' = dto''   
                        with
                        | _ -> printfn "Failed dto: %A vr: %A toDto:%A" dto vr (vr |> toDto); false
                    | None -> 
                        true
        
    
                |> testProp "Creating from dto has same result as creating from dto, back to dto and again from dto"
            
            ] 


        ]



