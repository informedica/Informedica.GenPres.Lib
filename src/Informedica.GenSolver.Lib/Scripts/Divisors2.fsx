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


    // Only works when min1 = incr1
    let calcSingleDivisor min1 incr1 max1 min2 incr2 max2 =
        let next min max mem = 
            set [
                if min + incr1 <= max1 then (min + incr1), max
                if max - incr2 >= min2 then min, (max - incr2)
            ]
            |> Set.union mem
            |> fun mem ->
                let min, max =
                    mem
                    |> Seq.tryFind (fun (x1, x2) -> (x1/x2) > (min/max))
                    |> Option.defaultValue (min, max) 
                min, max, mem

        let rec calc min max mem acc =
            if acc |> Set.isEmpty then
                let incr = min / max
                let mem = set [(min, max)]
                let min, max, mem = next min max mem
                set [incr]
                |> calc min max mem
            else
                let incr = min / max
                if (min >= max1 && max <= min2) ||
                   incr |> BigRational.isMultiple (acc |> Set.maxElement) then acc
                else
                    let min, max, mem = next min max mem
                    acc 
                    |> Set.add incr
                    |> calc min max mem
        
        calc min1 max2 Set.empty Set.empty


    let calcDivisorsOfIncrs  min1 incrs1 max1 min2 incrs2 max2 =
        set [
            for incr1 in incrs1 do
                for incr2 in incrs2 do
                    yield! calcSingleDivisor min1 incr1 max1 min2 incr2 max2
        ]


open MathNet.Numerics


type Range =
    {
        Multiples : int Set
        Delta : BigRational
    }


module Range =

    let create min incr max =
        { 
            Multiples =
                [min/incr..1N..max/incr]
                |> List.map (BigRational.ToBigInt >> int)
                |> Set.ofList
            Delta = incr
        }

    let calc op1 op2 (r1 : Range) (r2 : Range) =
        { 
            Multiples =
                r1.Multiples
                |> Seq.allPairs r2.Multiples
                |> Seq.map (fun (x1, x2) -> x1 |> op1 <| x2)
                |> Set.ofSeq
            Delta = r1.Delta |> op2 <| r2.Delta
        }


    let multiply = calc (*) (*)
    let divide = calc (/) (/)
    let add = calc (+) (+)
    let subtr = calc (-) (-)


