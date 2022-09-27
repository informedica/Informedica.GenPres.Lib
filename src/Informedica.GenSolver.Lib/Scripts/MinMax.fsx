
#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: FParsec"

#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "nuget: Unquote"


#r "../../Informedica.Utils.Lib/bin/Debug/net6.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.GenUnits.Lib/bin/Debug/net6.0/Informedica.GenUnits.Lib.dll"

#load "../Utils.fs"
#load "../Types.fs"
#load "../Logging.fs"
#load "../Exceptions.fs"
#load "../Variable.fs"


open MathNet.Numerics
open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.GenSolver.Lib

module Minimum = Informedica.GenSolver.Lib.Variable.ValueRange.Minimum
module Maximum = Informedica.GenSolver.Lib.Variable.ValueRange.Maximum


/// Create the necessary test generators
module Generators =

    open Types
    open Expecto
    open FsCheck
    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL


    let bigRGen (n, d) =
        let d = 1
//        let d = if d = 0 then 1 else d
        let n = n |> BigRational.FromInt
        let d = d |> BigRational.FromInt
        n / d


    let bigRGenOpt (n, d) = bigRGen (n, 1) |> Some


    let bigRGenerator =
        gen {
            let! n = Arb.generate<int>
            let! d = Arb.generate<int>
            return bigRGen(n, d)
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

    type BigRGenerator () =
        static member BigRational () =
            { new Arbitrary<BigRational>() with
                override x.Generator = bigRGenerator
            }


    type VarDtoGenerator () =
        static member Variable () =
            { new Arbitrary<Variable.Dto.Dto>() with
                override x.Generator = varGenerator
            }


    let config = {
        FsCheckConfig.defaultConfig with
            arbitrary = [
                typeof<BigRGenerator>
                typeof<VarDtoGenerator>
            ]
            maxTest = 1000
        }


    let testProp testName prop =
        prop |> testPropertyWithConfig config testName


    let run = runTestsWithCLIArgs [] [|"--summary" |]



/// Functions to calculate the `Minimum`
/// and `Maximum` in a `ValueRange`.
/// I.e. what happens when you mult, div, add or subtr
/// a `Range`, for example:
/// <1N..3N> * <4N..5N> = <4N..15N>
module MinMaxCalcultor =

    /// Exceptions that a MinMaxCalculator function can raise.
    module Exceptions =

        type Message = | NotAValidOperator

        exception MinMaxCalculatorException of Message

        let raiseExc m = m |> MinMaxCalculatorException |> raise


    /// Calculate **x1** and **x2** with operator **op**
    /// and use **incl1** and **inc2** to determine whether
    /// the result is inclusive. Use constructor **c** to
    /// create the optional result.
    let calc c op (x1, incl1) (x2, incl2) =
        let opIsMultOrDiv = (op |> BigRational.opIsMult || op |> BigRational.opIsDiv)

        let incl =
            match incl1, incl2 with
            | true, true -> true
            | _ -> false

        match x1, x2 with
        | Some v, _  when opIsMultOrDiv && v = 0N ->
            0N |> c incl1 |> Some
        | Some v, _
        | _, Some v when op |> BigRational.opIsMult && v = 0N ->
            0N |> c incl |> Some
        | Some _, None when op |> BigRational.opIsDiv ->
            0N |> c incl |> Some
        | Some (v1), Some (v2) ->
            if op |> BigRational.opIsDiv && v2 = 0N then None
            else
                v1 |> op <| v2 |> c incl |> Some
        | _ -> None


    /// Calculate an optional `Minimum`
    let calcMin = calc Minimum.create


    /// Calculate an optional `Maximum`
    let calcMax = calc Maximum.create


    let minimize min1 min2 =
        match min1, min2 with
        | None,    None     -> None
        | Some _,  None
        | None,    Some _   -> None
        | Some m1, Some m2 ->
            if m1 |> Minimum.minSTmin m2 then m1
            else m2
            |> Some


    let maximize max1 max2 =
        match max1, max2 with
        | None,    None     -> None
        | Some _,  None
        | None,    Some _   -> None
        | Some m1, Some m2 ->
            if m1 |> Maximum.maxGTmax m2 then m1
            else m2
            |> Some


    /// Match a min, max tuple **min**, **max**
    /// to:
    ///
    /// * `PP`: both positive
    /// * `NN`: both negative
    /// * `NP`: one negative, the other positive
    let (|PP|NN|NP|NZ|ZP|) (min, max) =
        match min, max with
        | Some min, _         when min > 0N             -> PP
        | _,        Some max  when max < 0N             -> NN
        | Some min, Some max  when min < 0N && max > 0N -> NP
        | None,     Some max  when max > 0N             -> NP
        | Some min, None      when min < 0N             -> NP
        | None,     None                                -> NP
        | _,        Some max  when max = 0N             -> NZ
        | Some min, _         when min = 0N             -> ZP
        // failing cases
        | Some min, Some max when min = 0N && max = 0N  ->
            $"{min} = {max} = 0" |> failwith
        | Some min, Some max when min >= 0N && max < 0N ->
            $"{min} > {max}" |> failwith
        | _ -> $"could not handle {min} {max}" |> failwith


    /// Calculate `Minimum` option and
    /// `Maximum` option for addition of
    /// (**min1**, **max1**) and (**min2**, **max2)
    let addition min1 max1 min2 max2 =
        let min = calcMin (+) min1 min2
        let max = calcMax (+) max1 max2
        min, max


    /// Calculate `Minimum` option and
    /// `Maximum` option for subtraction of
    /// (**min1**, **max1**) and (**min2**, **max2)
    let subtraction min1 max1 min2 max2 =
        let min = calcMin (-) min1 max2
        let max = calcMax (-) max1 min2
        min, max


    /// Calculate `Minimum` option and
    /// `Maximum` option for multiplication of
    /// (**min1**, **max1**) and (**min2**, **max2)
    let multiplication min1 max1 min2 max2 =
        match ((min1 |> fst), (max1 |> fst)), ((min2 |> fst), (max2 |> fst)) with
        | PP, PP ->  // min = min1 * min2, max = max1 * max2
            calcMin (*) min1 min2, calcMax (*) max1 max2
        | PP, ZP ->  // min = min1 * min2, max = max1 * max2
            calcMin (*) min1 min2, calcMax (*) max1 max2
        | PP, NN -> // min = max1 * min2, max = min1 * max2
            calcMin (*) max1 min2, calcMax (*) min1 max2
        | PP, NZ -> // min = max1 * min2, max = min1 * max2
            calcMin (*) max1 min2, calcMax (*) min1 max2
        | PP, NP -> // min = min1 * min2, max = max1 * max2
            calcMin (*) max1 min2, calcMax (*) max1 max2

        | ZP, PP ->  // min = min1 * min2, max = max1 * max2
            calcMin (*) min1 min2, calcMax (*) max1 max2
        | ZP, ZP ->  // min = min1 * min2, max = max1 * max2
            calcMin (*) min1 min2, calcMax (*) max1 max2
        | ZP, NN -> // min = max1 * min2, max = min1 * max2
            calcMin (*) max1 min2, calcMax (*) min1 max2
        | ZP, NZ -> // min = max1 * min2, max = min1 * max2
            calcMin (*) max1 min2, calcMax (*) min1 max2
        | ZP, NP -> // min = min1 * min2, max = max1 * max2
            calcMin (*) min1 min2, calcMax (*) max1 max2

        | NN, PP -> // min = min1 * max2, max = max1 * min2
            calcMin (*) min1 max2, calcMax (*) max1 min2
        | NN, ZP -> // min = min1 * max2, max = max1 * min2
            calcMin (*) min1 max2, calcMax (*) max1 min2
        | NN, NN -> // min = max1 * max2, max = min1 * min2
            calcMin (*) max1 max2, calcMax (*) min1 min2
        | NN, NZ -> // min = max1 * max2, max = min1 * min2
            calcMin (*) max1 max2, calcMax (*) min1 min2
        | NN, NP -> // min = min1 * max2, max = min1 * min2
            calcMin (*) min1 max2, calcMax (*) min1 min2

        | NZ, PP -> // min = min1 * max2, max = max1 * min2
            calcMin (*) min1 max2, calcMax (*) max1 min2
        | NZ, ZP -> // min = min1 * max2, max = max1 * min2
            calcMin (*) min1 max2, calcMax (*) max1 min2
        | NZ, NN -> // min = max1 * max2, max = min1 * min2
            calcMin (*) max1 max2, calcMax (*) min1 min2
        | NZ, NZ -> // min = max1 * max2, max = min1 * min2
            calcMin (*) max1 max2, calcMax (*) min1 min2
        | NZ, NP -> // min = min1 * max2, max = min1 * min2
            calcMin (*) min1 max2, calcMax (*) min1 min2

        | NP, PP -> // min = min1 * max2, max = max1 * max2
            calcMin (*) min1 max2, calcMax (*) max1 max2
        | NP, ZP -> // min = min1 * max2, max = max1 * max2
            calcMin (*) min1 max2, calcMax (*) max1 max2
        | NP, NN -> // min = max1 * min2, max = min1 * min2
            calcMin (*) max1 min2, calcMax (*) min1 min2
        | NP, NZ -> // min = max1 * min2, max = min1 * min2
            minimize (calcMin (*) min1 max2) (calcMin (*) min2 max1),
            maximize (calcMax (*) max1 max2) (calcMax (*) min1 min2)
        | NP, NP -> // min = min1 * max2, max = max1 * max2
            minimize (calcMin (*) min1 max2) (calcMin (*) min2 max1),
            maximize (calcMax (*) max1 max2) (calcMax (*) min1 min2)


    /// Calculate `Minimum` option and
    /// `Maximum` option for division of
    /// (**min1**, **max1**) and (**min2**, **max2)
    let division min1 max1 min2 max2 =
        match (min1 |> fst, max1 |> fst), (min2 |> fst, max2 |> fst) with
        | PP, PP -> // min = min1 / max2, max =	max1 / min2
            calcMin (/) min1 max2, calcMax (/) max1 min2
        | PP, NN -> // min = max1 / max2	, max = min1 / min2
            calcMin (/) max1 max2, calcMax (/) min1 min2
        | PP, ZP ->
            calcMin (/) min1 max2, calcMax (/) max1 min2

        | ZP, PP -> // min = min1 / max2, max =	max1 / min2
            calcMin (/) min1 max2, calcMax (/) max1 min2
        | ZP, NN -> // min = max1 / max2	, max = min1 / min2
            calcMin (/) max1 max2, calcMax (/) min1 min2
        | ZP, ZP ->
            calcMin (/) min1 max2, calcMax (/) max1 min2

        | NN, PP -> // min = min1 / min2, max = max1 / max2
            calcMin (/) min1 min2, calcMax (/) max1 max2
        | NN, NN -> // min = max1 / min2	, max = min1 / max2
            calcMin (/) max1 min2, calcMax (/) min1 max2
        | NN, NZ ->
            calcMin (/) max1 min2, calcMax (/) min1 max2
        | NN, ZP ->
            calcMin (/) min1 min2, calcMax (/) max1 max2

        | NZ, PP -> // min = min1 / min2, max = max1 / max2
            calcMin (/) min1 min2, calcMax (/) max1 max2
        | NZ, NN -> // min = max1 / min2	, max = min1 / max2
            calcMin (/) max1 min2, calcMax (/) min1 max2
        | NZ, NZ ->
            calcMin (/) max1 min2, calcMax (/) min2 max2

        | NP, PP -> // min = min1 / min2, max = max1 / min2
            calcMin (/) min1 min2, calcMax (/) max1 min2
        | NP, NN -> // min = max1 / max2, max = min1 / max2
            calcMin (/) max1 max2, calcMax (/) min1 max2
        // division by range containing zero
        | NN, NP
        | PP, NP
        | NP, NP
        | NZ, NP
        | ZP, NP

//        | NN, ZP
        | NP, ZP
        | NZ, ZP

        | PP, NZ
        | NP, NZ
        | ZP, NZ -> None, None


    /// Match the right minmax calcultion
    /// according to the operand
    let calcMinMax = function
        | BigRational.Mult  -> multiplication
        | BigRational.Div   -> division
        | BigRational.Add   -> addition
        | BigRational.Subtr -> subtraction
        //| BigRational.NoMatch ->
        //    Exceptions.NotAValidOperator
        //    |> Exceptions.raiseExc



open MathNet.Numerics
open Informedica.Utils.Lib.BCL

open Expecto
open FsCheck
open Types
open Variable.Operators

module Property = Variable.ValueRange.Property


let generateMinMax n =
    let v1 = (Generators.bigRGenerator |> Arb.fromGen).Generator.Sample(4, n)
    let v2 = (Generators.bigRGenerator |> Arb.fromGen).Generator.Sample(4, n)
    let b1 = Arb.generate<bool> |> Gen.sample 10 n
    let b2 = Arb.generate<bool> |> Gen.sample 10 n
    let s1 = Arb.generate<bool> |> Gen.sample 10 n
    let s2 = Arb.generate<bool> |> Gen.sample 10 n
    let min =
        v1
        |> List.zip b1
        |> List.zip s1
        |> List.map (fun (s,(incl, (br))) ->
            if s then None, false
            else
                Some br, incl
        )
    let max =
        v2
        |> List.zip b2
        |> List.zip s2
        |> List.map (fun (s,(incl, (br))) ->
            if s then None, false
            else
                Some br, incl
        )

    min
    |> List.zip max
    |> List.map (fun ((br1, incl1), (br2, incl2)) ->
        match br1, br2 with
        | Some _, Some _ ->
            if br1 > br2 then (br2, incl2), (br1, incl1)
            else
                if br1 = br2 && (incl1 = incl2 || (not incl2)) then (br2, incl2), (br1, incl1)
                else
                   (br1, incl1), (br2, incl2)
        | _ -> (br1, incl1), (br2, incl2)
    )


let minMax1 = generateMinMax 100
let minMax2 = generateMinMax 100


let testCalc s op =
    minMax1
    |> List.allPairs minMax2
    |> List.map (fun ((min1, max1), (min2, max2)) ->
        (min1, max1),
        (min2, max2),
        MinMaxCalcultor.calcMinMax op min1 max1 min2 max2
    )
    |> List.map (fun (r1, r2, r) ->
        let minToStr min =
            min
            |> Option.map (Minimum.toString true)
            |> Option.defaultValue "<"
        let maxToStr max =
            max
            |> Option.map (Maximum.toString true)
            |> Option.defaultValue ">"

        let toMin ((br, b), _) =
            br |> Option.map (fun br ->
                Minimum.create b br
            )
            |> minToStr

        let toMax (_, (br, b)) =
            br |> Option.map (fun br ->
                Maximum.create b br
            )
            |> maxToStr

        $"{r1 |> toMin} .. {r1 |> toMax} {s} {r2 |> toMin} .. {r2 |> toMax} = {r |> fst |> minToStr} .. {r |> snd |> maxToStr} "
    )
    |> List.distinct
    |> List.iteri (printfn "%i. %s")


testCalc "*" (*)
testCalc "/" (/)
testCalc "*" (*)
testCalc "*" (*)


open MinMaxCalcultor

let testMatch () =
    minMax1
    |> List.allPairs minMax2
    |> List.map (fun ((min1, max1), (min2, max2)) ->
        try
            match (min1 |> fst, max1 |> fst), (min2 |> fst, max2 |> fst) with
            | NN, NN
            | NN, NP
            | NN, PP
            | NN, NZ
            | NN, ZP
            | NP, NN
            | NP, NP
            | NP, PP
            | NP, NZ
            | NP, ZP
            | PP, NN
            | PP, NP
            | PP, PP
            | PP, NZ
            | PP, ZP
            | NZ, NN
            | NZ, NP
            | NZ, PP
            | NZ, NZ
            | NZ, ZP
            | ZP, NN
            | ZP, NP
            | ZP, PP
            | ZP, NZ
            | ZP, ZP -> "can match"
        with
        | _ -> $"cannot match {min1}, {max1},{min2}, {max2}"
    )

testMatch ()
|> List.distinct


