
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




/// Functions that handle the `Equation` type that
/// either represents a `ProductEquation` </br>
/// y = x1 \* x2 * ... \* xn </br>
/// or a `SumEquations` </br>
/// y = x1 + x2 + ... + xn
module Equation =

    open Types
    open Variable.Operators

    module ValueRange = Variable.ValueRange
    
    module Exception =

        /// Equation exception
        exception EquationException of Exceptions.Message

        /// Raise an `EquationException` with `Message` `m`.
        let raiseExc m = m |> EquationException |> raise


    /// Create an `Equation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    /// The type of Equation product or sum
    /// is determined by the constructor **c**.
    let create c succ fail (y, xs) = 
        y::xs
        |> List.filter (fun v ->
            y::xs
            |> List.filter (Variable.eqName v) |> List.length > 1)
        |> function
        | [] -> (y, xs) |> c |> succ
        | duplicates -> 
            duplicates 
            |> Exceptions.EquationDuplicateVariables 
            |> fail

    /// Create an `ProductEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    let createProductEq = create ProductEquation

    /// Create an `SumEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    let createSumEq = create SumEquation

    /// Create an `ProductEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time raising an exception.
    let createProductEqExc = createProductEq id Exception.raiseExc 

    /// Create an `SumEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time raising an exception.
    let createSumEqExc = createSumEq id Exception.raiseExc

    /// Apply **fp** to a `ProductEquation` and
    /// **fs** to a `SumEquation`.
    let apply fp fs = function
        | ProductEquation (y,xs) -> fp y xs
        | SumEquation (y, xs)    -> fs y xs

    /// Check whether an `Equation` is a product equation
    let isProduct = apply (fun _ _ -> true) (fun _ _ -> false)

    /// Check whether an `Equation` is a sum equation
    let isSum = apply (fun _ _ -> true) (fun _ _ -> false)

    /// Turn an `Equation` into a list of `Variable`
    let toVars = 
        let f y xs = y::xs
        apply f f

    let count eq = 
        eq
        |> toVars
        |> List.fold (fun (acc : int) v ->
            (+) (v |> Variable.count) acc
        ) 0

    let countProduct eq = 
        match eq with
        | SumEquation _ -> -1
        | _ ->
            eq
            |> toVars
            |> List.fold (fun acc v ->
                let c = v |> Variable.count
                (if c = 0 then 1 else c) * acc
            ) 1


    let toString exact eq = 
        let op = if eq |> isProduct then " * " else " + "
        let varToString = Variable.toString exact

        match eq |> toVars with
        | [] -> ""
        | _::[] -> ""
        | y::xs ->
            $"""{y |> varToString} = {xs |> List.map varToString |> String.concat op}"""


    /// Make sure that the `Variables` in the
    /// `Equation` can only contain positive 
    /// non zero values.
    let nonZeroOrNegative eq =
        let set c y xs =
            let y = y |> Variable.setNonZeroOrNegative
            let xs = xs |> List.map Variable.setNonZeroOrNegative
            (y, xs) |> c 
        let fp = set ProductEquation
        let fs = set SumEquation
        eq |> apply fp fs

    /// Check whether an `Equation` contains
    /// a `Variable` **v**
    let contains v = toVars >> (List.exists (Variable.eqName v))

    /// Check whether `Equation`s 
    /// **eq1** and **eq2** are equal
    let equals eq1 eq2 = 
        let vrs1 = eq1 |> toVars
        let vrs2 = eq2 |> toVars
        vrs1 |> List.forall (fun vr -> 
            vrs2 |> List.exists (Variable.eqName vr)) &&
        ((eq1 |> isProduct) && (eq2 |> isProduct) ||
         (eq1 |> isSum)     && (eq2 |> isSum))

    /// Find a `Variable` **vr** in
    /// an `Equation` **eq** and return
    /// the result in a list
    let find var eq =
        eq
        |> toVars
        |> List.filter (fun v -> v |> Variable.getName = (var |> Variable.getName))

    /// Find a `Variable` with `Name`
    /// **n** in an `Equation` **eq**
    /// and return the result as a list
    let findName n eq =
        eq
        |> toVars
        |> List.filter (fun vr -> vr |> Variable.getName = n)

    /// Replace a `Variable` **v** in the 
    /// `Equation` **e**.
    let replace var eq =
        let r c v vs =
            let vs = vs |> List.replace ((Variable.eqName) v) v
            c id (fun _ -> eq) ((vs |> List.head), (vs|> List.tail))
        let fp y xs = r createProductEq var (y::xs)
        let fs y xs = r createSumEq var (y::xs)
        eq |> apply fp fs


    // Check whether an equation is solved
    let isSolved = function
        | ProductEquation (y, xs) 
        | SumEquation (y, xs) ->
            y::xs |> List.forall Variable.isSolved


    // Check whether an equation will change by calc
    // This is not the same as `isSolved`!! If all 
    // the variables are unrestricted than the equation
    // is not solvable but is also not solved.
    let isSolvable = function 
        | ProductEquation (y, xs)
        | SumEquation (y, xs) ->
            let es = y::xs
            es |> List.exists Variable.isSolvable &&
            es |> List.filter Variable.isUnrestricted
               |> List.length > 1
               |> not


    let check eq = 
        let issub op (y : Variable) (xs : Variable list) =
            match xs with
            | [] -> true
            | _  ->
                if y.Values |> ValueRange.isValueSet &&
                   xs |> List.map Variable.getValueRange
                      |> List.forall ValueRange.isValueSet then
                
                    y.Values
                    |> ValueRange.isSubSetOf (xs |> List.reduce (op)).Values

                else true

        if eq |> isSolvable then
            match eq with
            | ProductEquation (y, xs) -> xs |> issub (^*) y
            | SumEquation (y, xs) -> xs |> issub (^+) y

        else true

    let calculationToString op1 op2 x y xs =
        let varToStr = Variable.toString true
        let opToStr op  = $" {op |> Variable.Operators.toString} "
        let filter x xs = xs |> List.filter (Variable.eqName x >> not)

        $"""{x |> varToStr} = {y |> varToStr}{op2 |> opToStr}{xs |> filter x |> List.map varToStr |> String.concat (op1 |> opToStr)} """

    /// Solve an equation **e**, return a list of
    /// changed `Variable`s. 
    let solve log eq =
        eq
        |> Events.EquationStartedSolving
        |> Logging.logInfo log

        let filter x xs = xs |> List.filter (Variable.eqName x >> not)
        let replAdd x xs = xs |> List.replaceOrAdd(Variable.eqName x) x
        let calcToStr = calculationToString

        if eq |> isSolved then eq, Unchanged
        else
            let rec calc op1 op2 y xs rest changed =
                (y::xs)
                |> Events.EquationStartedCalculation
                |> Logging.logInfo log

                match rest with 
                | []  -> 
                    //(changed, xs)
                    //need to change this!
                    ([], xs)
                    |> Events.EquationFinishedCalculation
                    |> Logging.logInfo log

                    changed, xs
                | x::tail ->
                    let newX =
                        match xs with
                        | [] -> x <== y 
                        | _  ->
                            calcToStr op1 op2 x y xs
                            |> printfn "calculating: %s"
                            x <== (y |> op2 <| (xs |> filter x |> List.reduce op1))

                    if x = newX then changed 
                    else
                        newX
                        |> Events.EquationVariableChanged
                        |> Logging.logInfo log
                            
                        changed
                        |> List.replaceOrAdd
                            (fst >> Variable.eqName newX)
                            (newX, newX.Values |> ValueRange.diffWith x.Values)

                    |> calc op1 op2 y (xs |> replAdd newX) tail

            // op1 = (*) or (+) and op2 = (/) or (-)
            let rec loop op1 op2 y xs changed =
            
                // Calculate y = x1 op1 x2 op1 .. op1 xn
                let ychanged, y =
                    (y::xs)
                    |> Events.EquationStartedCalculation
                    |> Logging.logInfo log

                    calcToStr op1 op1 y (xs |> List.head) (xs |> List.tail)
                    |> printfn "calculating: %s"
                    let newY = y <== (xs |> List.reduce op1)

                    if newY = y then [], y
                    else
                        [ newY, newY.Values |> ValueRange.diffWith y.Values ], newY
                    //let x = xs  |> List.head
                    //let xs = xs |> List.filter ((<>) x)
                    //calc op1 op1 x xs [y] []
            
                // Replace y with the new y with is in a list
                // let y = ys |> List.head
            
                // Calculate x1 = y op2 (x2 op1 x3 .. op1 xn)
                //       and x2 = y op2 (x1 op1 x3 .. op1 xn)
                //       etc..
                let xchanged, xs = calc op1 op2 y xs xs []

                // If something has changed restart until nothing changes anymore
                // or only has to run once
                match ychanged @ xchanged with
                | [] ->
                    changed
                    |> List.map fst
                    |> Events.EquationFinishedSolving
                    |> Logging.logInfo  log

                    let eq =
                        match eq with
                        | ProductEquation _ -> createProductEqExc (y, xs)
                        | SumEquation _     -> createSumEqExc (y, xs)
                    eq, Changed changed
                | _ ->
                    let changed = changed @ ychanged @ xchanged
                    (false, y, xs, [])
                    |> Events.EquationLoopedSolving
                    |> Logging.logInfo log

                    loop op1 op2 y xs changed
            
            let y, xs, op1, op2 =
                match eq with
                | ProductEquation (y, xs) -> y, xs, (^*), (^/)
                | SumEquation     (y, xs) -> y, xs, (^+), (^-)
                        
            match xs with 
            | [] -> eq, Unchanged
            | _  ->
                try 
                    loop op1 op2 y xs []
                with
                | Variable.Exceptions.VariableException m -> 
                    m 
                    |> Logging.logError log

                    eq
                    |> Events.EquationCouldNotBeSolved
                    |> Logging.logWarning log

                    m 
                    |> Variable.Exceptions.raiseExc


    module Dto =

        type VariableDto = Variable.Dto.Dto

        /// `Dto` for an `Equation`
        type Dto = { Vars: VariableDto[]; IsProdEq: bool }

        /// Create a `Dto` with `vars` (variable dto array)
        /// that is either a `ProductEquation` or a `SumEquation`
        let create isProd vars  = { Vars = vars; IsProdEq = isProd }

        /// Create a `ProductEquation` `Dto`
        let createProd = create true

        /// Create a `SumEquation` `Dto`
        let createSum  = create false

        /// Return the `string` representation of a `Dto`
        let toString exact (dto: Dto) = 
            let op = if dto.IsProdEq then "*" else "+"
            let varToString = Variable.Dto.toString exact

            match dto.Vars |> Array.toList with
            | [] -> ""
            | _::[] -> ""
            | y::xs -> 
                let s = 
                    sprintf "%s = " (y |> varToString) + 
                    (xs |> List.fold (fun s v -> s + (v |> varToString) + " " + op + " ") "")
                s.Substring(0, s.Length - 2)


        /// Create a `Dto` and raise an exception if it fails
        let fromDto dto =
            let succ = id
            let fail = Exception.raiseExc

            match dto.Vars |> Array.toList with
            | [] -> Exceptions.EquationEmptyVariableList |> fail
            | y::xs ->
                let y = y |> Variable.Dto.fromDto
                let e = (y, xs |> List.map Variable.Dto.fromDto)
            
                if dto.IsProdEq then 
                    e 
                    |> createProductEq succ fail
                else 
                    e 
                    |> createSumEq succ fail

        /// Create a `Dto` from an `Equation` **e**
        let toDto e =
            let c isProd y xs =
                { Vars = y::xs |> List.map Variable.Dto.toDto |> List.toArray; IsProdEq = isProd }
            
            let fp = c true 
            let fs = c false 
            
            e |> apply fp fs



/// Create the necessary test generators
module Generators =

    open Types
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


                let run () = tests |> Generators.run


            open Variable.ValueRange

            [<Tests>]
            let tests = testList "valuerange" [
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


            let run () = tests |> Generators.run

        open Variable

        [<Tests>]
        let tests = testList "variable dto" [
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

        let run () = tests |> Generators.run

    module EquationTests =

        [<Tests>]
        let tests = testList "product equation" [
            
            ]


open Tests
open UtilsTests
open VariableTests
open ValueRangeTests


BigRationalTests.run ()
ListTests.run ()
SetTests.run ()
MinimumTests.run ()
MaximumTests.run ()
IncrementTests.run ()
ValueRangeTests.run ()
VariableTests.run ()


open MathNet.Numerics
open Informedica.Utils.Lib.BCL

open Expecto
open FsCheck
open Types
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
    |> List.map (Variable.setNonZeroOrNegative)
    |> List.distinctBy (fun var ->
        var.Values
    )


let vars1 = generateVars 100
let vars2 = generateVars 100
let vars3 = generateVars 100


Seq.allPairs vars1 vars2
|> Seq.collect (fun (var1, var2) ->
    try
        [
            var1, var2, var1 ^* var2, "x"
            var1, var2, var1 ^/ var2, "/"
            var1, var2, var1 ^+ var2, "+"
            var1, var2, var1 ^- var2, "-"
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
    $"{i}. {y} = {x} {op} {z}"
    |> printfn "%s"    
)



Seq.allPairs vars1 vars2
|> Seq.allPairs vars3
|> Seq.distinct
|> Seq.collect (fun (y, (x1, x2)) ->
    try
        [
            y, y <== (x1 ^* x2), x1, x2, "x"
            y, y <== (x1 ^/ x2), x1, x2, "/"
            y, y <== (x1 ^+ x2), x1, x2, "+"
            y, y <== (x1 ^- x2), x1, x2, "-"
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
    $"{i}. {y0} -> {y1} = {x} {o} {z}"
    |> printfn "%s"
)


vars1
|> Seq.map (fun vr ->
    vr.Values, vr.Values |> Variable.ValueRange.toProperties
)
|> Seq.distinct
|> Seq.iteri (fun i (vr, props) ->
    printfn $"{i}. {vr |> Variable.ValueRange.toString true} -> {props}"
)



vars1
|> Seq.collect (fun var ->
    try
    let max = Variable.ValueRange.Maximum.create true 2N
    let v =
        var.Values
        |> Variable.ValueRange.setMax max
        |> Variable.setValueRange var
    [ var.Values, v.Values, v.Values |> Variable.ValueRange.diffWith var.Values ]
    with
    | _ -> []
)
|> Seq.distinct
|> Seq.iteri (fun i (vr1, vr2, props) ->
    printfn $"{i}. {vr1 |> Variable.ValueRange.toString true} -> {vr2 |> Variable.ValueRange.toString true} diff = {props}"
)


let resultToString = function
    | (eq, Unchanged)  -> "Unchanged"
    | (eq, Changed cs) ->
        let toStr (var : Variable, props)  =
            $"""changes: {var.Name |> Variable.Name.toString}: {props |> Set.map (Property.toString true) |> String.concat ", "}"""
        $"""
{eq |> Equation.toString true} {if cs |> List.isEmpty then "" else cs |> List.map toStr |> String.concat ", "}
    """       


vars1
|> Seq.allPairs vars2
|> Seq.allPairs vars3
|> Seq.choose (fun (y, (x1, x2)) ->
    try
        Equation.create ProductEquation Some (fun _ -> None) (y, [x1; x2])
        |> function
        | Some eq ->
            printfn $"== start solving equation"
            (eq, eq |> Equation.solve ({ Log = ignore })) |> Some
        | None -> None
    with
    | _ ->
        printfn "== cannot solve equation"
        None
)
|> Seq.take 1000
|> Seq.iteri (fun i (eq, res) ->
    printfn $"== finished solving equation {i}"
    $"{eq |> Equation.toString true} ==> {res |> resultToString}"
    |> printfn "%s"
)


Property.createMinInclProp 1N
|> Property.toString true


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


// var_8 [3..4] = var_10 <..2..> * var_7 <3..> ==> [var_10: ..0]]
Equation.createProductEqExc (var0, [var1; var2])
|> fun eq ->
    printfn $"{eq |> Equation.toString true}"
    eq
|> Equation.solve ({ Log = printfn "%A" })
|> resultToString
|> printfn "%s"


Variable.ValueRange.MinMaxCalcultor.calcMinMax
    (*)
    (Some 3N, false)
    (None, false)
    (None, false)
    (Some 0N, true)


var0 <== var2 ^* var1
