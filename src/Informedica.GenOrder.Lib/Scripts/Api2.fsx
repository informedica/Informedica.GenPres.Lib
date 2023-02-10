

#load "load.fsx"


#time



open MathNet.Numerics
open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.GenForm.Lib
open Informedica.GenUnits.Lib
open Informedica.GenSolver.Lib
open Informedica.GenOrder.Lib


open Api



let path = Some $"{__SOURCE_DIRECTORY__}/log.txt"
let startLogger () =
    // Start the logger at an informative level
    OrderLogger.logger.Start path Logging.Level.Informative


module Api =

    open System
    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL
    open Informedica.GenForm.Lib
    open Informedica.GenOrder.Lib


    let tryHead m = (Array.map m) >> Array.tryHead >> (Option.defaultValue "")


    let createProductComponent noSubst freqUnit (doseLimits : DoseLimit []) (ps : Product []) =
        { DrugOrder.productComponent with
            Name =
                ps
                |> tryHead (fun p -> p.Shape)
                |> fun s ->
                    if s |> String.isNullOrWhiteSpace then "oplosvloeistof"
                    else s
            Shape =
                ps
                |> tryHead (fun p -> p.Shape)
                |> fun s ->
                    if s |> String.isNullOrWhiteSpace then "oplosvloeistof"
                    else s
            Quantities =
                ps
                |> Array.collect (fun p -> p.ShapeQuantities)
                |> Array.distinct
                |> Array.toList
            TimeUnit = freqUnit
            RateUnit = "uur" //doseRule.RateUnit
            Divisible =
                ps
                |> Array.choose (fun p -> p.Divisible)
                |> Array.tryHead
                |> Option.defaultValue 1N
            Substances =
                if noSubst then []
                else
                    ps
                    |> Array.collect (fun p -> p.Substances)
                    |> Array.groupBy (fun s -> s.Name)
                    |> Array.map (fun (n, xs) ->
                        {
                            Name = n
                            Concentrations =
                                xs
                                |> Array.choose (fun s -> s.Quantity)
                                |> Array.distinct
                                |> Array.toList
                            Unit = xs |> tryHead (fun x -> x.Unit)
                            TimeUnit = freqUnit
                            Dose =
                                doseLimits
                                |> Array.tryFind (fun l -> l.Substance = n)
                            Solution = None
                        }
                    )
                    |> Array.toList
        }


    let setSolutionLimit (sls : SolutionLimit[]) (items : SubstanceItem list) =
        items
        |> List.map (fun item ->
            match sls |> Array.tryFind (fun sl -> sl.Substance |> String.equalsCapInsens item.Name) with
            | None -> item
            | Some sl ->
                { item with
                    Solution = Some sl
                }
        )


    let createDrugOrder (pr : PrescriptionRule) =
        let parenteral = Product.Parenteral.get ()
        let au =
            if pr.DoseRule.AdjustUnit |> String.isNullOrWhiteSpace then "kg"
            else pr.DoseRule.AdjustUnit

        let dose =
            pr.DoseRule.DoseLimits
            |> Array.filter (fun dl -> dl.Substance |> String.isNullOrWhiteSpace)
            |> function
            | [|dl|] -> dl |> Some
            | _ -> None

        let noSubst =
            dose
            |> Option.map (fun d -> d.DoseUnit = "keer")
            |> Option.defaultValue false
        //ToDo ??
        let prods =
            pr.DoseRule.Products
            |> createProductComponent noSubst pr.DoseRule.FreqUnit pr.DoseRule.DoseLimits
            |> List.singleton

        { DrugOrder.drugOrder with
            Id = Guid.NewGuid().ToString()
            Name = pr.DoseRule.Generic
            Products =
                pr.DoseRule.Products
                |> createProductComponent noSubst pr.DoseRule.FreqUnit pr.DoseRule.DoseLimits
                |> List.singleton
            Quantities = []
            Frequencies = pr.DoseRule.Frequencies |> Array.toList
            FreqUnit = pr.DoseRule.FreqUnit
            Unit =
                pr.DoseRule.Products
                |> tryHead (fun p -> p.ShapeUnit)
            Time = pr.DoseRule.Time
            TimeUnit = pr.DoseRule.TimeUnit
            RateUnit = "uur"
            Route = pr.DoseRule.Route
            DoseCount =
                if pr.SolutionRule.IsNone then Some 1N
                else None
            OrderType =
                match pr.DoseRule.DoseType with
                | Informedica.GenForm.Lib.Types.Continuous -> ContinuousOrder
                | _ when pr.DoseRule.TimeUnit |> String.isNullOrWhiteSpace -> DiscontinuousOrder
                | _ -> TimedOrder
            Dose = dose
            Adjust =
                if au = "kg" then
                    pr.Patient.Weight
                    |> Option.map (fun v -> v / 1000N)
                else pr.Patient |> Patient.calcBSA
            AdjustUnit = au
        }
        |> fun dro ->
                match pr.SolutionRule with
                | None -> dro
                | Some sr ->
//                    printfn "found solutionrule"
                    { dro with
                        Quantities = sr.Volumes |> Array.toList
                        DoseCount = sr.DosePerc.Maximum
                        Products =
                            let ps =
                                dro.Products
                                |> List.map (fun p ->
                                    { p with
                                        Substances =
                                            p.Substances
                                            |> setSolutionLimit sr.SolutionLimits
                                    }
                                )

                            let s =
                                // ugly hack to get default solution
                                sr.Solutions
                                |> Array.tryHead
                                |> Option.defaultValue "x"

                            parenteral
                            |> Array.tryFind (fun p -> p.Generic |> String.startsWith s)
                            |> function
                            | Some p ->
//                                printfn $"adding solution {p.Generic}"
                                [|p|]
                                |> createProductComponent true pr.DoseRule.FreqUnit [||]
                                |> List.singleton
                                |> List.append ps
                            | None ->
                                printfn $"couldn't find {s} in parenterals"
                                ps
                    }

    let evaluate (dr : DrugOrder) =
        dr
        |> DrugOrder.toOrder
        |> Order.Dto.fromDto
        |> Order.solveMinMax true { Log = ignore }


    // print an order list
    let toScenarios ind sn (sc : Order list) =
        sc
        |> List.mapi (fun i o ->
            o
            |> Order.Print.printPrescription sn
            |> fun (pres, prep, adm) ->
                {
                    No = i
                    Indication = ind
                    Name = o.Orderable.Name |> Informedica.GenSolver.Lib.Variable.Name.toString
                    Shape = o.Orderable.Components[0].Shape
                    Route = o.Route
                    Prescription = pres
                    Preparation = prep
                    Administration = adm
                }
        )



let evaluate logger (rule : PrescriptionRule) =
    let rec solve retry pr =
        pr
        |> Api.createDrugOrder
        |> DrugOrder.toOrder
        |> Order.Dto.fromDto
        |> Order.solveMinMax false logger
        |> function
        | Ok ord ->
            let dto = ord |> Order.Dto.toDto

            let shps =
                dto.Orderable.Components
                |> List.collect (fun cDto -> cDto.ComponentQuantity.Variable.Vals)
                |> List.toArray

            let sbsts =
                dto.Orderable.Components
                |> List.toArray
                |> Array.collect (fun cDto ->
                    cDto.Items
                    |> List.toArray
                    |> Array.collect (fun iDto ->
                        iDto.ComponentConcentration.Variable.Vals
                        |> List.toArray
                        |> Array.map (fun v -> iDto.Name, v |> Some)
                    )
                )
                |> Array.distinct

            let pr =
                pr
                |> PrescriptionRule.filterProducts
                    shps
                    sbsts

            Ok (ord, pr)
        | Error _ when retry ->
            printfn "trying a second time with manual product"
            { pr with
                DoseRule =
                    { pr.DoseRule with
                        Products =
                            pr.DoseRule.Products
                            |> Array.choose Product.manual
                    }
            }
            |> solve false
        | Error (ord, m) -> Error (ord, pr, m)

    solve true rule


let test pat n =
    let pr =
        pat
        |> PrescriptionRule.get
        |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
        |> Array.item n

    pr
    |> evaluate { Log = ignore }
    |> function
    | Ok (ord, pr) ->
        let ns =
            pr.DoseRule.DoseLimits
            |> Array.map (fun dl -> dl.Substance)
        let o =
            ord
            |> Order.Print.printPrescription ns
        let p =
            $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}"
        Ok (pat, p, o)
    | Error (ord, pr, m) ->
        let o =
            ord
            |> Order.toString
            |> String.concat "\n"
        let p =
            $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}"

        Error ($"%A{m}", p, o)


type Age = Patient.Optics.Age
type Weight = Patient.Optics.Weight
type Height = Patient.Optics.Height



let premature =
    Patient.patient

    |> Patient.Optics.setAge [ 1 |> Age.Weeks]
    |> Patient.Optics.setGestAge [ 32 |> Age.Weeks ]
    |> Patient.Optics.setWeight (1200 |> Weight.Gram |> Some)
    |> Patient.Optics.setHeight (45 |> Height.Centimeter |> Some)
    |> Patient.Optics.setDepartment "NEO"


let newBorn =
    Patient.patient

    |> Patient.Optics.setAge [ 1 |> Age.Weeks]
    |> Patient.Optics.setWeight (3.5m |> Weight.Kilogram |> Some)
    |> Patient.Optics.setHeight (60 |> Height.Centimeter |> Some)
    |> Patient.Optics.setDepartment "ICK"


let infant =
    Patient.patient

    |> Patient.Optics.setAge [ 1 |> Age.Years]
    |> Patient.Optics.setWeight (11.5m |> Weight.Kilogram |> Some)
    |> Patient.Optics.setHeight (70 |> Height.Centimeter |> Some)
    |> Patient.Optics.setDepartment "ICK"


let toddler =
    Patient.patient

    |> Patient.Optics.setAge [ 3 |> Age.Years]
    |> Patient.Optics.setWeight (15m |> Weight.Kilogram |> Some)
    |> Patient.Optics.setHeight (90 |> Height.Centimeter |> Some)
    |> Patient.Optics.setDepartment "ICK"


let child =
    Patient.patient

    |> Patient.Optics.setAge [ 4 |> Age.Years]
    |> Patient.Optics.setWeight (17m |> Weight.Kilogram |> Some)
    |> Patient.Optics.setHeight (100 |> Height.Centimeter |> Some)
    |> Patient.Optics.setDepartment "ICK"


let teenager =
    Patient.patient

    |> Patient.Optics.setAge [ 12 |> Age.Years]
    |> Patient.Optics.setWeight (40m |> Weight.Kilogram |> Some)
    |> Patient.Optics.setHeight (150 |> Height.Centimeter |> Some)
    |> Patient.Optics.setDepartment "ICK"


let adult =
    Patient.patient

    |> Patient.Optics.setAge [ 18 |> Age.Years]
    |> Patient.Optics.setWeight (70m |> Weight.Kilogram |> Some)
    |> Patient.Optics.setHeight (180 |> Height.Centimeter |> Some)
    |> Patient.Optics.setDepartment "ICK"


let getN pat =
    pat
    |> PrescriptionRule.get
    |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
    |> Array.length


let run n pat =
    for i in [0..n-1] do
        try
            i
            |> test pat
            |> function
            | Ok (pat, ind, (prs, prep, adm)) ->
                [
                    ""
                    $"{i}"
                    $"Patient: {pat |> Patient.toString}"
                    $"Indicatie: {ind}"
                    $"Voorschrift: {prs}"
                    if prep |> String.notEmpty then $"Bereiding: {prep}"
                    $"Toediening: {adm}"
                    ""
                ]
                |> String.concat "\n"
            | Error (_, p, _) -> $"\n{i}.Fail: {p}\n"
        with
        | _ ->
            let pr =
                pat
                |> PrescriptionRule.get
                |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
                |> Array.item i
                |> fun pr ->
                    $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}"

            $"\n{i}. could not calculate: {pr}\n"
        |>  File.appendTextToFile path.Value


let getRule i pat =
    pat
    |> PrescriptionRule.get
    |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
    |> Array.item i



[
    premature
    newBorn
    infant
    toddler
    child
    teenager
    adult
]
|> List.skip 4
|> List.take 1
|> List.iter (fun pat ->
    let n = getN pat
    printfn $"=== Running pat: {pat |> Patient.toString}: {n} ==="

    pat
    |> run n
)


test infant 408
|> function
| Ok (pat, ind, (prs, prep, adm)) ->
    [
        $"Patient: {pat |> Patient.toString}"
        $"Indicatie: {ind}"
        $"Voorschrift: {prs}"
        if prep |> String.notEmpty then $"Bereiding: {prep}"
        $"Toediening: {adm}"
    ]
    |> List.iter (printfn "%s")
| Error _ -> ()


startLogger ()


infant
|> getRule 2
|> Api.createDrugOrder
|> DrugOrder.toOrder
|> Order.Dto.fromDto
|> Order.applyConstraints
//|> Order.toString
|> Order.solveMinMax true OrderLogger.logger.Logger
|> function
| Error (ord, msgs) ->
    printfn "oeps error"
    printfn $"{msgs |> List.map string}"
    ord
    |> Order.toString
    |> String.concat "\n"
    |> printfn "%s"

| Ok ord  ->
    ord
    |> Order.toString
    |> String.concat "\n"
    |> printfn "%s"


open Order

try
    let ord =
        infant
        |> getRule 7
        |> Api.createDrugOrder
        |> DrugOrder.toOrder
        |> Order.Dto.fromDto
        |> Order.applyConstraints

    let mapping =
        match ord.Prescription with
        | Continuous -> Mapping.continuous
        | Discontinuous _ -> Mapping.discontinuous
        | Timed _ -> Mapping.timed
        |> Mapping.getEquations
        |> Mapping.getEqsMapping ord

    printfn $"{mapping}"

    let oEqs =
        ord
        |> mapToEquations mapping

    oEqs
//    |> Solver.mapToSolverEqs



with
| :? Informedica.GenSolver.Lib.Exceptions.SolverException as e ->
    printfn $"{e.Data0}"
    raise e


