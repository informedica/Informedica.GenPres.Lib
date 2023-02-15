

#load "load.fsx"


#time



open MathNet.Numerics
open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.GenForm.Lib
open Informedica.GenUnits.Lib
open Informedica.GenSolver.Lib
open Informedica.GenOrder.Lib




let path = Some $"{__SOURCE_DIRECTORY__}/log.txt"
let startLogger () =
    // Start the logger at an informative level
    OrderLogger.logger.Start path Logging.Level.Informative
let stopLogger () = OrderLogger.logger.Stop ()



let test pat n =
    let pr =
        pat
        |> PrescriptionRule.get
        |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
        |> Array.item n

    pr
    |> Api.evaluate { Log = ignore }
    |> Array.map (function
        | Ok (ord, pr) ->
            let ns =
                pr.DoseRule.DoseLimits
                |> Array.map (fun dl -> dl.Substance)
            let o =
                ord
                |> Order.Print.printPrescription ns
            let p =
                $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.DoseType} {pr.DoseRule.Indication}"
            Ok (pat, p, o)
        | Error (ord, pr, m) ->
            let o =
                ord
                |> Order.toString
                |> String.concat "\n"
            let p =
                $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}"

            Error ($"%A{m}", p, o)
    )



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
    |> fun p -> { p with Location = CVL}


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
            |> Array.map (function
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
            )
            |> String.concat "\n"

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



test child 9
|> Array.iter (function
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
)


startLogger ()
stopLogger ()

child
|> getRule 9 |> Api.evaluate (OrderLogger.logger.Logger)
|> fun pr -> pr |> Api.createDrugOrder (pr.SolutionRules[0] |> Some)  //|> printfn "%A"
|> DrugOrder.toOrder
|> Order.Dto.fromDto
|> Order.applyConstraints //|> Order.toString |> List.iter (printfn "%s")
|> Order.solveMinMax true OrderLogger.logger.Logger
|> function
| Error (ord, msgs) ->
    printfn "oeps error"
    // printfn $"{msgs |> List.map string}"
    // ord
    // |> Order.toString
    // |> String.concat "\n"
    // |> printfn "%s"

| Ok ord  ->
//    ord.Orderable.OrderableQuantity
//    |> printfn "%A"

    ord
    |> Order.toString
    |> String.concat "\n"
    |> printfn "%s"



open Order

try
    let ord =
        child
        |> getRule 703
        |> Api.createDrugOrder None
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
    |> Solver.mapToSolverEqs



with
| :? Informedica.GenSolver.Lib.Exceptions.SolverException as e ->
    printfn $"{e.Data0}"
    raise e





let testDto =
    infant
    |> getRule 5
    |> Api.createDrugOrder None
    |> DrugOrder.toOrder


testDto.Orderable.DoseCount.Constraints.Vals

(8. * 60. + 26.)/ 928.