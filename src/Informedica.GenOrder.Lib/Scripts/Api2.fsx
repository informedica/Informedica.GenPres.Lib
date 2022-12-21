

#load "load.fsx"


#time



open MathNet.Numerics
open Informedica.GenForm.Lib
open Informedica.GenUnits.Lib
open Informedica.GenSolver.Lib
open Informedica.GenOrder.Lib


open Api


let evaluate weight (doseRule : DoseRule) =
    let solRule =
        SolutionRule.getSolutionRules ()
        |> Array.tryFind (fun s ->
            s.Selector.Generic = doseRule.Generic
        )

    let sns =
        doseRule.DoseLimits
        |> Array.map (fun l -> l.Substance)
        |> Array.toList

    doseRule
    |> createDrugOrders solRule
    |> List.map (Api.setAdjust weight)
    |> List.map DrugOrder.toOrder
    |> List.map Order.Dto.fromDto
    |> List.map (Order.solveMinMax { Log = ignore })
//    |> toScenarios doseRule.Indication sns



let applConstrs weight (doseRule : DoseRule) =
    let solRule =
        SolutionRule.getSolutionRules ()
        |> Array.tryFind (fun s ->
            s.Selector.Generic = doseRule.Generic
        )

    doseRule
    |> createDrugOrders solRule
    |> List.map (Api.setAdjust weight)
    |> List.map DrugOrder.toOrder
    |> List.map Order.Dto.fromDto
    |> List.map Order.applyConstraints

//    |> List.map (Order.solveMinMax { Log = ignore })
//    |> toScenarios doseRule.Indication sns


let path = Some $"{__SOURCE_DIRECTORY__}/log.txt"
let startLogger () =
    // Start the logger at an informative level
    OrderLogger.logger.Start path Logging.Level.Informative


startLogger()


Api.filter None None None (Some "gentamicine") None None
|> Array.item 4
//|> applConstrs 50.
|> evaluate 2.
|> List.iter (fun o ->
    o
    |> Order.toString
    |> List.iter (printfn "%s")
)


startLogger()
Api.filter
    None None None
    (Some "noradrenaline")
    (Some "infusievloeistof") None
|> Array.item 0


// report output to the fsi
OrderLogger.logger.Report ()


SolutionRule.getSolutionRules ()

"mL"
|> DrugOrder.unitGroup

661111N/21999996000000000000000N
|> BigRational.ToDouble


4074073333N/14666664000000N
|> BigRational.ToDouble
|> fun x -> x * 3600.

1111111111N/4320000000000000000000N
|> BigRational.ToDouble
|> fun x -> x * 3600. * 1000.

SolutionRule.getSolutionRules ()
|> Array.tryFind (fun s ->
    s.Selector.Generic = "gentamicine"
)


DoseRule.getDoseRules ()
|> DoseRule.filter
    { DoseRule.allFilter with
        Age = 365N |> Some
//        Generic = "paracetamol" |> Some
//        Route = "iv" |> Some
        Weight = 10N * 1000N |> Some
    }
|> DoseRule.patients
|> Array.iteri (printfn "%i. %s")


{
    Diagnosis = ""
    Gender = AnyGender
    Age = { MinMax.none with Maximum = 19N * 365N |> Some }
    Weight = { MinMax.none with Maximum = 6N |> Some }
    BSA = MinMax.none
    GestAge = MinMax.none
    PMAge = MinMax.none
}
|> Patient.filter 
    { DoseRule.allFilter with
//        Age = 301N |> Some
        Weight = 30N |> Some
    }


let test n =
    DoseRule.getDoseRules ()
    |> Array.skip n
    |> Array.take 1
    |> fun xs ->
        xs
        |> DoseRule.patients
        |> Array.iteri (printfn "%i. %s")
        printfn "---"
        xs

    |> DoseRule.filter
        { DoseRule.allFilter with
    //        Age = 10N * 365N |> Some
    //        Generic = "paracetamol" |> Some
    //        Route = "iv" |> Some
            Weight = 30N |> Some
        }
    |> DoseRule.patients
    |> Array.iteri (printfn "%i. %s")


for i in [1..50] do
    printfn $"{i}"
    test i



test 24



let dr =
    DoseRule.getDoseRules()
    |> Array.skip 24
    |> Array.take 1

