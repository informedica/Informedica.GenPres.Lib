

#load "load.fsx"

open MathNet.Numerics
open Informedica.GenOrder.Lib


Api.filter None None None (Some "paracetamol") (Some "infusievloeistof") None
|> List.item 0
|> Api.evaluate None (6N)
|> List.map Api.translate
|> List.iter (printfn "%A")


// Start the logger at an informative level
OrderLogger.logger.Start Informedica.GenSolver.Lib.Types.Logging.Level.Informative

// report output to the fsi
OrderLogger.logger.Report ()


// write results to the test.txt in this folder
$"{__SOURCE_DIRECTORY__}/log.txt"
|> OrderLogger.logger.Write


Api.filter None None None (Some "gentamicine") None None
|> List.item 0
|> fun dr ->
    let solRule =
        Data.getSolutions ()
        |> List.tryFind (fun s -> s.Medication = dr.Medication)
    dr
    |> Api.createDrugOrders solRule
    |> List.map (DrugOrder.toConstrainedOrder true)
    |> List.map (DrugOrder.setDoseRule dr)
    |> fun xs ->
        if solRule.IsNone then xs
        else
            xs
            |> List.map (DrugOrder.setSolutionRule true solRule.Value)
    |> List.map (DrugOrder.setAdjust dr.Medication 10N)
    |> List.collect fst
    |> List.sortBy (fun c -> c.Mapping)
    |> List.iteri (printfn "%i. %A")

