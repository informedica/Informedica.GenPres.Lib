
#load "load.fsx"

open MathNet.Numerics
open Informedica.GenOrder.Lib

Api.filter None None None (Some "paracetamol") (Some "drank") None
|> List.item 1
|> Api.evaluate None 10N
|> List.map Api.translate

// Start the logger at an informative level
OrderLogger.logger.Start Informedica.GenSolver.Lib.Types.Logging.Level.Informative

// report output to the fsi
OrderLogger.logger.Report ()

// write results to the test.txt in this folder
$"{__SOURCE_DIRECTORY__}/test.txt"
|> OrderLogger.logger.Write


Api.filter None None None (Some "paracetamol") (Some "drank") None
|> List.item 1
|> fun dr ->
    dr
    |> Api.createDrugOrder
    |> DrugOrder.toConstrainedOrder
    |> DrugOrder.setDoseRule dr
    |> DrugOrder.setAdjust dr.Medication 10N
    |> fst
    |> List.sortBy (fun c -> c.Mapping)
    |> List.iteri (printfn "%i. %A")
