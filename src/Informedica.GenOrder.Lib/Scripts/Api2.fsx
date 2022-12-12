

#load "load.fsx"




open MathNet.Numerics
open Informedica.GenSolver.Lib
open Informedica.GenOrder.Lib


let path = Some $"{__SOURCE_DIRECTORY__}/log.txt"
let startLogger () =
    // Start the logger at an informative level
    OrderLogger.logger.Start path Logging.Level.Informative


startLogger()
Api.filter None None None (Some "paracetamol") None None
|> Array.item 2
|> Api.createDrugOrders None
|> List.item 0
|> DrugOrder.toOrder
|> Order.Dto.fromDto
|> Order.solveMinMax { Log = ignore }
|> Order.toString
|> List.iter (printfn "%s")



startLogger()
Api.filter None None None (Some "noradrenaline") (Some "infusievloeistof") None
|> List.item 0


// report output to the fsi
OrderLogger.logger.Report ()


Data.getProducts ()

