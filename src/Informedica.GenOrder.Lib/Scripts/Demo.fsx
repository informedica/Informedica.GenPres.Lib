

#load "load.fsx"

open Informedica.GenOrder.Lib


Demo.getIndications ()


Examples.listOrders ()


Demo.create
    15.
    "acute pijn/post operatief"
    (Some "morfine")
    None
|> List.iter (printfn "%s")


Demo.create
    15.
    "chronische pijn"
    None
    None
|> List.iter (printfn "%s")


Demo.create
    30.
    "behandeling PJP > 5 jaar"
    None //(Some "cotrimoxazol")
    None //(Some "intraveneus")
|> List.iter (printfn "%s")


Demo.create
    10.
    "bloeddruk verhoging"
    None
    None //(Some "intraveneus")
|> List.iter (printfn "%s")



OrderLogger.logger.Start Informedica.GenSolver.Lib.Types.Logging.Level.Informative

// write results to the test.txt in this folder
$"{__SOURCE_DIRECTORY__}/test.txt" |> OrderLogger.logger.Write
