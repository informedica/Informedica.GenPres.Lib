

#load "load.fsx"

open Informedica.GenOrder.Lib

#time


Demo.getIndications ()


Examples.listOrders ()


Demo.create
    15.
    "acute pijn/post operatief"
    None
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
    50.
    "bloeddruk verhoging"
    None
    None //(Some "intraveneus")
|> List.iter (printfn "%s")


Demo.create
    10.
    "ernstige infecties"
    None
    None



OrderLogger.logger.Start Informedica.GenSolver.Lib.Types.Logging.Level.Informative

// write results to the test.txt in this folder
$"{__SOURCE_DIRECTORY__}/test.txt" |> OrderLogger.logger.Write

