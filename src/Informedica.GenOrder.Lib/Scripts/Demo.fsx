

#load "load.fsx"

open Informedica.GenOrder.Lib


Demo.getIndications ()


Examples.listOrders ()


Demo.create
    15.
    "chronische pijn"
    None
    None


Demo.create
    30.
    "behandeling PJP > 5 jaar"
    None //(Some "cotrimoxazol")
    None //(Some "intraveneus")


Demo.create
    10.
    "bloeddruk verhoging"
    None
    None //(Some "intraveneus")



OrderLogger.logger.Start Informedica.GenSolver.Lib.Types.Logging.Level.Informative

// write results to the test.txt in this folder
$"{__SOURCE_DIRECTORY__}/test.txt" |> OrderLogger.logger.Write
