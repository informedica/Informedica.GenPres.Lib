

#load "load.fsx"

open Informedica.GenOrder.Lib

#time


Demo.filterIndications (Some "cotrimoxazol") None

Demo.filterMedications None None

Examples.listOrders ()


Demo.create
    15.
    (Some "acute pijn/post operatief")
    None
    None
|> List.iter (printfn "%s")


Demo.create
    15.
    (Some "chronische pijn")
    None
    None
|> List.iter (printfn "%s")


Demo.create
    30.
    (Some "behandeling PJP > 5 jaar")
    None //(Some "cotrimoxazol")
    None //(Some "intraveneus")
|> List.iter (printfn "%s")


Demo.create
    50.
    (Some "bloeddruk verhoging")
    None
    None //(Some "intraveneus")
|> List.iter (printfn "%s")


Demo.create
    10.
    (Some "ernstige infecties")
    None
    None


Demo.create 10. None None None
|> List.iter (printfn "%s")


OrderLogger.logger.Start Informedica.GenSolver.Lib.Types.Logging.Level.Informative


// write results to the test.txt in this folder
$"{__SOURCE_DIRECTORY__}/test.txt" |> OrderLogger.logger.Write

