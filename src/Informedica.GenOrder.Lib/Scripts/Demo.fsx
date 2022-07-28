

#load "load.fsx"

open Informedica.GenOrder.Lib


Demo.getIndications ()


Demo.create
    50.
    "behandeling PJP"
    None //(Some "cotrimoxazol")
    None //(Some "intraveneus")


Demo.create
    10.
    "bloeddruk verhoging"
    None
    None //(Some "intraveneus")


let doseIV w =
    Demo.filterIndications
        "acute pijn/post operatief"
        (Some "paracetamol")
        (Some "intraveneus")
    |> List.head
    |> fun (_, _, _, d, c) -> c w d

let doseOR w =
    Demo.filterIndications
        "acute pijn/post operatief"
        (Some "paracetamol")
        (Some "oraal")
    |> List.head
    |> fun (_, _, _, d, c) -> c w d

doseIV 10.
doseOR 10.


OrderLogger.logger.Start Informedica.GenSolver.Lib.Types.Logging.Level.Informative

// write results to the test.txt in this folder
$"{__SOURCE_DIRECTORY__}/test.txt" |> OrderLogger.logger.Write
