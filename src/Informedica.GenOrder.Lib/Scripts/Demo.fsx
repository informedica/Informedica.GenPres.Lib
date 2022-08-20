

#load "load.fsx"

open Informedica.GenOrder.Lib

#time

let create a w i m r =
    Demo.create a w i m r
    |> List.map Demo.toString



Demo.filter (Some (6 * 365)) 30. None (Some "cotrimoxazol") None

Demo.filter None 30. (Some "cotrimoxazol") None None

Demo.filter None 30. None None None

Demo.filterIndications (Some (6 * 365)) 30. (Some "cotrimoxazol") None

Demo.filterMedications (Some (6 * 365)) 30. None None

Examples.listOrders ()


create
    None
    3.
    None
    (Some "paracetamol")
    (Some "rectaal")
|> List.iter (printfn "%s")


create
    None
    15.
    (Some "chronische pijn")
    None
    None
|> List.iter (printfn "%s")


create
    (Some (10 * 365))
    30.
    (Some "behandeling PJP")
    None //(Some "cotrimoxazol")
    None //(Some "intraveneus")
|> List.iter (printfn "%s")


create
    None
    3.
    (Some "bloeddruk verhoging")
    None
    None //(Some "intraveneus")
|> List.iter (printfn "%s")


create
    None
    50.
    (Some "ernstige infecties")
    (Some "gentamicine")
    None
|> List.iter (printfn "%s")


create
    None
    0.8
    None
    (Some "ondansetron")
    (Some "intraveneus")


create
    None
    4.
    None
    (Some "midazolam")
    None



OrderLogger.logger.Start Informedica.GenSolver.Lib.Types.Logging.Level.Informative


// write results to the test.txt in this folder
$"{__SOURCE_DIRECTORY__}/test.txt" |> OrderLogger.logger.Write

