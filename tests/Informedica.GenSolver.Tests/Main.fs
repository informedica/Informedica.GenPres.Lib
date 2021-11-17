module Main
open Expecto

let config = 
    {
        defaultConfig with
            verbosity = Logging.Verbose
    }

[<EntryPoint>]
let main argv =
    argv |> Array.append [|"--summary"|]
    |> Tests.runTestsInAssembly defaultConfig 
