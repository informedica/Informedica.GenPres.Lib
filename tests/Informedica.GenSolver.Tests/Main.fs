namespace Informedica.GenSolver.Tests

module Main =
    open Expecto

    [<EntryPoint>]
    let main argv =    
        testList "All" [
            ValueRange.tests
            Name.tests
        ]
        |> runTests { defaultConfig with verbosity = Logging.LogLevel.Verbose }
    
//        Tests.runTestsInAssembly defaultConfig argv
