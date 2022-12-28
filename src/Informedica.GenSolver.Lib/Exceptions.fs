namespace Informedica.GenSolver.Lib


module Exceptions =

        /// Equation exception
        exception SolverException of Exceptions.Message


        /// Raise an `EquationException` with `Message` `m`.
        let raiseExc log m =
//            printfn $"Informedica.GenSolver.Lib error:\n{m}"

            match log with
            | Some log ->
//                printfn $"logging error {m}"
                m |> Logging.logError log
            | None -> ()

            m |> SolverException |> raise


