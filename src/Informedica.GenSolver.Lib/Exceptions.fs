namespace Informedica.GenSolver.Lib


module Exceptions =

        /// Equation exception
        exception SolverException of Exceptions.Message list


        /// Raise an `EquationException` with `Message` `m`.
        let raiseExc log errs m =
            match log with
            | Some log ->
                // printfn $"adding error {m} to {errs |> List.length}"
                m |> Logging.logError log
            | None -> ()

            m::errs |> SolverException |> raise


