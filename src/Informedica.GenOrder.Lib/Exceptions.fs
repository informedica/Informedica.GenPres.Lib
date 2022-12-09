namespace Informedica.GenOrder.Lib




module Exceptions =

        /// Equation exception
        exception OrderException of Exceptions.Message

        /// Raise an `EquationException` with `Message` `m`.
        let raiseExc log m o =
            match log with
            | Some log ->
                printfn $"logging error {m}"
                (m, o)
                |> Exceptions.OrderCouldNotBeSolved
                |> Logging.logError log

            | None -> ()

            (m, o) |> Exceptions.OrderCouldNotBeSolved |> OrderException |> raise

