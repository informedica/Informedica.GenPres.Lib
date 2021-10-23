namespace Informedica.GenOrder.Lib

open System

module Logging =

    open Types
    open Informedica.GenSolver.Lib.Types.Logging


    module SolverLogging = Informedica.GenSolver.Lib.Logging
    module LoggingType = Informedica.GenSolver.Lib.Types.Logging
    

    let private log level (logger : Logger) msg  =
        msg
        |> Logging.OrderMessage
        |> fun m ->
            {
                TimeStamp = DateTime.Now
                Level = level
                Message = m
            }
            |> logger.Log 


    let logInfo = log Informative 


    let logWarning = log Warning 


    let logError (logger: Logger) exc =
        exc
        |> Logging.OrderException
        |> fun m ->
            {
                TimeStamp = DateTime.Now
                Level = Error
                Message = m
            }
            |> logger.Log

            