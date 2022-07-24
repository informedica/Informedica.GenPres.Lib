namespace Informedica.GenOrder.Lib

module OrderLogger =

    open System
    open System.Diagnostics

    open Informedica.GenUnits.Lib
    open Informedica.GenOrder.Lib

    open Types

    module Units = ValueUnit.Units
    module DrugConstraint = DrugOrder.DrugConstraint
    module Quantity = VariableUnit.Quantity

    module SolverLogging = Informedica.GenSolver.Lib.SolverLogging

    type Logger = Informedica.GenSolver.Lib.Types.Logging.Logger
    type SolverMessage = Informedica.GenSolver.Lib.Types.Logging.SolverMessage
    type OrderMessage = Informedica.GenOrder.Lib.Types.Logging.Message


    type Agent<'Msg> = MailboxProcessor<'Msg>
    type IMessage = Informedica.GenSolver.Lib.Types.Logging.IMessage
    type Level = Informedica.GenSolver.Lib.Types.Logging.Level


    // To print all messages related to an order
    let printOrderMsg msg = 
        match msg with 
        | Logging.OrderMessage m ->
            match m with
            | Events.SolverReplaceUnit (n, u) -> 
                $"replaced {n} with {u |> ValueUnit.unitToString}"
            | Events.OrderSolved o -> ""
            | Events.OrderConstraintsSolved (o, cs) -> 
                o
                |> Order.toString
                |> String.concat "\n"
                |> sprintf "=== Order constraints solved ===\n%s"

            | Events.OrderScenario s -> ""
            | Events.OrderScenerioWithNameValue (o, n, br) -> ""
        | Logging.OrderException s -> s

    // Catches a message and will dispatch this to the appropiate
    // print function
    let printMsg (msg : IMessage) = 
        match msg with
        | :? SolverMessage as m -> 
            m 
            |> SolverLogging.printMsg
        | :? OrderMessage  as m -> m |> printOrderMsg
        | _ -> 
            sprintf ""

    // A message to send to the order logger agent
    type Message =  
        | Start of Level
        | Received of Informedica.GenSolver.Lib.Types.Logging.Message
        | Report
        | Write of string

    // The type for an order logger agent that will
    // catch a message and will proces this in an asynchronous way.
    type OrderLogger =
        {
            Start : Level -> unit
            Logger: Logger
            Report: unit -> unit
            Write : string -> unit
        }


    let noLogger : Logger = { Log = ignore }

    // Create the logger agent 
    let logger =

        let loggerAgent : Agent<Message> = 
            Agent.Start <| fun inbox ->
                let msgs = ResizeArray<(float * Informedica.GenSolver.Lib.Types.Logging.Message)>()

                let rec loop (timer : Stopwatch) level msgs =
                    async {
                        let! msg = inbox.Receive ()

                        match msg with
                        | Start level -> 
                            let timer = Stopwatch.StartNew()
                            return! 
                                ResizeArray<(float * Informedica.GenSolver.Lib.Types.Logging.Message)>()
                                |> loop timer level

                        | Received m -> 
                            if m.Level = level then
                                msgs.Add(timer.Elapsed.TotalSeconds, m)
                            return! loop timer level msgs

                        | Report ->
                            printfn "=== Start Report ===\n"
                            msgs 
                            |> Seq.length
                            |> printfn "Total messages received: %i\n"
                            
                            msgs 
                            |> Seq.iteri (fun i (t, m) ->
                                m.Message
                                |> printMsg
                                |> function 
                                | s when s |> String.IsNullOrEmpty -> ()
                                | s -> printfn "\n%i. %f: %A\n%s" i t m.Level s
                            )
                            printfn "\n"

                            return! loop timer level msgs

                        | Write path ->
                            msgs 
                            |> Seq.iteri (fun i (t, m) ->
                                m.Message
                                |> printMsg
                                |> function 
                                | s when s |> String.IsNullOrEmpty -> ()
                                | s -> 
                                    let s = sprintf "\n%i. %f: %A\n%s" i t m.Level s
                                    System.IO.File.AppendAllLines(path, [s])
                            )
                            System.IO.File.AppendAllLines(path, ["\n"])

                            return! loop timer level msgs
                    }
                
                let timer = Stopwatch.StartNew()
                loop timer Level.Error msgs
            
        {
            Start = 
                fun level ->
                    level
                    |> Start
                    |> loggerAgent.Post
            Logger = {
                Log =
                    fun msg ->
                        msg
                        |> Received
                        |> loggerAgent.Post 
            } 
            Report = 
                fun _ ->
                    Report
                    |> loggerAgent.Post
            Write =
                fun path ->
                    Write path
                    |> loggerAgent.Post
        }

    // print an order list
    let printScenarios v n (sc : Order list) =
        let w =
            match sc with 
            | h::_ -> 
                h.Adjust
                |> Quantity.toValueUnitStringList None
                |> Seq.map snd
                |> Seq.tryHead
                |> Option.defaultValue ""
            | _ -> ""

        printfn "\n\n=== SCENARIOS for Weight: %s ===" w
        sc
        |> List.iteri (fun i o ->
            o
            |> Order.printPrescription n
            |> fun (p, a, d) ->
                printfn "%i\tprescription:\t%s" (i + 1) p
                printfn "  \tdispensing:\t%s" a
                printfn "  \tpreparation:\t%s" d
            
            if v then
                o
                |> Order.toString
                |> List.iteri (fun i s -> printfn "%i\t%s" (i + 1) s)

                printfn "\n"
        )
