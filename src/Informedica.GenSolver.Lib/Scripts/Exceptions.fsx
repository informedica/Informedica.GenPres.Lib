
#load "load.fsx"


open Informedica.GenSolver.Lib


fsi.AddPrinter<exn> (fun e ->
    printf "printing exception"
    match e with
    | :? Exceptions.SolverException as se ->
        sprintf $"{se.Data0}"
    | _ -> e.ToString()
)



Variable.Name.createExc ""

