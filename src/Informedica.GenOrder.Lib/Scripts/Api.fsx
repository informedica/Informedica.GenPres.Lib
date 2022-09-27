

#load "load.fsx"

open MathNet.Numerics
open Informedica.GenSolver.Lib
open Informedica.GenOrder.Lib



let path = Some $"{__SOURCE_DIRECTORY__}/log.txt"


Api.filter None None None (Some "paracetamol") None None
|> List.item 0
|> Api.evaluate None (6N)
|> List.map Api.translate
|> List.iter (printfn "%A")


Api.filter None None None (Some "noradrenaline") (Some "infusievloeistof") None
|> List.item 0
|> Api.evaluate None (6N)
|> List.map Api.translate
|> List.iter (printfn "%A")


// Start the logger at an informative level
OrderLogger.logger.Start path Logging.Level.Informative

// report output to the fsi
OrderLogger.logger.Report ()



Api.filter None None None (Some "gentamicine") None None
|> List.item 0
|> fun dr ->
    let solRule =
        Data.getSolutions ()
        |> List.tryFind (fun s -> s.Medication = dr.Medication)
    dr
    |> Api.createDrugOrders solRule
    |> List.map (DrugOrder.toConstrainedOrder true)
    |> List.map (DrugOrder.setDoseRule dr)
    |> fun xs ->
        if solRule.IsNone then xs
        else
            xs
            |> List.map (DrugOrder.setSolutionRule true solRule.Value)
    |> List.map (DrugOrder.setAdjust dr.Medication 10N)


Api.filter None None None (Some "gentamicine") None None
|> List.item 0
|> fun dr ->
    let solRule =
        Data.getSolutions ()
        |> List.tryFind (fun s -> s.Medication = dr.Medication)
    dr
    |> Api.createDrugOrders solRule
    |> List.map (DrugOrder.toConstrainedOrder true)
    |> List.map (DrugOrder.setDoseRule dr)
    |> fun xs ->
        if solRule.IsNone then xs
        else
            xs
            |> List.map (DrugOrder.setSolutionRule true solRule.Value)
    |> List.map (DrugOrder.setAdjust dr.Medication 10N)
    |> List.head
    |> fun (cs, o) ->
        let l = OrderLogger.logger.Logger
        // return eqs
        let cs = cs |> List.map (DrugOrder.DrugConstraint.mapToConstraint o)

        let toEqString op vs =
            vs
            |> List.sortBy (fun vs -> vs |> List.head)
            |> List.map (fun vs ->
                match vs with
                | h::tail ->
                    let s =
                        tail
                        |> List.map (VariableUnit.toString false)
                        |> String.concat op
                    $"{h |> VariableUnit.toString false} = {s}"
                | _ -> ""
            )
            |> String.concat "\n"

        let (Id s) = o.Id
        let s = s + "."

        o
        |> Order.solveUnits l
        |> Order.solveConstraints l cs
        |> Order.toEqs
        |> fun (vs1, vs2) ->
            $"""
{(vs1 |> toEqString " * ").Replace(s, "")}
{(vs2 |> toEqString " + ").Replace(s, "")}
"""
