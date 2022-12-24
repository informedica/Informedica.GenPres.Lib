

#load "load.fsx"


#time



open MathNet.Numerics
open Informedica.GenForm.Lib
open Informedica.GenUnits.Lib
open Informedica.GenSolver.Lib
open Informedica.GenOrder.Lib


open Api



let path = Some $"{__SOURCE_DIRECTORY__}/log.txt"
let startLogger () =
    // Start the logger at an informative level
    OrderLogger.logger.Start path Logging.Level.Informative


module Api =

    open System
    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL
    open Informedica.GenForm.Lib
    open Informedica.GenOrder.Lib


    let tryHead m = (Array.map m) >> Array.tryHead >> (Option.defaultValue "")


    let createProductComponent noSubst freqUnit (doseLimits : DoseLimit []) (ps : Product []) =
        { DrugOrder.productComponent with
            Name = ps |> tryHead (fun p -> p.Shape)
            Quantities =
                ps
                |> Array.collect (fun p -> p.ShapeQuantities)
                |> Array.distinct
                |> Array.toList
            TimeUnit = freqUnit
            RateUnit = "uur" //doseRule.RateUnit
            Divisible =
                ps
                |> Array.choose (fun p -> p.Divisible)
                |> Array.tryHead
                |> Option.defaultValue 1N
            Substances =
                if noSubst then []
                else
                    ps
                    |> Array.collect (fun p -> p.Substances)
                    |> Array.groupBy (fun s -> s.Name)
                    |> Array.map (fun (n, xs) ->
                        {
                            Name = n
                            Concentrations =
                                xs
                                |> Array.choose (fun s -> s.Quantity)
                                |> Array.distinct
                                |> Array.toList
                            OrderableQuantities = []
                            Unit = xs |> tryHead (fun x -> x.Unit)
                            TimeUnit = freqUnit
                            Dose =
                                doseLimits
                                |> Array.tryFind (fun l -> l.Substance = n)
                                |> Option.defaultValue DoseRule.DoseLimit.limit
                            Solution = None
                        }
                    )
                    |> Array.toList
        }


    let createDrugOrder (pr : PrescriptionRule) =
        let parenteral = Product.Parenteral.get ()
        let au =
            if pr.DoseRule.AdjustUnit |> String.isNullOrWhiteSpace then "kg"
            else pr.DoseRule.AdjustUnit

        let dose =
            pr.DoseRule.DoseLimits
            |> Array.filter (fun dl -> dl.Substance |> String.isNullOrWhiteSpace)
            |> function
            | [|dl|] -> dl |> Some
            | _ -> None

        let noSubst =
            dose
            |> Option.map (fun d -> d.DoseUnit = "keer")
            |> Option.defaultValue false

        { DrugOrder.drugOrder with
            Id = Guid.NewGuid().ToString()
            Name = pr.DoseRule.Generic
            Products =
                pr.DoseRule.Products
                |> createProductComponent noSubst pr.DoseRule.FreqUnit pr.DoseRule.DoseLimits
                |> List.singleton
            Quantities = []
            Frequencies = pr.DoseRule.Frequencies |> Array.toList
            FreqUnit = pr.DoseRule.FreqUnit
            Unit =
                pr.DoseRule.Products
                |> tryHead (fun p -> p.ShapeUnit)
            TimeUnit = pr.DoseRule.TimeUnit
            RateUnit = "uur"
            Route = pr.DoseRule.Route
            DoseCount =
                if pr.SolutionRule.IsNone then Some 1N
                else None
            OrderType =
                match pr.DoseRule.DoseType with
                | Informedica.GenForm.Lib.Types.Continuous -> ContinuousOrder
                | _ when pr.DoseRule.TimeUnit |> String.isNullOrWhiteSpace -> DiscontinuousOrder
                | _ -> TimedOrder
            Dose = dose
            Adjust =
                if au = "kg" then
                    pr.Patient.Weight
                    |> Option.map (fun v -> v / 1000N)
                else pr.Patient.BSA
            AdjustUnit = au
        }
        |> fun dr ->
                match pr.SolutionRule with
                | None -> dr
                | Some sr ->
                    { dr with
                        Quantities = sr.Volumes |> Array.toList
                        DoseCount = sr.DosePerc.Maximum
                        Products =
                            match sr.Solutions with
                            | [|s|] ->
                                parenteral
                                |> Array.tryFind (fun p -> p.Generic |> String.startsWith s)
                                |> function
                                | Some p ->
                                    [|p|]
                                    |> createProductComponent true pr.DoseRule.FreqUnit [||]
                                    |> List.singleton
                                    |> List.append dr.Products
                                | None -> dr.Products
                            | _ -> dr.Products
                    }

    let evaluate (dr : DrugOrder) =
        dr
        |> DrugOrder.toOrder
        |> Order.Dto.fromDto
        |> Order.solveMinMax { Log = ignore }


    // print an order list
    let toScenarios ind sn (sc : Order list) =
        sc
        |> List.mapi (fun i o ->
            o
            |> Order.Print.printPrescription sn
            |> fun (pres, prep, adm) ->
                {
                    No = i
                    Indication = ind
                    Name = o.Orderable.Name |> Informedica.GenSolver.Lib.Variable.Name.toString
                    Shape = o.Orderable.Components[0].Shape
                    Route = o.Route
                    Prescription = pres
                    Preparation = prep
                    Administration = adm
                }
        )



let test a w n =
    let a = a * 365m |> BigRational.FromDecimal |> Some
    let w = w * 1000m |> BigRational.FromDecimal |> Some
    Patient.patient
    |> Patient.Optics.setAge a
    |> Patient.Optics.setWeight w
    |> Patient.Optics.setBSA (1N |> Some)
    |> PrescriptionRule.get
    |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
    |> Array.item n
    |> fun pr ->
        printfn $"{[|pr|] |> PrescriptionRule.toMarkdown}"
        pr
    |> Api.createDrugOrder
    |> DrugOrder.toOrder
    |> Order.Dto.fromDto
    |> Order.solveMinMax { Log = ignore }
    |> Order.toString
    |> String.concat "\n"
    |> printfn "# Berekening:\n\n%s"



test 3m 15m 17


Patient.patient
|> Patient.Optics.setAge (10N * 365N |> Some)
|> Patient.Optics.setWeight (30N * 1000N |> Some)
|> Patient.Optics.setBSA (1N |> Some)
|> PrescriptionRule.get
|> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
|> Array.item 17




DoseRule.get ()
|> Array.filter (fun dr -> dr.Products |> Array.isEmpty)
|> Array.map (fun dr -> dr.Generic, dr.Shape, dr.Route)
|> Array.map (fun (g, s, r) ->
    let ps =
        Product.get ()
        |> Array.filter (fun p -> p.Generic = g)
        |> Array.map (fun p -> p.Shape)
        |> Array.distinct
        |> String.concat ";"
    $"{g}, {s}, {r}: {ps}"
)
|> Array.distinct
|> Array.map (fun x -> printfn "%s" x; x)
|> Array.length



open Informedica.ZIndex.Lib


GenPresProduct.get true
|> Array.filter (fun gpp ->
    gpp.GenericProducts
    |> Array.exists (fun gp -> gp.Id = 165638)
)
|> Array.map (fun gpp -> gpp.Name.ToLower())














