

#load "load.fsx"


#time



open MathNet.Numerics
open Informedica.Utils.Lib.BCL
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
            Name =
                ps
                |> tryHead (fun p -> p.Shape)
                |> fun s ->
                    if s |> String.isNullOrWhiteSpace then "oplosvloeistof"
                    else s
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
            Time = pr.DoseRule.Time
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
                else pr.Patient |> Patient.calcBSA
            AdjustUnit = au
        }
        |> fun dr ->
                match pr.SolutionRule with
                | None -> dr
                | Some sr ->
//                    printfn "found solutionrule"
                    { dr with
                        Quantities = sr.Volumes |> Array.toList
                        DoseCount = sr.DosePerc.Maximum
                        Products =
                            let s =
                                // ugly hack to get default solution
                                sr.Solutions
                                |> Array.tryHead
                                |> Option.defaultValue "x"
                            parenteral
                            |> Array.tryFind (fun p -> p.Generic |> String.startsWith s)
                            |> function
                            | Some p ->
//                                printfn $"adding solution {p.Generic}"
                                [|p|]
                                |> createProductComponent true pr.DoseRule.FreqUnit [||]
                                |> List.singleton
                                |> List.append dr.Products
                            | None ->
                                printfn $"couldn't find {s} in parenterals"
                                dr.Products
                    }

    let evaluate (dr : DrugOrder) =
        dr
        |> DrugOrder.toOrder
        |> Order.Dto.fromDto
        |> Order.solveMinMax true { Log = ignore }


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



let evaluate (pr : PrescriptionRule) =
    try
        let ord =
            pr
            |> Api.createDrugOrder
            |> DrugOrder.toOrder
            |> Order.Dto.fromDto
            |> Order.solveMinMax false { Log = ignore }

        let dto = ord |> Order.Dto.toDto

        let shps =
            dto.Orderable.Components
            |> List.collect (fun cDto -> cDto.ComponentQuantity.Variable.Vals)
            |> List.toArray

        let sbsts =
            dto.Orderable.Components
            |> List.toArray
            |> Array.collect (fun cDto ->
                cDto.Items
                |> List.toArray
                |> Array.collect (fun iDto ->
                    iDto.ComponentConcentration.Variable.Vals
                    |> List.toArray
                    |> Array.map (fun v -> iDto.Name, v |> Some)
                )
            )
            |> Array.distinct

        let pr =
            pr
            |> PrescriptionRule.filterProducts
                shps
                sbsts

        ord, pr
    with
    | _ ->
//        printfn "bereken met handmatige bereiding"
        let pr =
            { pr with
                DoseRule =
                    { pr.DoseRule with
                        Products =
                            pr.DoseRule.Products
                            |> Array.choose Product.manual
                    }
            }

        let ord =
            pr
            |> Api.createDrugOrder
            |> DrugOrder.toOrder
            |> Order.Dto.fromDto
            |> Order.solveMinMax false { Log = ignore }

        let dto = ord |> Order.Dto.toDto

        let shps =
            dto.Orderable.Components
            |> List.collect (fun cDto -> cDto.ComponentQuantity.Variable.Vals)
            |> List.toArray

        let sbsts =
            dto.Orderable.Components
            |> List.toArray
            |> Array.collect (fun cDto ->
                cDto.Items
                |> List.toArray
                |> Array.collect (fun iDto ->
                    iDto.ComponentConcentration.Variable.Vals
                    |> List.toArray
                    |> Array.map (fun v -> iDto.Name, v |> Some)
                )
            )
            |> Array.distinct

        let pr =
            pr
            |> PrescriptionRule.filterProducts
                shps
                sbsts

        ord, pr



let test pat n =
    let ord, pr =
        pat
        |> PrescriptionRule.get
        |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
        |> Array.item n
        |> evaluate

    $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}",
    ord
    |> Order.toString
    |> String.concat "\n"


type Age = Patient.Optics.Age
type Weight = Patient.Optics.Weight
type Height = Patient.Optics.Height


let pat =
    Patient.patient
    
    |> Patient.Optics.setAge [ 15 |> Age.Years] 
    |> Patient.Optics.setWeight (70m |> Weight.Kilogram |> Some)
    |> Patient.Optics.setHeight (170 |> Height.Centimeter |> Some)
    |> Patient.Optics.setDepartment "ICK"


let n =
    pat
    |> PrescriptionRule.get
    |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
    |> Array.length
    

for i in [0..n - 1] do
    try
        i
        |> test pat
        |> ignore
    with
    | _ ->
        let pr =
            pat
            |> PrescriptionRule.get
            |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
            |> Array.item i
            |> fun pr ->
                $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}"

        printfn $"could not calculate: {i}. {pr}"


