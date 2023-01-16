

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
                |> tryHead (fun p -> p.Label)
                |> fun s ->
                    if s |> String.isNullOrWhiteSpace then "oplosvloeistof"
                    else s
            Shape =
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


    let setSolutionLimit (sls : SolutionLimit[]) (items : SubstanceItem list) =
        items
        |> List.map (fun item ->
            match sls |> Array.tryFind (fun sl -> sl.Substance |> String.equalsCapInsens item.Name) with
            | None -> item
            | Some sl ->
                { item with
                    Solution = Some sl
                }
        )


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

        let prods =
            pr.DoseRule.Products
            |> createProductComponent noSubst pr.DoseRule.FreqUnit pr.DoseRule.DoseLimits
            |> List.singleton

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
        |> fun dro ->
                match pr.SolutionRule with
                | None -> dro
                | Some sr ->
                    printfn "found solutionrule"
                    { dro with
//                        Quantities = sr.Volumes |> Array.toList
                        DoseCount = sr.DosePerc.Maximum
                        Products =
                            let ps =
                                dro.Products
                                |> List.map (fun p ->
                                    { p with
                                        Substances =
                                            p.Substances
                                            |> setSolutionLimit sr.SolutionLimits
                                    }
                                )

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
                                |> List.append ps
                            | None ->
                                printfn $"couldn't find {s} in parenterals"
                                ps
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



let evaluate logger (rule : PrescriptionRule) =
    let rec solve retry pr =
        pr
        |> Api.createDrugOrder
        |> DrugOrder.toOrder
        |> Order.Dto.fromDto
        |> Order.solveMinMax false logger
        |> function
        | Ok ord ->
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

            Ok (ord, pr)
        | Error _ when retry ->
            printfn "trying a second time with manual product"
            { pr with
                DoseRule =
                    { pr.DoseRule with
                        Products =
                            pr.DoseRule.Products
                            |> Array.choose Product.manual
                    }
            }
            |> solve false
        | Error (ord, m) -> Error (ord, pr, m)

    solve true rule



let test pat n =
    pat
    |> PrescriptionRule.get
    |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
    |> Array.item n
    |> evaluate { Log = ignore }
    |> function
    | Ok (ord, pr) ->
        let o =
            ord
            |> Order.toString
            |> String.concat "\n"
        let p =
            $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}"
        Ok (p, o)
    | Error (ord, pr, m) ->
        let o =
            ord
            |> Order.toString
            |> String.concat "\n"
        let p =
            $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}"

        Error ($"%A{m}", p, o)



type Age = Patient.Optics.Age
type Weight = Patient.Optics.Weight
type Height = Patient.Optics.Height


let pat =
    Patient.patient

    |> Patient.Optics.setAge [ 1 |> Age.Weeks]
    |> Patient.Optics.setWeight (3.5m |> Weight.Kilogram |> Some)
    |> Patient.Optics.setHeight (60 |> Height.Centimeter |> Some)
    |> Patient.Optics.setDepartment "ICK"


let n =
    pat
    |> PrescriptionRule.get
    |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
    |> Array.length



for i in [0..n-1] do
    try
        i
        |> test pat
        |> function
        | Ok (p, _) ->
            printfn $"{i}.Ok: {p}"
        | Error (_, p, _) -> printfn $"{i}.Fail: {p}"
    with
    | _ ->
        let pr =
            pat
            |> PrescriptionRule.get
            |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
            |> Array.item i
            |> fun pr ->
                $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}"

        printfn $"{i}. could not calculate: {pr}"


test pat 34


let pr i =
    pat
    |> PrescriptionRule.get
    |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
    |> Array.item i


startLogger ()


pr 34
|> Api.createDrugOrder
|> DrugOrder.toOrder
|> Order.Dto.fromDto
|> Order.applyConstraints |> Order.toString
|> Order.solveMinMax false { Log = ignore }
|> function
| Error (ord, msgs) ->
    printfn "oeps error"
    printfn $"{msgs |> List.map string}"
    ord
    |> Order.toString
    |> String.concat "\n"
    |> printfn "%s"

| Ok ord  ->
    ord
    |> Order.toString
    |> String.concat "\n"
    |> printfn "%s"



