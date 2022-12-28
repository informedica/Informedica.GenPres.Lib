

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



let test a w h d n =
    let a = a * 365m |> BigRational.FromDecimal |> Some
    let w = w * 1000m |> BigRational.FromDecimal |> Some
    let h = h |> BigRational.FromDecimal |> Some

    let pat =
        Patient.patient
        |> Patient.Optics.setAge a
        |> Patient.Optics.setWeight w
        |> Patient.Optics.setHeight h
        |> Patient.Optics.setDepartment d

    let pr =
        pat
        |> PrescriptionRule.get
        |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
        |> Array.item n

    //[| pr |]
    //|> PrescriptionRule.toMarkdown
    //|> printfn "Found rule:\n%s"
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

//        printfn $"{[|pr|] |> PrescriptionRule.toMarkdown}"

        $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}",
        ord
        |> Order.toString
        |> String.concat "\n"
//        |> printfn "# Berekening:\n\n%s"
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

//        printfn $"{[|pr|] |> PrescriptionRule.toMarkdown}"

        $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}",
        ord
        |> Order.toString
        |> String.concat "\n"
//        |> printfn "# Berekening:\n\n%s"


let n =
    let a = 15m * 365m |> BigRational.FromDecimal |> Some
    let w = 70m * 1000m |> BigRational.FromDecimal |> Some
    let h = 170m |> BigRational.FromDecimal |> Some

    let pat =
        Patient.patient
        |> Patient.Optics.setAge a
        |> Patient.Optics.setWeight w
        |> Patient.Optics.setHeight h
        |> Patient.Optics.setDepartment "ICK"

    pat
    |> PrescriptionRule.get
    |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
    |> Array.length
    

for i in [0..500] do
    try
        let (p, s) =
            i
            |> test 15m 70m 170m "ICK"
        printfn $"calculated: {i}. {p}"
    with
    | _ ->
        let a = 15m * 365m |> BigRational.FromDecimal |> Some
        let w = 70m * 1000m |> BigRational.FromDecimal |> Some
        let h = 170m |> BigRational.FromDecimal |> Some

        let pat =
            Patient.patient
            |> Patient.Optics.setAge a
            |> Patient.Optics.setWeight w
            |> Patient.Optics.setHeight h
            |> Patient.Optics.setDepartment "ICK"

        let pr =
            pat
            |> PrescriptionRule.get
            |> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
            |> Array.item i
            |> fun pr ->
                $"{pr.DoseRule.Generic}, {pr.DoseRule.Shape}, {pr.DoseRule.Indication}"

        printfn $"could not calculate: {i}. {pr}"


Patient.patient
|> Patient.Optics.setAge (15N * 365N |> Some)
|> Patient.Optics.setWeight (70N * 1000N |> Some)
|> Patient.Optics.setHeight (170N |> Some)
|> Patient.Optics.setDepartment "ICK"
|> PrescriptionRule.get
|> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
|> Array.item 20
|> Api.createDrugOrder
|> DrugOrder.toOrder
|> Order.Dto.fromDto
|> Order.applyConstraints
|> Order.toString


Patient.patient
|> Patient.Optics.setAge (3N * 365N |> Some)
|> Patient.Optics.setWeight (15N * 1000N |> Some)
|> Patient.Optics.setHeight (170N |> Some)
|> Patient.Optics.setDepartment "ICK"
|> PrescriptionRule.get
|> Array.filter (fun pr -> pr.DoseRule.Products |> Array.isEmpty |> not)
|> Array.filter (fun pr -> pr.SolutionRule.IsSome)
|> Array.item 2


Patient.patient
|> Patient.Optics.setAge (3N * 365N |> Some)
|> Patient.Optics.setWeight (15N * 1000N |> Some)
|> Patient.Optics.setHeight (170N |> Some)
|> Patient.Optics.setDepartment "ICK"
|> PrescriptionRule.get
|> Array.filter (fun pr ->
    pr.DoseRule.Patient |> PatientCategory.toString = "neonaten" //&& pr.DoseRule.Indication = "diurese"
)
|> PrescriptionRule.indications
|> Array.sort
|> Array.iteri (printfn "%i. %s")


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


open Informedica.Utils.Lib.BCL

let manual (p : Product) =
    if p.Substances |> Array.isEmpty then None
    else
        match p.Substances[0].Quantity with
        | Some sq ->
            { p with
                GPK = $"{90000000 + (p.GPK |> Int32.parse)}"
                Product = p.Product  + " EIGEN BEREIDING" |> String.trim
                Label = p.Label + " EIGEN BEREIDING" |> String.trim
                ShapeQuantities = [| 1N |]
                Substances =
                    p.Substances
                    |> Array.map (fun s ->
                        { s with
                            Quantity = s.Quantity |> Option.map (fun v -> v / sq)
                        }
                    )
            }
            |> Some
        | None -> None


Product.get ()
|> Array.take 100
|> Array.map manual


open Informedica.ZIndex.Lib


GenPresProduct.get true
|> Array.filter (fun gpp ->
    gpp.GenericProducts
    |> Array.exists (fun gp -> gp.Id = 165638)
)
|> Array.map (fun gpp -> gpp.Name.ToLower())




GenPresProduct.get true
|> Array.filter (fun gpp ->
    gpp.Name |> String.startsWith "AMFO"
)
|> Array.map (fun gpp -> gpp.Name, gpp.Shape)

 


