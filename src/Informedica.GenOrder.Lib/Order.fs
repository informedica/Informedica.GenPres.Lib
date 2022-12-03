namespace Informedica.GenOrder.Lib

/// Types and functions that deal with an order.
/// An `Order` models the `Prescription` of an
/// `Orderable` with a `StartStop` start date and
/// stop date.
module Order =

    open Informedica.Utils.Lib.BCL

    open Informedica.GenSolver.Lib.Types
    open Types

    module Logging = Informedica.GenOrder.Lib.Logging


    /// Utitlity functions to
    /// enable mapping of a `Variable`s
    /// to an `Order`
    module Mapping =

        let qty = VariableUnit.Quantity.name
        let cnc = VariableUnit.Concentration.name
        let tot = VariableUnit.Total.name
        let rte = VariableUnit.Rate.name
        let qtyAdj = VariableUnit.QuantityAdjust.name
        let totAdj = VariableUnit.TotalAdjust.name
        let rteAdj = VariableUnit.RateAdjust.name
        let cnt = VariableUnit.Count.name
        let frq = VariableUnit.Frequency.name
        let tme = VariableUnit.Time.name
        let itm = Orderable.Literals.item
        let cmp = Orderable.Literals.comp
        let orb = Orderable.Literals.orderable
        let dos = Orderable.Literals.dose
        let prs = "Prescription"
        let ord = "Order"
        let adj = "Adjust"

        let map = function
            | ItemComponentQty -> $"{itm}.{cmp}.{qty}"
            | ItemComponentConc -> $"{itm}.{cmp}.{cnc}"
            | ItemOrderableQty -> $"{itm}.{orb}.{qty}"
            | ItemOrderableConc -> $"{itm}.{orb}.{cnc}"
            | ItemDoseQty -> $"{itm}.{dos}.{qty}"
            | ItemDoseTotal -> $"{itm}.{dos}.{tot}"
            | ItemDoseRate -> $"{itm}.{dos}.{rte}"
            | ItemDoseAdjustQty -> $"{itm}.{dos}.{qtyAdj}"
            | ItemDoseAdjustTotal -> $"{itm}.{dos}.{totAdj}"
            | ItemDoseAdjustRate -> $"{itm}.{dos}.{rteAdj}"
            | ComponentQty -> $"{cmp}.{cmp}.{qty}"
            | ComponentOrderableQty -> $"{cmp}.{orb}.{qty}"
            | ComponentOrderableConc -> $"{cmp}.{orb}.{cnc}"
            | ComponentOrderableCount -> $"{cmp}.{orb}.{cnt}"
            | ComponentOrderCount -> $"{cmp}.{ord}.{cnt}"
            | ComponentDoseQty -> $"{cmp}.{dos}.{qty}"
            | ComponentDoseTotal -> $"{cmp}.{dos}.{tot}"
            | ComponentDoseRate -> $"{cmp}.{dos}.{rte}"
            | ComponentDoseAdjustQty -> $"{cmp}.{dos}.{qtyAdj}"
            | ComponentDoseAdjustTotal -> $"{cmp}.{dos}.{totAdj}"
            | ComponentDoseAdjustRate -> $"{cmp}.{dos}.{rteAdj}"
            | OrderableQty -> $"{orb}.{orb}.{qty}"
            | OrderableDoseCount -> $"{orb}.{dos}.{cnt}"
            | OrderableDoseQty -> $"{orb}.{dos}.{qty}"
            | OrderableDoseTotal -> $"{orb}.{dos}.{tot}"
            | OrderableDoseRate -> $"{orb}.{dos}.{rte}"
            | OrderableDoseAdjustQty -> $"{orb}.{dos}.{qtyAdj}"
            | OrderableDoseAdjustTotal -> $"{orb}.{dos}.{totAdj}"
            | OrderableDoseAdjustRate -> $"{orb}.{dos}.{rteAdj}"
            | OrderableOrderQty -> $"{orb}.{ord}.{qty}"
            | OrderableOrderCount -> $"{orb}.{ord}.{cnt}"
            | OrderPresFreq -> $"{ord}.{prs}.{frq}"
            | OrderPresTime -> $"{ord}.{prs}.{tme}"
            | OrderAdjustQty -> $"{ord}.{adj}.{qty}"


        let fromString s =
            match s with
            | _ when s = (ItemComponentQty |> map) -> ItemComponentQty
            | _ when s = (ItemComponentConc |> map) -> ItemComponentConc
            | _ when s = (ItemOrderableQty |> map) -> ItemOrderableQty
            | _ when s = (ItemOrderableConc |> map) -> ItemOrderableConc
            | _ when s = (ItemDoseQty |> map) -> ItemDoseQty
            | _ when s = (ItemDoseTotal |> map) -> ItemDoseTotal
            | _ when s = (ItemDoseRate |> map) -> ItemDoseRate
            | _ when s = (ItemDoseAdjustQty |> map) -> ItemDoseAdjustQty
            | _ when s = (ItemDoseAdjustTotal |> map) -> ItemDoseAdjustTotal
            | _ when s = (ItemDoseAdjustRate |> map) -> ItemDoseAdjustRate
            | _ when s = (ComponentQty |> map) -> ComponentQty
            | _ when s = (ComponentOrderableQty |> map) -> ComponentOrderableQty
            | _ when s = (ComponentOrderableConc |> map) -> ComponentOrderableConc
            | _ when s = (ComponentOrderableCount |> map) -> ComponentOrderableCount
            | _ when s = (ComponentDoseQty |> map) ->  ComponentDoseQty
            | _ when s = (ComponentDoseTotal |> map) ->  ComponentDoseTotal
            | _ when s = (ComponentDoseRate |> map) -> ComponentDoseRate
            | _ when s = (ComponentDoseAdjustQty |> map) -> ComponentDoseAdjustQty
            | _ when s = (ComponentDoseAdjustTotal |> map) -> ComponentDoseAdjustTotal
            | _ when s = (ComponentDoseAdjustRate |> map) -> ComponentDoseAdjustRate
            | _ when s = (ComponentOrderCount  |> map) ->  ComponentOrderCount
            | _ when s = (OrderableQty |> map) -> OrderableQty
            | _ when s = (OrderableDoseCount |> map) -> OrderableDoseCount
            | _ when s = (OrderableDoseQty  |> map) -> OrderableDoseQty
            | _ when s = (OrderableDoseTotal |> map) -> OrderableDoseTotal
            | _ when s = (OrderableDoseRate |> map) -> OrderableDoseRate
            | _ when s = (OrderableDoseAdjustQty |> map) -> OrderableDoseAdjustQty
            | _ when s = (OrderableDoseAdjustTotal |> map) -> OrderableDoseAdjustTotal
            | _ when s = (OrderableDoseAdjustRate |> map) -> OrderableDoseAdjustRate
            | _ when s = (OrderableOrderQty |> map) -> OrderableOrderQty
            | _ when s = (OrderableOrderCount |> map) -> OrderableOrderCount
            | _ when s = (OrderPresFreq |> map) -> OrderPresFreq
            | _ when s = (OrderPresTime |> map) -> OrderPresTime
            | _ when s = (OrderAdjustQty |> map) -> OrderAdjustQty
            | _ -> $"cannot map {s} to an OrderMapping" |> failwith



    /// Types and functions that
    /// model a start and stop date time
    /// of an `Order`
    module StartStop =

        let toString stst =
            match stst with
            | Start dt ->
                dt
                |> DateTime.formattedString "dd-MM-yy"
                |> sprintf "%s"
            | StartStop (start, stop) ->
                stop
                |> DateTime.formattedString "dd-MM-yy"
                |> sprintf "%s - %s" (start |> DateTime.formattedString "dd-MM-yy")

    open System

    open Informedica.GenUnits.Lib
    open WrappedString

    module ValueRange = Informedica.GenSolver.Lib.Variable.ValueRange
    module Equation = Informedica.GenSolver.Lib.Equation
    module Property = ValueRange.Property
    module Quantity = VariableUnit.Quantity
    module Frequency = VariableUnit.Frequency
    module Concentration = VariableUnit.Concentration
    module Rate = VariableUnit.Rate
    module Dose = VariableUnit.Dose
    module DoseAdjust = VariableUnit.DoseAdjust
    module Time = VariableUnit.Time
    module Units = ValueUnit.Units


    /// Apply `f` to `Order` `ord`
    let apply f (ord: Order) = ord |> f

    /// Utilty function to facilitate type inference
    let get = apply id

    /// Get the order id
    let getId ord = (ord |> get).Id

    /// Create an `Order` with
    ///
    /// * id: the id of the order
    /// * adj: by which doses are adjusted
    /// * orb: the `Orderable`
    /// * prs: `Prescription`, how the orderable is prescribed
    /// * rte: the route of administration of the orderable
    let create id adj_qty orb prs rte sts =
        {
            Id = id
            Adjust = adj_qty
            Orderable = orb
            Prescription = prs
            Route = rte
            StartStop = sts
        }

    /// Create a new orderable using:
    ///
    /// * id: the unique id in an `OrderSet` of an `Order`
    /// * nm: the name of the `Orderable`
    /// * cil: the names for the `Component`s, each with a list of name, string tuples for `Item`s
    /// * orb_un: the unit name for the `Orderable` and its `Component`s
    /// * cmp_un: the unit for the `Component`
    /// * adj_un: the unit used to adjust doses
    /// * tme_un: the unit for time
    /// * str_prs: a function that takes in a list of strings that will generate the names and returns a `Prescription`
    /// * route: the route of administration
    let createNew id n shape str_prs route =
        let orb = Orderable.createNew id n shape
        let nm  = orb |> Orderable.getName
        let adj = Quantity.quantity [ id |> Id.toString; n |> Name.toString; Mapping.ord; Mapping.adj ] ValueUnit.NoUnit
        let prs = [id |> Id.toString; nm |> Name.toString; Mapping.ord; Mapping.prs] |> str_prs
        let sts = DateTime.Now  |> StartStop.Start

        create id adj orb prs route sts

    let getAdjust ord = (ord |> get).Adjust

    let getName ord =
        ord
        |> (getAdjust >> Quantity.toVarUnt >> VariableUnit.getName)
        |> Name.toString
        |> String.split "."
        |> List.take 2
        |> String.concat "."


    let mapName n m (ord : Order) =
        let dls = "."

        [ (ord.Id |> Id.toString) + dls + n + dls + (m |> Mapping.map)]
        |> Name.create


    let getOrderable ord = (ord |> get).Orderable

    /// Map an `Order` *ord* to
    /// a list of `VariableUnit` lists
    let toEqs (ord: Order) =
        let orb = ord.Orderable
        let adj = ord.Adjust |> Quantity.toVarUnt
        let frq, tme = ord.Prescription |> Prescription.toEqs
        let hasRate =
            ord.Prescription |> Prescription.isContinuous ||
            ord.Prescription |> Prescription.isTimed

        orb |> Orderable.toEqs hasRate adj frq tme

    /// Map a list of `VariableUnit` lists
    /// to an `Order` *ord*
    let fromEqs ord eqs =
        let orb = ord.Orderable |> Orderable.fromEqs eqs
        let adj = ord.Adjust    |> Quantity.fromVar eqs
        let prs = ord.Prescription |> Prescription.fromEqs eqs

        {
            ord with
                Adjust = adj
                Orderable = orb
                Prescription = prs
        }


    /// Turn an order into a list of string
    /// representing variable name, valuerange
    /// and unit group
    let toString (ord: Order) =
        [ OrderAdjustQty |> Mapping.map; ord.Adjust |> Quantity.toString ]
        |> List.append (Orderable.Literals.orderable::(ord.Orderable |> Orderable.toString))
        |> List.append ("Prescription"::(ord.Prescription |> Prescription.toString))
        |> List.append ("Route"::[ord.Route])
        |> List.filter (String.isNullOrWhiteSpace >> not)


    let getVariableUnit n m o =
        let dls = "."

        let n =
            match m with
            | OrderPresFreq
            | OrderAdjustQty ->
                [ (o |> getName) + dls + (m |> Mapping.map) ] |> Name.create
            | _ ->
                [ (o.Id |> Id.toString) + dls + n + dls + (m |> Mapping.map) ]
                |> Name.create

        let prod, sum = o |> toEqs

        sum @ prod
        |> List.collect id
        |> List.tryFind (VariableUnit.getName >> ((=) n))


    let setVariableUnit o vru =

        let prod, sum = o |> toEqs

        sum @ prod
        |> List.collect id
        |> List.map (fun x ->
            if vru |> VariableUnit.getName = x.Variable.Name  then vru else x
        )
        |> List.singleton
        |> fromEqs o


    let hasUnitValue n m v o =
        getVariableUnit n m o
        |> function
        | Some vru ->
            vru
            |> VariableUnit.containsUnitValue v
        | None -> false

    let contains n v o =
        o
        |> toEqs
        |> fun (prod, sum) ->
            let find =
                List.tryFind (fun vru ->
                    vru
                    |> VariableUnit.getName
                    |> ((=) n)
                )

            match prod @ sum |> List.collect id |> find with
            | Some vru -> vru |> VariableUnit.containsUnitValue v
            | None -> false


    //let isEmpty n m o =
    //    getVariableUnit n m o
    //    |> function
    //    | Some vru ->
    //        vru.Variable.Values
    //        |> ValueRange.isEmpty
    //    | None -> false


    let solveUnits log o =
        // return eqs
        let toEql prod sum =

            prod
            |> List.map Solver.productEq
            |> List.append (sum |> List.map Solver.sumEq)

        let prod, sum = o |> toEqs

        let eqs = toEql prod sum

        eqs
        |> Solver.solveUnits log
        |> List.map (fun e ->
            match e with
            | OrderProductEquation (vru, vrus)
            | OrderSumEquation     (vru, vrus) -> vru::vrus
        )
        |> fromEqs o


    /// Solve an `Order` *ord* with
    ///
    /// * n: the name of the variable to be set
    /// * m: the mapping for the field of the order
    /// * p: the property of the variable to be set
    /// * vs: the values to be set
    let solve log n m p o =
        let n =
            mapName n m o

        // return eqs
        let toEql prod sum =

            prod
            |> List.map Solver.productEq
            |> List.append (sum |> List.map Solver.sumEq)


        let prod, sum = o |> toEqs

        let eqs = toEql prod sum

        eqs
        |> Solver.solve log n p
        |> fromEqs o
        |> fun o ->
            o
            |> Events.OrderSolveFinished
            |> Logging.logInfo log

            o


    let applyConstraints log cs o =
        // return eqs
        let toEql prod sum =

            prod
            |> List.map Solver.productEq
            |> List.append (sum |> List.map Solver.sumEq)

        let prod, sum = o |> toEqs

        let eqs = toEql prod sum

        eqs
        |> Solver.applyConstraints log cs
        |> fromEqs o


    let solveConstraints log cs order =
        let _log msg o =
            (o, cs)
            |> msg
            |> Logging.logInfo log

            o

        // return eqs
        let toEql prod sum =

            prod
            |> List.map Solver.productEq
            |> List.append (sum |> List.map Solver.sumEq)

        let prod, sum = order |> toEqs

        let eqs = toEql prod sum

        order |> _log Events.OrderSolveConstraintsStarted |> ignore

        try
            eqs
            |> Solver.solveConstraints log cs
            |> fromEqs order
            |> _log Events.OrderSolveConstraintsFinished
        with
        | e ->
            Exceptions.raiseExc (Some log) (e.ToString()) order

    let solveAll log o =
        // return eqs
        let toEql prod sum =

            prod
            |> List.map Solver.productEq
            |> List.append (sum |> List.map Solver.sumEq)

        let prod, sum = o |> toEqs

        let eqs = toEql prod sum

        try
            eqs
            |> Solver.solveAll log
            |> fromEqs o
        with
        | e ->
            Exceptions.raiseExc (Some log) (e.ToString()) o


    let calcScenarios log (order : Order) =

        let solve n v o =
            try
                o
                |> toEqs
                |> function
                | prod, sum ->
                    prod
                    |> List.map Solver.productEq
                    |> List.append (sum |> List.map Solver.sumEq)

                |> Solver.solve log n (v |> Set.singleton |> Property.createValsProp)
                |> fromEqs o
                |> Some
            with
            | e ->
                (e.ToString(), o)
                |> Exceptions.OrderCouldNotBeSolved
                |> Logging.logError log

                None

        let smallest o =
            o
            |> toEqs
            |> function
            | vrus1, vrus2 ->
                vrus1
                |> List.append vrus2
                |> List.collect id
                |> List.filter (fun vru ->
                    vru
                    |> VariableUnit.getBaseValues
                    |> Seq.length > 1
                )
            |> List.map (fun vru ->
                vru.Variable.Name, vru |> VariableUnit.getUnitValues
            )
            |> function
            | [] -> None
            | xs ->
                xs
                |> List.sortBy (fun (_, vs) -> vs |> Seq.length)
                |> List.tryHead

        let rec calc os sc =
            match sc with
            | None         ->
                os
            | Some (n, vs) ->
                (vs |> Seq.map BigRational.toString |> String.concat ",")
                |> sprintf "scenario: %A, with %A" n
                |> Events.OrderScenario
                |> Logging.logInfo log

                [
                    for v in vs do
                        for o in os do
                            if o |> contains n v then
                                (o, n, v)
                                |> Events.OrderScenerioWithNameValue
                                |> Logging.logInfo log

                                let o =
                                    o
                                    |> solve n v

                                if o |> Option.isSome then
                                    o
                                    |> Option.get

                ]
                |> List.map (fun o ->
                    o
                    |> smallest
                    |> calc [o]
                )
                |> List.collect id
                |> List.distinct

        let o = order |> solveAll log

        o
        |> smallest
        |> calc [ o ]


    let printItemConcentration (c : Component) =
        c.Items
        |> Seq.collect (fun i ->
            i.ComponentConcentration
            |> Concentration.toValueUnitStringList (Some 1)
            |> Seq.map (fun (_, s) ->
                $"{s} {i.Name |> Name.toString}"
            )
        )
        |> String.concat " + "


    let printComponentQuantity o =
        o.Orderable.Components
        |> Seq.map (fun c ->
            c.OrderableQuantity
            |> Quantity.toValueUnitStringList (Some 1)
            |> Seq.map (fun (_, q) ->
                let s =
                    c
                    |> printItemConcentration
                    |> String.trim
                    |> fun s ->
                        if s |> String.isNullOrWhiteSpace then ""
                        else
                            $" ({s})"
                $"{q} {c.Name |> Name.toString}{s}"
            )
            |> String.concat ""
        ) |> String.concat " + "


    let printOrderableDoseQuantity o =
        o.Orderable.Dose
        |> Dose.get
        |> fun (qt, _, _) ->
            qt
            |> Quantity.toValueUnitStringList (Some 2)
            |> Seq.map snd
            |> String.concat ""


    let printPrescription sn (o : Order) =
        let on = o.Orderable.Name |> Name.toString

        let printItem get unt o =
            o.Orderable.Components
            |> Seq.collect (fun c ->
                c.Items
                |> Seq.collect (fun i ->
                    let n = i.Name |> Name.toString
                    if sn |> Seq.exists ((=) n) then
                        i
                        |> get
                        |> unt
                        |> Seq.map snd
                        |> fun xs ->
                            if on |> String.startsWith n then
                                xs
                                |>Seq.map (sprintf "%s")
                            else
                                xs
                                |> Seq.map (sprintf "%s %s" n)

                    else Seq.empty
                )
            )
            |> String.concat " + "

        match o.Prescription with
        | Prescription.Discontinuous fr ->
            // frequencies
            let fr =
                fr
                |> Frequency.toValueUnitStringList None
                |> Seq.map snd
                |> String.concat ";"

            let dq =
                o
                |> printItem
                    (fun i -> i.Dose |> Dose.get |> (fun (dq, _, _) -> dq))
                    (VariableUnit.Quantity.toValueUnitStringList (Some 3))

            let dt =
                o
                |> printItem
                    (fun i -> i.DoseAdjust |> DoseAdjust.get |> (fun (_, dt, _) -> dt))
                    (VariableUnit.TotalAdjust.toValueUnitStringList (Some 2))

            let pres = $"{o.Orderable.Name |> Name.toString} {fr} {dq} ({dt})"
            let prep = $"{o |> printComponentQuantity}"
            let adm = $"{fr} {o |> printOrderableDoseQuantity}"

            pres, prep, adm

        | Prescription.Continuous ->
            // infusion rate
            let rt =
                o.Orderable.Dose
                |> Dose.get
                |> fun (_, _, dr) ->
                    dr
                    |> Rate.toValueUnitStringList (Some 1)
                    |> Seq.map snd
                    |> String.concat ""

            let oq =
                o.Orderable.OrderableQuantity
                |> Quantity.toValueUnitStringList (Some 2)
                |> Seq.map snd
                |> String.concat ""

            let it =
                o
                |> printItem
                    (fun i -> i.OrderableQuantity)
                    (Quantity.toValueUnitStringList (Some 2))

            let dr =
                o
                |> printItem
                    (fun i -> i.DoseAdjust |> DoseAdjust.get |> (fun (_, _, dr) -> dr))
                    (VariableUnit.RateAdjust.toValueUnitStringList (Some 2))

            let pres = $"""{sn |> String.concat " + "} {dr}"""
            let prep = o |> printComponentQuantity
            let adm = $"""{sn |> String.concat " + "} {it} in {oq}, {rt}"""

            pres, prep, adm

        | Prescription.Timed (fr, tme) ->

            // frequencies
            let fr =
                fr
                |> Frequency.toValueUnitStringList None
                |> Seq.map snd
                |> String.concat ";"

            let tme =
                tme
                |> Time.toValueUnitStringList (Some 2)
                |> Seq.map snd
                |> String.concat ""
            // infusion rate
            let rt =
                o.Orderable.Dose
                |> Dose.get
                |> fun (_, _, dr) ->
                    dr
                    |> Rate.toValueUnitStringList (Some 1)
                    |> Seq.map snd
                    |> String.concat ""

            let dq =
                o
                |> printItem
                    (fun i -> i.Dose |> Dose.get |> (fun (dq, _, _) -> dq))
                    (VariableUnit.Quantity.toValueUnitStringList (Some 3))

            let dt =
                o
                |> printItem
                    (fun i -> i.DoseAdjust |> DoseAdjust.get |> (fun (_, dt, _) -> dt))
                    (VariableUnit.TotalAdjust.toValueUnitStringList (Some 1))

            let pres = $"{o.Orderable.Name |> Name.toString} {fr} {dq} = ({dt}) {rt}"
            let prep = o |> printComponentQuantity
            let adm = $"{fr} {o |> printOrderableDoseQuantity} in {tme}, {rt}"

            pres, prep, adm


        | Prescription.Process ->
            let p =
                o.Orderable.Name
                |> Name.toString
                |> sprintf "%s"
            p, "", ""


    module Dto =

        type Dto (id , n, shape) =
            member val Id = id with get, set
            member val Adjust = VariableUnit.Dto.dto () with get, set
            member val Orderable = Orderable.Dto.dto id n shape with get, set
            member val Prescription = Prescription.Dto.dto n with get, set
            member val Route = "" with get, set
            member val Start = DateTime.now () with get, set
            member val Stop : DateTime option = None with get, set

        let fromDto (dto : Dto) =
            let id = dto.Id |> Id.create
            let adj_qty = dto.Adjust |> Quantity.fromDto
            let orb = dto.Orderable |> Orderable.Dto.fromDto
            let prs = dto.Prescription |> Prescription.Dto.fromDto
            let sts =
                match dto.Stop with
                | Some dt -> (dto.Start, dt) |> StartStop.StartStop
                | None -> dto.Start |> StartStop.Start

            create id adj_qty orb prs dto.Route sts


        let toDto (ord : Order) =
            let id = ord.Id |> Id.toString
            let n = ord.Orderable.Name |> Name.toString
            let dto = Dto (id, n, ord.Orderable.Shape)

            dto.Adjust <- ord.Adjust |> Quantity.toDto
            dto.Orderable <- ord.Orderable |> Orderable.Dto.toDto
            dto.Prescription <- ord.Prescription |> Prescription.Dto.toDto
            dto.Route <- ord.Route
            let start, stop =
                match ord.StartStop with
                | StartStop.Start dt -> (dt, None)
                | StartStop.StartStop(start, stop) -> (start, stop |> Some)
            dto.Start <- start
            dto.Stop <- stop

            dto

        let ``process`` id n shape rte =
            let id = id |> Id.create
            let n = [ n ] |> Name.create
            let str_prs =
                fun _ -> Prescription.``process``

            createNew id n shape str_prs rte
            |> toDto

        let continuous id n shape rte =
            let id = id |> Id.create
            let n = [ n ] |> Name.create
            let str_prs =
                Prescription.continuous ValueUnit.NoUnit ValueUnit.NoUnit

            createNew id n shape str_prs rte
            |> toDto

        let discontinuous id n shape rte =
            let id = id |> Id.create
            let n = [ n ] |> Name.create
            let str_prs =
                Prescription.discontinuous ValueUnit.NoUnit ValueUnit.NoUnit

            createNew id n shape str_prs rte
            |> toDto

        let timed id n shape rte =
            let id = id |> Id.create
            let n = [ n ] |> Name.create
            let str_prs =
                Prescription.timed ValueUnit.NoUnit ValueUnit.NoUnit

            createNew id n shape str_prs rte
            |> toDto

        let setToProcess (dto : Dto) =
            dto.Prescription <-
                dto.Prescription
                |> Prescription.Dto.setToProcess
            dto

        let setToContinuous (dto : Dto) =
            dto.Prescription <-
                dto.Prescription
                |> Prescription.Dto.setToContinuous
            dto

        let setToDiscontinuous (dto : Dto) =
            dto.Prescription <-
                dto.Prescription
                |> Prescription.Dto.setToDiscontinuous
            dto

        let setToTimed (dto : Dto) =
            dto.Prescription <-
                dto.Prescription
                |> Prescription.Dto.setToTimed
            dto