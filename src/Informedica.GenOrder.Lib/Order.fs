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

    /// Contains literals used
    /// to generate `Variable` names
    module Literals =

        [<Literal>]
        let adjust = "Adjust"

    /// Utitlity functions to
    /// enable mapping of a `Variable`s
    /// to an `Order`
    module Mapping =

        let map = function
            | PresFreq -> "Freq"
            | PresTime -> "Time"
            | ItemComponentQty -> "Item.Component.Qty"
            | ItemOrderableQty -> "Item.Orderable.Qty"
            | ItemComponentConc -> "Item.Component.Conc"
            | ItemOrderableConc -> "Item.Orderable.Conc"
            | ItemDoseQty -> "Item.Dose.Qty"
            | ItemDoseTotal -> "Item.Dose.Total"
            | ItemDoseRate -> "Item.Dose.Rate"
            | ItemDoseAdjustQtyAdjust -> "Item.DoseAdjust.QtyAdjust"
            | ItemDoseAdjustTotalAdjust -> "Item.DoseAdjust.TotalAdjust"
            | ItemDoseAdjustRateAdjust -> "Item.DoseAdjust.RateAdjust"
            | ComponentComponentQty -> "Component.Component.Qty"
            | ComponentOrderableQty -> "Component.Orderable.Qty"
            | ComponentOrderableCount -> "Component.Orderable.Count"
            | ComponentOrderCount -> "Component.Order.Count"
            | ComponentOrderableConc -> "Component.Orderable.Conc"
            | ComponentDoseQty -> "Component.Dose.Qty"
            | ComponentDoseTotal -> "Component.Dose.Total"
            | ComponentDoseRate -> "Component.Dose.Rate"
            | ComponentDoseAdjustQtyAdjust -> "Component.DoseAdjust.QtyAdjust"
            | ComponentDoseAdjustTotalAdjust -> "Component.DoseAdjust.TotalAdjust"
            | ComponentDoseAdjustRateAdjust -> "Component.DoseAdjust.RateAdjust"
            | OrderableOrderableQty -> "Orderable.Orderable.Qty"
            | OrderableOrderQty -> "Orderable.Order.Qty"
            | OrderableOrderCount -> "Orderable.Order.Count"
            | OrderableDoseCount -> "Orderable.Dose.Count"
            | OrderableDoseQty -> "Orderable.Dose.Qty"
            | OrderableDoseTotal -> "Orderable.Dose.Total"
            | OrderableDoseRate -> "Orderable.Dose.Rate"
            | OrderableDoseAdjustQtyAdjust -> "Orderable.DoseAdjust.QtyAdjust"
            | OrderableDoseAdjustTotalAdjust -> "Orderable.DoseAdjust.TotalAdjust"
            | OrderableDoseAdjustRateAdjust -> "Orderable.DoseAdjust.RateAdjust"
            | OrderAdjustQty -> "Adjust.Qty"



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
    module Property = Informedica.GenSolver.Lib.Property
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
        let adj = Quantity.quantity [ id |> Id.toString; n |> Name.toString; Literals.adjust ] ValueUnit.NoUnit
        let prs = [id |> Id.toString; nm |> Name.toString] |> str_prs
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


    let mapName n m o =   
        let dls = "."

        match m with
        | PresFreq 
        | OrderAdjustQty ->
            [ (o |> getName) + dls + (m |> Mapping.map)] |> Name.create
        | _ -> 
            [ (o.Id |> Id.toString) + dls + n + dls + (m |> Mapping.map)] 
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
        [ Literals.adjust; ord.Adjust |> Quantity.toString ]
        |> List.append (Orderable.Literals.orderable::(ord.Orderable |> Orderable.toString))
        |> List.append ("Prescription"::(ord.Prescription |> Prescription.toString))
        |> List.append ("Route"::[(ord.Route)])
        |> List.filter (String.isNullOrWhiteSpace >> not)


    let getVariableUnit n m o =
        let dls = "."

        let n =
            match m with
            | PresFreq 
            | OrderAdjustQty ->
                [ (o |> getName) + dls + (m |> Mapping.map)] |> Name.create
            | _ -> 
                [ (o.Id |> Id.toString) + dls + n + dls + (m |> Mapping.map)] 
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
    let solve log lim n m p o =
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
        |> Solver.solve log lim n p
        |> fromEqs o
        |> fun o ->
            o
            |> Events.OrderSolved
            |> Logging.logInfo log

            o

    let solveConstraints log cs o =
        // return eqs 
        let toEql prod sum =

            prod 
            |> List.map Solver.productEq
            |> List.append (sum |> List.map Solver.sumEq)

        let prod, sum = o |> toEqs
    
        let eqs = toEql prod sum
    
        eqs
        |> Solver.solveConstraints log cs
        |> fromEqs o
        |> fun o ->
            (o, cs)
            |> Events.OrderConstraintsSolved
            |> Logging.logInfo log

            o



    let calcScenarios log (o : Order) =

        let solve n v o =
            try 
                o
                |> toEqs
                |> function 
                | (prod, sum) ->
                    prod 
                    |> List.map Solver.productEq
                    |> List.append (sum |> List.map Solver.sumEq)

                |> Solver.solve log None n (v |> Set.singleton |> Property.createValsProp)
                |> fromEqs o
                |> Some
            with
            | e -> 
                e.ToString()
                |> sprintf "could not solve %A: %A\n%s" v n 
                |> failwith
                None

        let smallest o =
            o
            |> toEqs
            |> function
            | (vrus1, vrus2) ->
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

        // To do add logger    
        let rec calc os sc =
        
            match sc with
            | None         -> 
                os
            | Some (n, vs) ->
                let msg = 
                    (vs |> Seq.map BigRational.toString |> String.concat ",")
                    |> sprintf "scenario: %A, with %A" n

                msg
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

        o
        |> smallest
        |> calc [ o ]


    let printItemConcentration (c : Component) =
        c.Items
        |> Seq.collect (fun i ->
            i.ComponentConcentration
            |> Concentration.toValueUnitStringList None
            |> Seq.map (fun (_, s) ->
                $"{s} {i.Name |> Name.toString}"
            )
        )
        |> String.concat " + "


    let printComponentQuantity o =
        o.Orderable.Components
        |> Seq.map (fun c ->
            c.OrderableQuantity
            |> Quantity.toValueUnitStringList None
            |> Seq.map (fun (_, q) ->
                $"{q} {c.Name |> Name.toString} ({c |> printItemConcentration})"
            )
            |> String.concat ""
        ) |> String.concat " + "


    let printOrderableDoseQuantity o =
        o.Orderable.Dose
        |> Dose.get
        |> fun (qt, _, _) ->
            qt
            |> Quantity.toValueUnitStringList None
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
                    (VariableUnit.Quantity.toValueUnitStringList None)

            let dt =
                o
                |> printItem 
                    (fun i -> i.DoseAdjust |> DoseAdjust.get |> (fun (_, dt, _) -> dt))
                    (VariableUnit.TotalAdjust.toValueUnitStringList None)

            let p = $"{o.Orderable.Name |> Name.toString} {fr} {dq} ({dt})"
            let a = $"{fr} {o |> printOrderableDoseQuantity}"
            let d = o |> printComponentQuantity

            p, a, d

        | Prescription.Continuous ->
            // infusion rate
            let rt =
                o.Orderable.Dose
                |> Dose.get 
                |> fun (_, _, dr) ->
                    dr
                    |> Rate.toValueUnitStringList None
                    |> Seq.map snd
                    |> String.concat ""

            let oq =
                o.Orderable.OrderableQuantity
                |> Quantity.toValueUnitStringList None
                |> Seq.map snd
                |> String.concat ""

            let it =
                o
                |> printItem
                    (fun i -> i.OrderableQuantity)
                    (Quantity.toValueUnitStringList None)

            let dr =
                o
                |> printItem 
                    (fun i -> i.DoseAdjust |> DoseAdjust.get |> (fun (_, _, dr) -> dr))
                    (VariableUnit.RateAdjust.toValueUnitStringList (Some 2))

            let p = $"""{sn |> String.concat " + "} {dr}"""
            let a = $"""{sn |> String.concat " + "} {it} in {oq}, {rt}"""
            let d = o |> printComponentQuantity
        
            p, a, d

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
                    |> Rate.toValueUnitStringList None
                    |> Seq.map snd
                    |> String.concat ""

            let dq =
                o
                |> printItem 
                    (fun i -> i.Dose |> Dose.get |> (fun (dq, _, _) -> dq))
                    (VariableUnit.Quantity.toValueUnitStringList None)

            let dt =
                o
                |> printItem 
                    (fun i -> i.DoseAdjust |> DoseAdjust.get |> (fun (_, dt, _) -> dt))
                    (VariableUnit.TotalAdjust.toValueUnitStringList None)

            let p = $"{o.Orderable.Name |> Name.toString} {fr} {dq} = ({dt}) {rt}"  
            let d = o |> printComponentQuantity
            let a = $"{fr} {o |> printOrderableDoseQuantity} in {tme}, {rt}"

            p, a, d
 

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
            let (start, stop) =
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