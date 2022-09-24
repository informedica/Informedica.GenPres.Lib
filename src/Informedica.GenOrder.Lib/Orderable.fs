namespace Informedica.GenOrder.Lib


/// Types and functions to deal
/// with an `Orderable`, i.e. something
/// that can be ordered.
module Orderable =

    open Informedica.GenUnits.Lib
    open Informedica.GenSolver.Lib
    open WrappedString

    open Types

    type Name = Types.Name

    /// Contains string constants
    /// to create `Variable` names
    module Literals =

        [<Literal>]
        let item = "Item"
        [<Literal>]
        let ``component`` = "Component"
        [<Literal>]
        let orderable = "Orderable"
        [<Literal>]
        let order = "Order"
        [<Literal>]
        let dose = "Dose"
        [<Literal>]
        let doseAdjust = "DoseAdjust"

    /// Type and functions that models an
    /// `Order` `Item` that is contained in
    /// a `Component`
    module Item =

        module Quantity = VariableUnit.Quantity
        module Concentration = VariableUnit.Concentration
        module Total = VariableUnit.Total
        module Rate = VariableUnit.Rate
        module Dose = VariableUnit.Dose
        module DoseAdjust = VariableUnit.DoseAdjust


        /// Create an item with
        ///
        /// * **id**: the order id
        /// * **n**: the name of the item
        /// * **cmp_qty**: the quantity of the item in a component
        /// * **orb_qty**: the quantity of the item in an orderable
        /// * **cmp_cnc**: the item concentration in a component
        /// * **orb_cnc**: the item concentration in an orderable
        /// * **dos**: the item dose
        /// * **dos_adj**: the adjusted item dose
        let create id n cmp_qty orb_qty cmp_cnc orb_cnc dos dos_adj =
            {
                OrderId = id
                Name = n
                ComponentQuantity = cmp_qty
                OrderableQuantity = orb_qty
                ComponentConcentration = cmp_cnc
                OrderableConcentration = orb_cnc
                Dose = dos
                DoseAdjust = dos_adj
            }

        /// Create a new item with
        ///
        /// **id**: the order id
        /// **n**: the string name of the item
        let createNew id n =
            let s = [Literals.item] |> List.append [id |> Id.toString; n |> Name.toString]
            let un = ValueUnit.NoUnit

            let cmp_qty = let s = [Literals.``component``] |> List.append s in Quantity.quantity s un
            let orb_qty = let s = [Literals.orderable]     |> List.append s in Quantity.quantity s un
            let cmp_cnc = let s = [Literals.``component``] |> List.append s in Concentration.conc s un un
            let orb_cnc = let s = [Literals.orderable]     |> List.append s in Concentration.conc s un un
            let dos     = let s = [Literals.dose]          |> List.append s in Dose.dose s un un un
            let dos_adj = let s = [Literals.doseAdjust]    |> List.append s in DoseAdjust.doseAdjust s un un un un

            create id n cmp_qty orb_qty cmp_cnc orb_cnc dos dos_adj

        /// Aply **f** to an `item`
        let apply f (itm: Item) = itm |> f

        /// Utility method to facilitaite type inference
        let get = apply id

        /// Get the order id of an `Item`
        let getOrderId itm = (itm |> get).OrderId

        /// Get the `Name` of an `Item`
        let getName itm = (itm |> get).Name

        /// Get the unique id of an `Item`
        let getId itm =
            ((itm |> get).OrderId |> Id.toString) + "." +
            (itm.Name |> Name.toString)

        /// Get the `Item` dose
        let getDose itm = (itm |> get).Dose

        // Get the `Item` adjusted dose
        let getDoseAdjust itm = (itm |> get).DoseAdjust

        /// Turn an `Item` to `VariableUnit`s
        let toVarUnt itm =
            let itm_cmp_qty = (itm |> get).ComponentQuantity |> Quantity.toVarUnt
            let itm_orb_qty = itm.OrderableQuantity          |> Quantity.toVarUnt
            let itm_cmp_cnc = itm.ComponentConcentration     |> Concentration.toVarUnt
            let itm_orb_cnc = itm.OrderableConcentration     |> Concentration.toVarUnt

            let itm_dos_qty, itm_dos_tot, itm_dos_rte = itm.Dose                   |> Dose.toVarUnt
            let itm_dos_qty_adj, itm_dos_tot_adj, itm_dos_rte_adj = itm.DoseAdjust |> DoseAdjust.toVarUnt

            (
                itm_cmp_qty,
                itm_orb_qty,
                itm_cmp_cnc,
                itm_orb_cnc,
                itm_dos_qty,
                itm_dos_tot,
                itm_dos_rte,
                itm_dos_qty_adj,
                itm_dos_tot_adj,
                itm_dos_rte_adj
            )


        /// Turn an `Item` to a list of `string`s,
        /// each string containing the variable
        /// `Name`, `ValueRange` and `Unit`
        let toString itm =
            let itm_cmp_qty,
                itm_orb_qty,
                itm_cmp_cnc,
                itm_orb_cnc,
                itm_dos_qty,
                itm_dos_tot,
                itm_dos_rte,
                itm_dos_qty_adj,
                itm_dos_tot_adj,
                itm_dos_rte_adj = itm |> toVarUnt

            [
                itm_cmp_qty
                itm_orb_qty
                itm_cmp_cnc
                itm_orb_cnc
                itm_dos_qty
                itm_dos_tot
                itm_dos_rte
                itm_dos_qty_adj
                itm_dos_tot_adj
                itm_dos_rte_adj
            ]
            |> List.map (VariableUnit.toString false)


        /// The following variables are used
        ///
        /// * adj: the adjustment of the dose
        /// * frq: the prescription frequency
        /// * tme: the prescription time duration
        /// * qty: the orderable dose quantity
        /// * tot: the orderable dose total
        /// * rte: the orderable dose rate
        /// * qty_adj: the orderable adjusted dose quantity
        /// * tot_ajd: the orderable adjusted dose total
        /// * rte_adj: the orderable adjusted dose rate
        ///
        /// * cmp\_cmp\_qty: the component quantity
        /// * cmp\_orb\_qty: the quantity of component in an orderable
        /// * cmp\_dos\_qty: the component dose quantity
        /// * cmp\_dos\_tot: the component dose total
        /// * cmp\_dos\_rte: the component dose rate
        /// * cmp\_dos\_qty_adj: the component adjusted dose quantity
        /// * cmp\_dos\_tot_ajd: the component adjusted dose total
        /// * cmp\_dos\_rte_adj: the component adjusted dose rate
        /// * orb\_orb\_qty: the orderable quantity
        ///
        /// * itm: the item from which the following can be used:
        ///
        /// * itm\_cmp\_qty: the quantity of item in a component
        /// * itm\_orb\_qty: the quantity of item in an orderable
        /// * itm\_cmp\_cnc: the concentration of an item in a component
        /// * itm\_orb\_cnc: the concentration of an item in an orderable
        /// * itm\_dos\_qty: the item dose quantity
        /// * itm\_dos\_tot: the item dose total
        /// * itm\_dos\_rte: the item dose rate
        /// * itm\_dos\_qty\_adj: the adjusted item dose quantity
        /// * itm\_dos\_tot\_adj: the adjusted item dose total
        /// * itm\_dos\_rte\_adj: the adjusted item dose rate
        ///
        /// With these variables the following equations are generated
        /// depending on prescription type
        ///
        /// *Process*
        ///
        /// * itm\_cmp\_qty = itm\_cmp\_cnc \* cmp\_cmp\_qty ItemComponentQuantiy = ItemComponentConcentration * CompoentQuantity
        /// * itm\_orb\_qty = itm\_orb\_cnc \* orb\_orb\_qty ItemOrderableQuantity = ItemOrderableConcentration * OrderableQuantity
        /// * itm\_orb\_qty = itm\_cmp\_cnc \* cmp\_orb\_qty ItemOrderableQuantity = ItemComponentConcentration * ComponentOrderableQuantity
        ///
        /// *Discontinuous Timed*
        ///
        /// * itm\_dos\_tot = itm\_dos\_qty \* frq ItemDoseTotal = ItemDoseQuantity * Frequency
        /// * itm\_dos\_qty = itm\_dos\_rte \* tme ItemDoseQuantity = ItemDoseRate * Time
        /// * itm\_dos\_qty = itm\_orb\_cnc \* qty ItemDoseQuantity = ItemOrderableConcentration * OrderableDoseQuantity
        /// * itm\_dos\_tot = itm\_orb\_cnc \* tot ItemDoseTotal = ItemOrderableConcentration * OrderableDoseTotal
        /// * itm\_dos\_rte = itm\_orb\_cnc \* rte ItemDoseRate = ItemOrderableConcentration * OrderableDoseRate
        /// * itm\_dos\_qty\_adj = itm\_orb\_cnc \* qty\_adj ItemDoseQuantityAdjust = ItemOrderableConcentration * OrderableDoseQuantityAdjust
        /// * itm\_dos\_tot\_adj = itm\_orb\_cnc \* tot\_adj ItemDoseTotalAdjust = ItemOrderableConcentration * OrderableDoseTotalAdjust
        /// * itm\_dos\_rte\_adj = itm\_orb\_cnc \* rte\_adj ItemDoseRateAdjust = ItemOrderableConcentration * OrderableDoseRateAdjust
        /// * itm\_dos\_qty = itm\_cmp\_cnc \* qty ItemDoseQuantity = ItemComponentConcentration * ComponentDoseQuantity
        /// * itm\_dos\_tot = itm\_cmp\_cnc \* tot ItemDoseTotal = ItemComponentConcentration * ComponentDoseTotal
        /// * itm\_dos\_rte = itm\_cmp\_cnc \* rte ItemDoseRate = ItemComponentConcentration * ComponentDoseRate
        /// * itm\_dos\_qty\_adj = itm\_cmp\_cnc \* qty\_adj ItemDoseQuantityAdjust = ItemComponentConcentration * ComponentDoseQuantityAdjust
        /// * itm\_dos\_tot\_adj = itm\_cmp\_cnc \* tot\_adj ItemDoseTotalAdjust = ItemComponentConcentration * ComponentDoseTotalAdjust
        /// * itm\_dos\_rte\_adj = itm\_cmp\_cnc \* rte\_adj ItemDoseRateAdjust = ItemComponentConcentration * ComponentDoseRateAdjust
        /// * itm\_dos\_tot\_adj = itm\_dos\_qty\_adj \* frq = ItemDoseTotalAdjust = ItemDoseQuantityAdjust * frq
        /// * itm\_dos\_qty\_adj = itm\_dos\_rte \* tme = ItemDoseQuantityAdjust = ItemDoseRateAdjust * tme
        /// * itm\_dos\_qty = itm\_dos\_qty\_adj \* adj ItemDoseQuantity = ItemDoseQuantityAdjust * Adjust
        /// * itm\_dos\_tot = itm\_dos\_tot\_adj \* adj ItemDoseTotal = ItemDoseTotalAdjust * Adjust
        /// * itm\_dos\_rte = itm\_dos\_rte\_adj \* adj ItemDoseRate = ItemDoseRate * Adjust
        ///
        /// *Discontinuous*
        ///
        /// * itm\_dos\_tot = itm\_dos\_qty \* frq ItemDoseTotal = ItemDoseQuantity * Frequency
        /// * itm\_dos\_qty = itm\_orb\_cnc \* qty ItemDoseQuantity = ItemOrderableConcentration * OrderableDoseQuantity
        /// * itm\_dos\_tot = itm\_orb\_cnc \* tot ItemDoseTotal = ItemOrderableConcentration * OrderableDoseTotal
        /// * itm\_dos\_qty\_adj = itm\_orb\_cnc \* qty\_adj ItemDoseQuantityAdjust = ItemOrderableConcentration * OrderableDoseQuantityAdjust
        /// * itm\_dos\_tot\_adj = itm\_orb\_cnc \* tot\_adj ItemDoseTotalAdjust = ItemOrderableConcentration * OrderableDoseTotalAdjust
        /// * itm\_dos\_qty = itm\_cmp\_cnc \* qty ItemDoseQuantity = ItemComponentConcentration * ComponentDoseQuantity
        /// * itm\_dos\_tot = itm\_cmp\_cnc \* tot ItemDoseTotal = ItemComponentConcentration * ComponentDoseTotal
        /// * itm\_dos\_qty\_adj = itm\_cmp\_cnc \* qty\_adj ItemDoseQuantityAdjust = ItemComponentConcentration * ComponentDoseQuantityAdjust
        /// * itm\_dos\_tot\_adj = itm\_cmp\_cnc \* tot\_adj ItemDoseTotalAdjust = ItemComponentConcentration * ComponentDoseTotalAdjust
        /// * itm\_dos\_tot\_adj = itm\_dos\_qty\_adj \* frq = ItemDoseTotalAdjust = ItemDoseQuantityAdjust * frq
        /// * itm\_dos\_qty = itm\_dos\_qty\_adj \* adj ItemDoseQuantity = ItemDoseQuantityAdjust * Adjust
        /// * itm\_dos\_tot = itm\_dos\_tot\_adj \* adj ItemDoseTotal = ItemDoseTotalAdjust * Adjust
        ///
        /// *Continuous*
        ///
        /// * itm\_dos\_rte = itm\_orb\_cnc \* rte ItemDoseRate = ItemOrderableConcentration * OrderableDoseRate
        /// * itm\_dos\_rte = itm\_dos\_rte\_adj \* adj = ItemDoseRate = ItemDoseRateAdjust * Adjust
        /// * itm\_dos\_rte\_adj = itm\_orb\_cnc \* rte\_adj ItemDoseRateAdjust = ItemOrderableConcentration * OrderableDoseRateAdjust
        /// * itm\_dos\_rte = itm\_cmp\_cnc \* rte ItemDoseRate = ItemComponentConcentration * ComponentDoseRate
        /// * itm\_dos\_rte\_adj = itm\_cmp\_cnc \* rte\_adj ItemDoseRateAdjust = ItemComponentConcentration * ComponentDoseRateAdjust
        let toEqs adj frq tme qty tot rte qty_adj tot_adj rte_adj
                  cmp_cmp_qty cmp_orb_qty
                  cmp_dos_qty cmp_dos_tot cmp_dos_rte cmp_dos_qty_adj cmp_dos_tot_adj cmp_dos_rte_adj
                  orb_orb_qty itm =
            let itm_cmp_qty,
                itm_orb_qty,
                itm_cmp_cnc,
                itm_orb_cnc,
                itm_dos_qty,
                itm_dos_tot,
                itm_dos_rte,
                itm_dos_qty_adj,
                itm_dos_tot_adj,
                itm_dos_rte_adj = itm |> toVarUnt

            let eqs =
                [
                    [ itm_cmp_qty; itm_cmp_cnc; cmp_cmp_qty ]
                    [ itm_orb_qty; itm_orb_cnc; orb_orb_qty ]
                    [ itm_orb_qty; itm_cmp_cnc; cmp_orb_qty ]
                ]

            match rte, frq, tme, rte_adj, cmp_dos_rte, cmp_dos_rte_adj with
            // Discontinuous timed
            | Some rte, Some frq, Some tme, Some rte_adj, Some cmp_dos_rte, Some cmp_dos_rte_adj ->
                [
                    [ itm_dos_tot;     itm_dos_qty;     frq ]
                    [ itm_dos_qty;     itm_dos_rte;     tme ]
                    [ itm_dos_qty;     itm_orb_cnc;     qty ]
                    [ itm_dos_tot;     itm_orb_cnc;     tot ]
                    [ itm_dos_rte;     itm_orb_cnc;     rte ]
                    [ itm_dos_qty_adj; itm_orb_cnc;     qty_adj ]
                    [ itm_dos_tot_adj; itm_orb_cnc;     tot_adj ]
                    [ itm_dos_rte_adj; itm_orb_cnc;     rte_adj ]
                    [ itm_dos_qty;     itm_cmp_cnc;     cmp_dos_qty ]
                    [ itm_dos_tot;     itm_cmp_cnc;     cmp_dos_tot ]
                    [ itm_dos_rte;     itm_cmp_cnc;     cmp_dos_rte ]
                    [ itm_dos_qty_adj; itm_cmp_cnc;     cmp_dos_qty_adj ]
                    [ itm_dos_tot_adj; itm_cmp_cnc;     cmp_dos_tot_adj ]
                    [ itm_dos_rte_adj; itm_cmp_cnc;     cmp_dos_rte_adj ]
                    [ itm_dos_tot_adj; itm_dos_qty_adj; frq ]
                    [ itm_dos_qty_adj; itm_dos_rte_adj; tme ]
                    [ itm_dos_qty;     itm_dos_qty_adj; adj ]
                    [ itm_dos_tot;     itm_dos_tot_adj; adj ]
                    [ itm_dos_rte;     itm_dos_rte_adj; adj ]
                ] |> List.append eqs
            // Discontinuous
            | None, Some frq, None, None, None, None   ->
                [
                    [ itm_dos_tot;     itm_dos_qty;     frq ]
                    [ itm_dos_qty;     itm_orb_cnc;     qty ]
                    [ itm_dos_tot;     itm_orb_cnc;     tot ]
                    [ itm_dos_qty_adj; itm_orb_cnc;     qty_adj ]
                    [ itm_dos_tot_adj; itm_orb_cnc;     tot_adj ]
                    [ itm_dos_qty;     itm_cmp_cnc;     cmp_dos_qty ]
                    [ itm_dos_tot;     itm_cmp_cnc;     cmp_dos_tot ]
                    [ itm_dos_qty_adj; itm_cmp_cnc;     cmp_dos_qty_adj ]
                    [ itm_dos_tot_adj; itm_cmp_cnc;     cmp_dos_tot_adj ]
                    [ itm_dos_tot_adj; itm_dos_qty_adj; frq ]
                    [ itm_dos_qty;     itm_dos_qty_adj; adj ]
                    [ itm_dos_tot;     itm_dos_tot_adj; adj ]
                ] |> List.append eqs
            // Continuous
            | Some rte, _, _, Some rte_adj, Some cmp_dos_rte, Some cmp_dos_rte_adj ->
                [
                    [ itm_dos_rte; itm_dos_rte_adj; adj ]
                    [ itm_dos_rte; itm_orb_cnc;     rte ]
                    [ itm_dos_rte_adj; itm_orb_cnc; rte_adj ]
                    [ itm_dos_rte;     itm_cmp_cnc; cmp_dos_rte ]
                    [ itm_dos_rte_adj; itm_cmp_cnc; cmp_dos_rte_adj ]
                ] |> List.append eqs
            // Process
            | _ -> eqs

        /// create an item from `eqs`, a list of
        /// variable lists
        let fromEqs eqs (itm: Item) =
            {
                itm with
                    ComponentQuantity = Quantity.fromVar eqs itm.ComponentQuantity
                    OrderableQuantity = Quantity.fromVar eqs  itm.OrderableQuantity
                    ComponentConcentration = Concentration.fromVar eqs  itm.ComponentConcentration
                    OrderableConcentration = Concentration.fromVar eqs  itm.OrderableConcentration
                    Dose = Dose.fromVar eqs itm.Dose
                    DoseAdjust = DoseAdjust.fromVar eqs  itm.DoseAdjust
            }

        module Dto =

            module Units = ValueUnit.Units
            module Id = WrappedString.Id
            module Name = WrappedString.Name
            module Quantity = VariableUnit.Quantity
            module Concentration = VariableUnit.Concentration
            module Dose = VariableUnit.Dose
            module DoseAdjust = VariableUnit.DoseAdjust

            type Dto () =
                member val Id = "" with get, set
                member val Name = "" with get, set
                member val ComponentQuantity = VariableUnit.Dto.dto () with get, set
                member val OrderableQuantity = VariableUnit.Dto.dto () with get, set
                member val ComponentConcentration = VariableUnit.Dto.dto () with get, set
                member val OrderableConcentration = VariableUnit.Dto.dto () with get, set
                member val DoseQuantity = VariableUnit.Dto.dto () with get, set
                member val DoseTotal = VariableUnit.Dto.dto () with get, set
                member val DoseRate = VariableUnit.Dto.dto () with get, set
                member val DoseQuantityAdjust = VariableUnit.Dto.dto () with get, set
                member val DoseTotalAdjust = VariableUnit.Dto.dto () with get, set
                member val DoseRateAdjust = VariableUnit.Dto.dto () with get, set

            let fromDto (dto: Dto) =

                let id = dto.Id |> Id.create
                let n = [ dto.Name ] |> Name.create
                let cmp_qty = dto.ComponentQuantity |> Quantity.fromDto
                let orb_qty = dto.OrderableQuantity |> Quantity.fromDto
                let cmp_cnc = dto.ComponentConcentration |> Concentration.fromDto
                let orb_cnc = dto.OrderableConcentration |> Concentration.fromDto
                let dos =
                    (dto.DoseQuantity, dto.DoseTotal, dto.DoseRate)
                    |> Dose.fromDto
                let dos_adj =
                    (dto.DoseQuantityAdjust, dto.DoseTotalAdjust, dto.DoseRateAdjust)
                    |> DoseAdjust.fromDto

                create id n cmp_qty orb_qty cmp_cnc orb_cnc dos dos_adj

            let toDto (itm : Item) =
                let dto = Dto ()

                dto.Id <- itm.OrderId |> Id.toString
                dto.Name <- itm.Name |> Name.toString
                dto.ComponentQuantity <-
                    itm.ComponentQuantity
                    |> Quantity.toDto
                dto.OrderableQuantity <-
                    itm.OrderableQuantity
                    |> Quantity.toDto
                dto.ComponentConcentration <-
                    itm.ComponentConcentration
                    |> Concentration.toDto
                dto.OrderableConcentration <-
                    itm.OrderableConcentration
                    |> Concentration.toDto

                let q, t, r = itm.Dose |> Dose.toDto
                dto.DoseQuantity <- q
                dto.DoseTotal <- t
                dto.DoseRate <- r

                let q, t, r = itm.DoseAdjust |> DoseAdjust.toDto
                dto.DoseQuantityAdjust <- q
                dto.DoseTotalAdjust <- t
                dto.DoseRateAdjust <- r

                dto

            let dto id n =
                let id = id |> Id.create
                let n = [ n ] |> Name.create

                createNew id n
                |> toDto



    /// Types and functions to model a
    /// `Component` in an `Orderable`.
    /// A `Component` contains a list
    /// of `Item`s
    module Component =

        module Name = Name
        module Quantity = VariableUnit.Quantity
        module Concentration = VariableUnit.Concentration
        module Count = VariableUnit.Count
        module Total = VariableUnit.Total
        module Rate = VariableUnit.Rate
        module Dose = VariableUnit.Dose
        module DoseAdjust = VariableUnit.DoseAdjust


        /// Create a component with
        ///
        /// * `id`: the order id
        /// * `n`: the name of the component
        /// * `cmp_qty`: quantity of component
        /// * `orb_qty`: quantity of component in orderable
        /// * `orb_cnt`: count of component in orderable
        /// * `ord_qty`: quantity of component in order
        /// * `ord_cnt`: count of component in order
        /// * `orb_cnc`: concentration of component in orderble
        /// * `dos`: component dose
        /// * `dos_adj`: adjusted dose of component
        /// * `ii`: list of `Item`s in a component
        let create id n cmp_qty orb_qty orb_cnt ord_qty ord_cnt orb_cnc dos dos_adj ii =
            {
                OrderId = id
                Name = n
                ComponentQuantity = cmp_qty
                OrderableQuantity = orb_qty
                OrderableCount = orb_cnt
                OrderQuantity = ord_qty
                OrderCount = ord_cnt
                OrderableConcentration = orb_cnc
                Dose = dos
                DoseAdjust = dos_adj
                Items = ii
            }

        /// Create a new component with
        /// * `id`: the id of the component
        /// * `n`: the name of the component
        let createNew id n =
            let s = [id |> Id.toString; n |> Name.toString; Literals.``component``]
            let un = ValueUnit.NoUnit

            let cmp_qty = let s = [Literals.``component``] |> List.append s in Quantity.quantity s un
            let orb_qty = let s = [Literals.orderable]     |> List.append s in Quantity.quantity s un
            let orb_cnt = let s = [Literals.orderable]     |> List.append s in Count.count s
            let ord_qty = let s = [Literals.order]         |> List.append s in Quantity.quantity s un
            let ord_cnt = let s = [Literals.order]         |> List.append s in Count.count s
            let orb_cnc = let s = [Literals.orderable]     |> List.append s in Concentration.conc s un un
            let dos = let s = [Literals.dose]              |> List.append s in Dose.dose s un un un
            let dos_adj = let s = [Literals.doseAdjust]    |> List.append s in DoseAdjust.doseAdjust s un un un un

            create id n cmp_qty orb_qty orb_cnt ord_qty ord_cnt orb_cnc dos dos_adj []

        /// Apply **f** to a `Component` **comp**
        let apply f (comp: Component) = comp |> f

        /// Utility to facilitate type inference
        let get = apply id

        /// Get the id of a `Component`
        let getId cmp =
            ((cmp |> get).OrderId |> Id.toString) + "." +
            (cmp.Name |> Name.toString)

        /// Get the name of a `Component`
        let getName cmp = (cmp |> get).Name

        /// Get the `Item`s in an `Component`
        let getItems cmp = (cmp |> get).Items

        /// Map a `Component` **cmp**
        /// to `VariableUnit`s
        let toVar cmp =
            let cmp_cmp_qty = (cmp |> get).ComponentQuantity |> Quantity.toVarUnt
            let cmp_orb_qty = cmp.OrderableQuantity          |> Quantity.toVarUnt
            let cmp_orb_cnt = cmp.OrderableCount             |> Count.toVarUnt
            let cmp_orb_cnc = cmp.OrderableConcentration     |> Concentration.toVarUnt
            let cmp_ord_qty = cmp.OrderQuantity              |> Quantity.toVarUnt
            let cmp_ord_cnt = cmp.OrderCount                 |> Count.toVarUnt

            let cmp_dos_qty,     cmp_dos_tot,     cmp_dos_rte     = cmp.Dose       |> Dose.toVarUnt
            let cmp_dos_qty_adj, cmp_dos_tot_adj, cmp_dos_rte_adj = cmp.DoseAdjust |> DoseAdjust.toVarUnt

            (
                cmp_cmp_qty,
                cmp_orb_qty,
                cmp_orb_cnt,
                cmp_orb_cnc,
                cmp_ord_qty,
                cmp_ord_cnt,
                cmp_dos_qty,
                cmp_dos_tot,
                cmp_dos_rte,
                cmp_dos_qty_adj,
                cmp_dos_tot_adj,
                cmp_dos_rte_adj
            )

        /// Create a string list from a
        /// component where each string is
        /// a variable name with the valuerange
        /// and the Unit
        let toString cmp =
            let cmp_cmp_qty,
                cmp_orb_qty,
                cmp_orb_cnt,
                cmp_orb_cnc,
                cmp_ord_qty,
                cmp_ord_cnt,
                cmp_dos_qty,
                cmp_dos_tot,
                cmp_dos_rte,
                cmp_dos_qty_adj,
                cmp_dos_tot_adj,
                cmp_dos_rte_adj = cmp |> toVar

            let ii = cmp.Items

            [
                cmp_cmp_qty
                cmp_orb_qty
                cmp_orb_cnt
                cmp_orb_cnc
                cmp_ord_qty
                cmp_ord_cnt
                cmp_dos_qty
                cmp_dos_tot
                cmp_dos_rte
                cmp_dos_qty_adj
                cmp_dos_tot_adj
                cmp_dos_rte_adj
            ]
            |> List.map (VariableUnit.toString false)
            |> List.append (ii |> List.collect Item.toString )


        /// The following variables are used
        ///
        /// * adj: the adjustment of the dose
        /// * frq: the prescription frequency
        /// * tme: the prescription time
        /// * qty: the orderable dose quantity
        /// * tot: the orderable dose total
        /// * rte: the orderable dose rate
        /// * qty_adj: the orderable adjusted dose quantity
        /// * tot_adj: the orderable adjusted dose total
        /// * rte_adj: the orderable adjusted dose rate
        ///
        /// * orb\_orb\_qty: the orderable quantity
        /// * cmp : the component
        ///
        /// The following is derived from the component
        /// * cmp\_cmp\_qty: the component quantity
        /// * cmp\_orb\_qty: the quantity of component in an orderable
        /// * cmp\_orb\_cnt: the count of component in an orderable
        /// * cmp\_orb\_cnc: the concentration of an component in an orderable
        /// * cmp\_ord\_qty: the quantity of component in an order
        /// * cmp\_ord\_cnt: the count of component in an order
        /// * cmp\_dos\_qty: the component dose quantity
        /// * cmp\_dos\_tot: the component dose total
        /// * cmp\_dos\_rte: the component dose rate
        /// * cmp\_dos\_qty\_adj: the adjusted component dose quantity
        /// * cmp\_dos\_tot\_adj: the adjusted component dose total
        /// * cmp\_dos\_rte\_adj: the adjusted component dose rate
        ///
        /// The following equations are generated:
        ///
        /// *Process*
        ///
        /// * cmp\_orb\_qty = cmp\_orb\_cnc \* orb\_orb\_qty
        /// * orb\_orb\_qty = cmp\_cmp\_qty \* cmp\_orb\_cnt
        /// * orb\_ord\_qty = cmp\_cmp\_qty \* cmp\_ord\_cnt
        ///
        /// *Discontinuous Timed*
        ///
        /// * cmp\_dos\_tot = cmp\_dos\_qty \* frq
        /// * cmp\_dos\_qty = cmp\_dos\_rte \* tme
        /// * cmp\_dos\_qty = cmp\_orb\_cnc \* qty
        /// * cmp\_dos\_tot = cmp\_orb\_cnc \* tot
        /// * cmp\_dos\_rte = cmp\_orb\_cnc \* rte
        /// * cmp\_dos\_qty\_adj = cmp\_orb\_cnc \* qty\_adj
        /// * cmp\_dos\_tot\_adj = cmp\_orb\_cnc \* tot\_adj
        /// * cmp\_dos\_rte\_adj = cmp\_orb\_cnc \* rte\_adj
        /// * cmp\_dos\_tot\_adj = cmp\_dos\_qty\_adj \* frq
        /// * cmp\_dos\_qty\_ajd = cmp\_dos\_rte\_adj \* tme
        /// * cmp\_dos\_qty = cmp\_dos\_qty\_adj \* adj
        /// * cmp\_dos\_tot = cmp\_dos\_tot\_adj \* adj
        /// * cmp\_dos\_rte = cmp\_dos\_rte\_adj \* adj
        ///
        /// *Discontinuous*
        ///
        /// * cmp\_dos\_tot = cmp\_dos\_qty \* frq
        /// * cmp\_dos\_qty = cmp\_orb\_cnc \* qty
        /// * cmp\_dos\_tot = cmp\_orb\_cnc \* tot
        /// * cmp\_dos\_qty\_adj = cmp\_orb\_cnc \* qty\_adj
        /// * cmp\_dos\_tot\_adj = cmp\_orb\_cnc \* tot\_adj
        /// * cmp\_dos\_tot\_adj = cmp\_dos\_qty\_adj \* frq
        /// * cmp\_dos\_qty = cmp\_dos\_qty\_adj \* adj
        /// * cmp\_dos\_tot = cmp\_dos\_tot\_adj \* adj
        ///
        /// *Continuous*
        ///
        /// * cmp\_dos\_rte = cmp\_orb\_cnc \* rte
        /// * cmp\_dos\_rte = cmp\_dos\_rte\_adj \* adj
        /// * cmp\_dos\_rte\_adj = cmp\_orb\_cnc \* rte\_adj
        let toEqs adj frq tme qty tot rte qty_adj tot_adj rte_adj
                  orb_orb_qty cmp =
            let cmp_cmp_qty,
                cmp_orb_qty,
                cmp_orb_cnt,
                cmp_orb_cnc,
                _, //cmp_ord_qty,
                _, //cmp_ord_cnt,
                cmp_dos_qty,
                cmp_dos_tot,
                cmp_dos_rte,
                cmp_dos_qty_adj,
                cmp_dos_tot_adj,
                cmp_dos_rte_adj = cmp |> toVar

            let map =
                let cmp_dos_rte, cmp_dos_rte_adj =
                    match rte with
                    | Some _ -> (Some cmp_dos_rte, Some cmp_dos_rte_adj)
                    | _      -> (None, None)
                Item.toEqs
                    adj frq tme qty tot rte qty_adj tot_adj rte_adj
                    cmp_cmp_qty cmp_orb_qty
                    cmp_dos_qty cmp_dos_tot cmp_dos_rte cmp_dos_qty_adj cmp_dos_tot_adj cmp_dos_rte_adj
                    orb_orb_qty
            let ii = cmp.Items

            let eqs =
                [
                    [ cmp_orb_qty; cmp_orb_cnc; orb_orb_qty ]
                    [ orb_orb_qty; cmp_cmp_qty; cmp_orb_cnt ]
//                    [ orb_ord_qty; cmp_cmp_qty; cmp_ord_cnt ]
                ]

            match rte, frq, tme, rte_adj with
            // Discontinuous timed
            | Some rte, Some frq, Some tme, Some rte_adj ->
                [
                    [ cmp_dos_tot;     cmp_dos_qty;     frq ]
                    [ cmp_dos_qty;     cmp_dos_rte;     tme ]
                    [ cmp_dos_qty;     cmp_orb_cnc;     qty ]
                    [ cmp_dos_tot;     cmp_orb_cnc;     tot ]
                    [ cmp_dos_rte;     cmp_orb_cnc;     rte ]
                    [ cmp_dos_qty_adj; cmp_orb_cnc;     qty_adj ]
                    [ cmp_dos_tot_adj; cmp_orb_cnc;     tot_adj ]
                    [ cmp_dos_rte_adj; cmp_orb_cnc;     rte_adj ]
                    [ cmp_dos_tot_adj; cmp_dos_qty_adj; frq ]
                    [ cmp_dos_qty_adj; cmp_dos_rte_adj; tme ]
                    [ cmp_dos_qty;     cmp_dos_qty_adj; adj ]
                    [ cmp_dos_tot;     cmp_dos_tot_adj; adj ]
                    [ cmp_dos_rte;     cmp_dos_rte_adj; adj ]
                ] |> List.append eqs
            // Discontinuous
            | None, Some frq, None, None   ->
                [
                    [ cmp_dos_tot;     cmp_dos_qty;     frq ]
                    [ cmp_dos_qty;     cmp_orb_cnc;     qty ]
                    [ cmp_dos_tot;     cmp_orb_cnc;     tot ]
                    [ cmp_dos_qty_adj; cmp_orb_cnc;     qty_adj ]
                    [ cmp_dos_tot_adj; cmp_orb_cnc;     tot_adj ]
                    [ cmp_dos_tot_adj; cmp_dos_qty_adj; frq ]
                    [ cmp_dos_qty;     cmp_dos_qty_adj; adj ]
                    [ cmp_dos_tot;     cmp_dos_tot_adj; adj ]
                ] |> List.append eqs
            // Continuous
            | Some rte, None, _, Some rte_adj ->
                [
                    [ cmp_dos_rte; cmp_orb_cnc;     rte ]
                    [ cmp_dos_rte; cmp_dos_rte_adj; adj ]
                    [ cmp_dos_rte_adj; cmp_orb_cnc; rte_adj ]
                ] |> List.append eqs
            // Process
            | _ -> eqs
            |> List.append (ii |> List.collect map)

        /// Create a `Component` from a list
        /// of variable list eqs
        let fromEqs eqs  (cmp: Component) =
            let items =
                cmp.Items
                |> List.map (Item.fromEqs eqs )
            {
                cmp with
                    ComponentQuantity = Quantity.fromVar eqs  cmp.ComponentQuantity
                    OrderableQuantity = Quantity.fromVar eqs  cmp.OrderableQuantity
                    OrderableCount = Count.fromVar eqs  cmp.OrderableCount
                    OrderQuantity = Quantity.fromVar eqs  cmp.OrderQuantity
                    OrderCount = Count.fromVar eqs  cmp.OrderCount
                    OrderableConcentration = Concentration.fromVar eqs  cmp.OrderableConcentration
                    Dose = Dose.fromVar eqs  cmp.Dose
                    DoseAdjust = DoseAdjust.fromVar eqs  cmp.DoseAdjust
                    Items = items
            }


        module Dto =

            module Units = ValueUnit.Units
            module Id = WrappedString.Id
            module Name = WrappedString.Name
            module Quantity = VariableUnit.Quantity
            module Concentration = VariableUnit.Concentration
            module Dose = VariableUnit.Dose
            module DoseAdjust = VariableUnit.DoseAdjust
            module CT = VariableUnit.Count

            type Dto () =
                member val Id = "" with get, set
                member val Name = "" with get, set
                member val ComponentQuantity = VariableUnit.Dto.dto () with get, set
                member val OrderableQuantity = VariableUnit.Dto.dto () with get, set
                member val OrderableCount = VariableUnit.Dto.dto () with get, set
                member val OrderQuantity = VariableUnit.Dto.dto () with get, set
                member val OrderCount = VariableUnit.Dto.dto () with get, set
                member val OrderableConcentration = VariableUnit.Dto.dto () with get, set
                member val DoseQuantity = VariableUnit.Dto.dto () with get, set
                member val DoseTotal = VariableUnit.Dto.dto () with get, set
                member val DoseRate = VariableUnit.Dto.dto () with get, set
                member val DoseQuantityAdjust = VariableUnit.Dto.dto () with get, set
                member val DoseTotalAdjust = VariableUnit.Dto.dto () with get, set
                member val DoseRateAdjust = VariableUnit.Dto.dto () with get, set
                member val Items : Item.Dto.Dto list = [] with get, set

            let fromDto (dto: Dto) =

                let id = dto.Id |> Id.create
                let n = [ dto.Name ] |> Name.create
                let cmp_qty = dto.ComponentQuantity |> Quantity.fromDto
                let orb_qty = dto.OrderableQuantity |> Quantity.fromDto
                let orb_cnt = dto.OrderableCount    |> Count.fromDto
                let orb_cnc = dto.OrderableConcentration |> Concentration.fromDto
                let ord_qty = dto.OrderQuantity |> Quantity.fromDto
                let ord_cnt = dto.OrderCount    |> Count.fromDto
                let ii =
                    dto.Items
                    |> List.map Item.Dto.fromDto

                let dos =
                    (dto.DoseQuantity, dto.DoseTotal, dto.DoseRate)
                    |> Dose.fromDto
                let dos_adj =
                    (dto.DoseQuantityAdjust, dto.DoseTotalAdjust, dto.DoseRateAdjust)
                    |> DoseAdjust.fromDto

                create id n cmp_qty orb_qty orb_cnt ord_qty ord_cnt orb_cnc dos dos_adj ii

            let toDto (cmp : Component) =
                let dto = Dto ()

                dto.Id <- cmp.OrderId |> Id.toString
                dto.Name <- cmp.Name |> Name.toString
                dto.ComponentQuantity <-
                    cmp.ComponentQuantity
                    |> Quantity.toDto
                dto.OrderableQuantity <-
                    cmp.OrderableQuantity
                    |> Quantity.toDto
                dto.OrderableCount <-
                    cmp.OrderableCount
                    |> Count.toDto
                dto.OrderQuantity <-
                    cmp.OrderQuantity
                    |> Quantity.toDto
                dto.OrderCount <-
                    cmp.OrderCount
                    |> Count.toDto
                dto.OrderableConcentration <-
                    cmp.OrderableConcentration
                    |> Concentration.toDto

                let q, t, r = cmp.Dose |> Dose.toDto
                dto.DoseQuantity <- q
                dto.DoseTotal <- t
                dto.DoseRate <- r

                let q, t, r = cmp.DoseAdjust |> DoseAdjust.toDto
                dto.DoseQuantityAdjust <- q
                dto.DoseTotalAdjust <- t
                dto.DoseRateAdjust <- r

                dto.Items <-
                    cmp.Items
                    |> List.map Item.Dto.toDto

                dto

            let dto id n =
                let id = id |> Id.create
                let n = [ n ] |> Name.create

                createNew id n
                |> toDto


    module Quantity = VariableUnit.Quantity
    module Concentration = VariableUnit.Concentration
    module Count = VariableUnit.Count
    module Total = VariableUnit.Total
    module Rate = VariableUnit.Rate
    module Dose = VariableUnit.Dose
    module DoseAdjust = VariableUnit.DoseAdjust
    module QuantityAdjust = VariableUnit.QuantityAdjust
    module TotalAdjust = VariableUnit.TotalAdjust
    module RateAdjust = VariableUnit.RateAdjust


    /// Create an `Orderable` with
    ///
    /// * id: the order id
    /// * nm: the name of the orderable
    /// * orb\_qty: quantity of the orderable
    /// * ord\_qty: quantity of orderable in the order
    /// * orb\_cnt: the count of orderable in the order
    /// * dos: the orderable dose
    /// * dos\_adj: the adjusted orderable dose
    let create id n shape orb_qty ord_qty ord_cnt dos_cnt dos dos_adj cc =
        {
            OrderId = id
            Name = n
            Shape = shape
            OrderableQuantity = orb_qty
            OrderQuantity = ord_qty
            OrderCount = ord_cnt
            DoseCount = dos_cnt
            Dose = dos
            DoseAdjust = dos_adj
            Components = cc
        }

    /// Create a new `Orderable` with a `Component` list
    /// `cl`, and
    /// * `Orderable`unit `un` and
    /// * component unit `cu`
    /// * time unit `tu`
    /// * adjust unit `adj`
    let createNew id n shape =
        let s =  [id |> Id.toString; n |> Name.toString; Literals.orderable]
        let un = ValueUnit.NoUnit

        let orb_qty = let s = [Literals.orderable]    |> List.append s in Quantity.quantity s un
        let ord_qty = let s = [Literals.order]        |> List.append s in Quantity.quantity s un
        let orb_cnt = let s = [Literals.order]        |> List.append s in Count.count s
        let dos_cnt = let s = [Literals.dose]         |> List.append s in Count.count s
        let dos     = let s = [Literals.dose]         |> List.append s in Dose.dose s un un un
        let dos_ajd = let s = [Literals.doseAdjust]   |> List.append s in DoseAdjust.doseAdjust s un un un un

        create id n shape orb_qty ord_qty orb_cnt dos_cnt dos dos_ajd []

    /// Apply **f** to `Orderable` `ord`
    let apply f (orb: Orderable) = orb |> f

    /// Utility function to facilitate type inference
    let get = apply id

    /// Get the name of the `Orderable`
    let getName orb = (orb |> get).Name

    /// Get the `Component`s in an `Orderable`
    let getComponents orb = (orb |> get).Components

    /// Get the `Orderable` dose
    let getDose orb = (orb |> get).Dose

    // Get the `Orderable` adjusted dose
    let getDoseAdjust orb = (orb |> get).DoseAdjust

    // Get the base `Unit` of an `Orderable`
    let getUnit orb =
        (orb |> get).OrderableQuantity
        |> Quantity.toVarUnt
        |> VariableUnit.getUnit

    /// Map an `Orderable` **orb** to
    /// `VariableUnit`s
    let toVarUnt orb =
        let ord_qty = (orb |> get).OrderQuantity |> Quantity.toVarUnt
        let orb_qty = orb.OrderableQuantity      |> Quantity.toVarUnt
        let ord_cnt = orb.OrderCount             |> Count.toVarUnt
        let dos_cnt = orb.DoseCount              |> Count.toVarUnt

        let dos_qty,     dos_tot,     dos_rte     = orb.Dose       |> Dose.toVarUnt
        let dos_qty_adj, dos_tot_adj, dos_rte_adj = orb.DoseAdjust |> DoseAdjust.toVarUnt

        (
            ord_qty,
            orb_qty,
            ord_cnt,
            dos_cnt,
            dos_qty,
            dos_tot,
            dos_rte,
            dos_qty_adj,
            dos_tot_adj,
            dos_rte_adj
        )


    /// Turn an `Orderable` `ord` into
    /// a list of strings.
    let toString orb =
        let ord_qty,
            orb_qty,
            ord_cnt,
            dos_cnt,
            dos_qty,
            dos_tot,
            dos_rte,
            dos_qty_adj,
            dos_tot_adj,
            dos_rte_adj = orb |> toVarUnt

        let cc = orb.Components

        [
            ord_qty
            orb_qty
            ord_cnt
            dos_cnt
            dos_qty
            dos_tot
            dos_rte
            dos_qty_adj
            dos_tot_adj
            dos_rte_adj
        ]
        |> List.map (VariableUnit.toString false)
        |> List.append (cc |> List.collect Component.toString )
        |> List.sort

    /// The following variables are used:
    ///
    /// * ord\_qty: the quantity of orderable in an order
    /// * orb\_qty: the quantity of orderable
    /// * ord\_cnt: the count of orderable in an order
    /// * dos\_qty: the dose quantity of orderable
    /// * dos\_tot: the dose total of orderable
    /// * dos\_rte: the dose rate of orderable
    /// * dos\_qty_adj: the adjustedn dose quantity
    /// * dos\_tot_adj: the adjusted dose total
    /// * dos\_rte_adj: the adjusted dose rate
    /// * frq: frequency
    /// * tme: time
    ///
    /// The following equations are generated:
    ///
    /// *Process or Continuous*
    ///
    /// * ord\_qty = ord\_cnt \* orb\_qty
    ///
    /// *Discontinuous Timed*
    ///
    /// * dos\_tot = dos\_qty \* frq
    /// * dos\_qty = dos\_rte \* tme
    /// * dos\_tot\_adj = dos\_qty\_adj \* frq
    /// * dos\_qty\_adj = dos\_rte\_adj \* tme
    /// * dos\_tot = dos\_tot\_adj \* adj
    /// * dos\_qty = dos\_qty\_adj \* adj
    /// * dos\_rte = dos\_rte\_adj \* adj
    ///
    /// *Discontinuous*
    ///
    /// * dos\_tot = dos\_qty \* frq
    /// * dos\_tot\_adj = dos\_qty\_adj \* frq
    /// * dos\_tot = dos\_tot\_adj \* adj
    /// * dos\_qty = dos\_qty\_adj \* adj
    let toEqs hasRte adj frq tme orb =
        let ord_qty,
            orb_qty,
            ord_cnt,
            dos_cnt,
            dos_qty,
            dos_tot,
            dos_rte,
            dos_qty_adj,
            dos_tot_adj,
            dos_rte_adj = orb |> toVarUnt

        let rte = if hasRte then dos_rte |> Some else None
        let rte_adj = if hasRte then Some dos_rte_adj else None

        let qty_adj = dos_qty_adj
        let tot_adj = dos_tot_adj

        let map = Component.toEqs adj frq tme dos_qty dos_tot rte qty_adj tot_adj rte_adj orb_qty
        let cc = orb.Components

        let eqs =
            [
                [ ord_qty; ord_cnt; orb_qty ]
            ]

        let sum =
            match rte, frq, tme with
            // Discontinuous timed
            | Some _, Some _, Some _ ->
                [
                    dos_qty::(cc |> List.map (fun c -> c.Dose |> Dose.getQuantity |> Quantity.toVarUnt))
                    dos_tot::(cc |> List.map (fun c -> c.Dose |> Dose.getTotal |> Total.toVarUnt))
                    dos_rte::(cc |> List.map (fun c -> c.Dose |> Dose.getRate |> Rate.toVarUnt))
                    //dos_qty_adj::(cc |> List.map (fun c -> c.DoseAdjust |> DoseAdjust.getQuantity |> QA.toVarUnt))
                    //dos_tot_adj::(cc |> List.map (fun c -> c.DoseAdjust |> DoseAdjust.getTotal    |> TA.toVarUnt))
                    //dos_rte_adj::(cc |> List.map (fun c -> c.DoseAdjust |> DoseAdjust.getRate     |> RA.toVarUnt))
                ]
            // Discontinuous
            | None, Some _, None   ->
                [
                    dos_qty::(cc |> List.map (fun c -> c.Dose |> Dose.getQuantity |> Quantity.toVarUnt))
                    dos_tot::(cc |> List.map (fun c -> c.Dose |> Dose.getTotal |> Total.toVarUnt))
                    //dos_qty_adj::(cc |> List.map (fun c -> c.DoseAdjust |> DoseAdjust.getQuantity |> QA.toVarUnt))
                    //dos_tot_adj::(cc |> List.map (fun c -> c.DoseAdjust |> DoseAdjust.getTotal    |> TA.toVarUnt))
                ]
            // Continuous
            | Some _, None, None ->
                [
                    dos_rte::(cc |> List.map (fun c -> c.Dose |> Dose.getRate |> Rate.toVarUnt))
                    //dos_rte_adj::(cc |> List.map (fun c -> c.DoseAdjust |> DoseAdjust.getRate |> RA.toVarUnt))
                ]
            | _ -> []
            |> List.append [ orb_qty::(cc |> List.map (fun c -> c.OrderableQuantity |> Quantity.toVarUnt)) ]

        match rte, frq, tme with
        // Timed
        | Some _, Some frq, Some tme ->
            [
                [ dos_tot;     dos_qty;     frq ]
                [ dos_qty;     dos_rte;     tme ]
                [ dos_tot_adj; dos_qty_adj; frq ]
                [ dos_qty_adj; dos_rte_adj; tme ]
                [ dos_tot;     dos_tot_adj; adj ]
                [ dos_qty;     dos_qty_adj; adj ]
                [ dos_rte;     dos_rte_adj; adj ]
                [ orb_qty;     dos_qty;     dos_cnt ]
            ] |> List.append eqs
        // Discontinuous
        | None, Some frq, None   ->
            [
                [ dos_tot;     dos_qty;     frq ]
                [ dos_tot_adj; dos_qty_adj; frq ]
                [ dos_tot;     dos_tot_adj; adj ]
                [ dos_qty;     dos_qty_adj; adj ]
                [ orb_qty;     dos_qty;     dos_cnt ]
            ] |> List.append eqs
        // Continuous
        | Some _, None, None ->
            [
                [ dos_rte; dos_rte_adj; adj ]
            ]
        | _ -> eqs
        |> List.append (cc |> List.collect map)
        , sum


    let fromEqs eqs  (orb: Orderable) =
        let cmps =
            orb.Components
            |> List.map (Component.fromEqs eqs )
        {
            orb with
                OrderableQuantity = Quantity.fromVar eqs  orb.OrderableQuantity
                OrderQuantity = Quantity.fromVar eqs  orb.OrderQuantity
                OrderCount = Count.fromVar eqs  orb.OrderCount
                DoseCount = Count.fromVar eqs orb.DoseCount
                Dose = Dose.fromVar eqs  orb.Dose
                DoseAdjust = DoseAdjust.fromVar eqs  orb.DoseAdjust
                Components = cmps
        }


    module Dto =

        module Units = ValueUnit.Units
        module Id = WrappedString.Id
        module Name = WrappedString.Name
        module Quantity = VariableUnit.Quantity
        module Concentration = VariableUnit.Concentration
        module Dose = VariableUnit.Dose
        module DoseAdjust = VariableUnit.DoseAdjust
        module CT = VariableUnit.Count

        type Dto () =
            member val Id = "" with get, set
            member val Name = "" with get, set
            member val Shape = "" with get, set
            member val OrderableQuantity = VariableUnit.Dto.dto () with get, set
            member val OrderQuantity = VariableUnit.Dto.dto () with get, set
            member val OrderCount = VariableUnit.Dto.dto () with get, set
            member val DoseCount = VariableUnit.Dto.dto () with get, set
            member val DoseQuantity = VariableUnit.Dto.dto () with get, set
            member val DoseTotal = VariableUnit.Dto.dto () with get, set
            member val DoseRate = VariableUnit.Dto.dto () with get, set
            member val DoseQuantityAdjust = VariableUnit.Dto.dto () with get, set
            member val DoseTotalAdjust = VariableUnit.Dto.dto () with get, set
            member val DoseRateAdjust = VariableUnit.Dto.dto () with get, set
            member val Components : Component.Dto.Dto list = [] with get, set

        let fromDto (dto: Dto) =

            let id = dto.Id |> Id.create
            let n = [ dto.Name ] |> Name.create

            let orb_qty = dto.OrderableQuantity |> Quantity.fromDto
            let ord_qty = dto.OrderQuantity     |> Quantity.fromDto
            let ord_cnt = dto.OrderCount        |> Count.fromDto
            let dos_cnt = dto.DoseCount         |> Count.fromDto

            let cc =
                dto.Components
                |> List.map Component.Dto.fromDto

            let dos =
                (dto.DoseQuantity, dto.DoseTotal, dto.DoseRate)
                |> Dose.fromDto
            let dos_adj =
                (dto.DoseQuantityAdjust, dto.DoseTotalAdjust, dto.DoseRateAdjust)
                |> DoseAdjust.fromDto

            create id n dto.Shape orb_qty ord_qty ord_cnt dos_cnt dos dos_adj cc

        let toDto (orb : Orderable) =
            let dto = Dto ()

            dto.Id <- orb.OrderId |> Id.toString
            dto.Name <- orb.Name |> Name.toString
            dto.Shape <- orb.Shape

            dto.OrderableQuantity <-
                orb.OrderableQuantity
                |> Quantity.toDto
            dto.OrderQuantity <-
                orb.OrderQuantity
                |> Quantity.toDto
            dto.OrderCount <-
                orb.OrderCount
                |> Count.toDto
            dto.DoseCount <-
                orb.DoseCount
                |> Count.toDto

            let q, t, r = orb.Dose |> Dose.toDto
            dto.DoseQuantity <- q
            dto.DoseTotal <- t
            dto.DoseRate <- r

            let q, t, r = orb.DoseAdjust |> DoseAdjust.toDto
            dto.DoseQuantityAdjust <- q
            dto.DoseTotalAdjust <- t
            dto.DoseRateAdjust <- r

            dto.Components <-
                orb.Components
                |> List.map Component.Dto.toDto

            dto

        let dto id n shape =
            let id = id |> Id.create
            let n = [ n ] |> Name.create

            createNew id n shape
            |> toDto

