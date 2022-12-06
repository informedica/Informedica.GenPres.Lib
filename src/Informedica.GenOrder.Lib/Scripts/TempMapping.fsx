

/// Types and functions that deal with an order.
/// An `Order` models the `Prescription` of an
/// `Orderable` with a `StartStop` start date and
/// stop date.
module Order =

    open Informedica.Utils.Lib.BCL

    /// Utitlity functions to
    /// enable mapping of a `Variable`s
    /// to an `Order`
    module Mapping =

        let [<Literal>] qty = "qty"
        let [<Literal>] cnc = "cnc"
        let [<Literal>] ptm = "ptm"
        let [<Literal>] rte = "rte"
        let [<Literal>] tot = "tot"
        let [<Literal>] qtyAdj = "qty_adj"
        let [<Literal>] ptmAdj = "ptm_adj"
        let [<Literal>] rteAdj = "rte_adj"
        let [<Literal>] totAdj = "tot_adj"
        let [<Literal>] cnt = "cnt"
        let [<Literal>] frq = "frq"
        let [<Literal>] tme = "tme"
        let [<Literal>] itm = "itm" //Orderable.Literals.item
        let [<Literal>] cmp = "cmp" //Orderable.Literals.comp
        let [<Literal>] orb = "orb" //Orderable.Literals.orderable
        let [<Literal>] dos = "dos" //Orderable.Literals.dose
        let [<Literal>] prs = "prs" //"Prescription"
        let [<Literal>] ord = "ord" // "Order"
        let [<Literal>] adj = "adj" // "Adjust"


        let map id nOrb nCmp nItm m =
            match m with
            | ItemComponentQuantity -> [ itm; cmp; qty; ]
            | ItemComponentConcentration -> [ itm; cmp; cnc; ]
            | ItemOrderableQuantity -> [ itm; orb; qty; ]
            | ItemOrderableConcentration -> [ itm; orb; cnc; ]
            | ItemOrderQuantity -> [ itm; ord; qty ]
            | ItemDoseQuantity -> [ itm; dos; qty; ]
            | ItemDosePerTime -> [ itm; dos; ptm; ]
            | ItemDoseRate -> [ itm; dos; rte; ]
            | ItemDoseTotal -> [ itm; dos; tot ]
            | ItemDoseQuantityAdjust -> [ itm; dos; qtyAdj; ]
            | ItemDosePerTimeAdjust -> [ itm; dos; ptmAdj; ]
            | ItemDoseRateAdjust -> [ itm; dos; rteAdj; ]
            | ItemDoseTotalAdjust -> [itm; dos; qtyAdj ]
            | ComponentComponentQuantity -> [ cmp; cmp; qty; ]
            | ComponentOrderableQuantity -> [ cmp; orb; qty; ]
            | ComponentOrderableConcentration -> [ cmp; orb; cnc; ]
            | ComponentOrderableCount -> [ cmp; orb; cnt; ]
            | ComponentOrderQuantity -> [ cmp; ord; qty]
            | ComponentOrderCount -> [ cmp; ord; cnt; ]
            | ComponentDoseQuantity -> [ cmp; dos; qty; ]
            | ComponentDosePerTime -> [ cmp; dos; ptm; ]
            | ComponentDoseRate -> [ cmp; dos; rte; ]
            | ComponentDoseTotal -> [ cmp; dos; tot ]
            | ComponentDoseQuantityAdjust -> [ cmp; dos; qtyAdj; ]
            | ComponentDosePerTimeAdjust -> [ cmp; dos; ptmAdj; ]
            | ComponentDoseRateAdjust -> [ cmp; dos; rteAdj; ]
            | ComponentDoseTotalAdjust -> [ cmp; dos; totAdj ]
            | OrderableOrderableQuantity -> [ orb; orb; qty; ]
            | OrderableDoseCount -> [ orb; dos; cnt; ]
            | OrderableDoseQuantity -> [ orb; dos; qty; ]
            | OrderableDosePerTime -> [ orb; dos; ptm; ]
            | OrderableDoseRate -> [ orb; dos; rte; ]
            | OrderableDoseTotal -> [ orb; dos; tot ]
            | OrderableDoseQuantityAdjust -> [ orb; dos; qtyAdj; ]
            | OrderableDosePerTimeAdjust -> [ orb; dos; ptmAdj; ]
            | OrderableDoseRateAdjust -> [ orb; dos; rteAdj; ]
            | OrderableDoseTotalAdjust -> [ orb; dos; totAdj ]
            | OrderableOrderQuantity -> [ orb; ord; qty; ]
            | OrderableOrderCount -> [ orb; ord; cnt; ]
            | OrderPrescriptionFrequency -> [ ord; prs; frq; ]
            | OrderPrescriptionTime -> [ ord; prs; tme; ]
            | OrderAdjustQuantity -> [ ord; adj; qty; ]
            | OrderOrderTime -> [ord; ord; tme ]
            |> fun xs ->
                xs[1..]
                |> List.append [ $"<{xs[0]}>" ]
            |> String.concat "."
            |> String.replace "<ord>" $"{id}"
            |> String.replace "<orb>" $"{id}_{nOrb}"
            |> String.replace "<cmp>" $"{id}_{nOrb}_{nCmp}"
            |> String.replace "<itm>" $"{id}_{nOrb}_{nCmp}_{nItm}"

        (*
        let fromString s =
            match s with
            | _ when s = (ItemComponentQuantity |> map) -> ItemComponentQuantity
            | _ when s = (ItemComponentConcentration |> map) -> ItemComponentConcentration
            | _ when s = (ItemOrderableQuantity |> map) -> ItemOrderableQuantity
            | _ when s = (ItemOrderableConcentration |> map) -> ItemOrderableConcentration
            | _ when s = (ItemDoseQuantity |> map) -> ItemDoseQuantity
            | _ when s = (ItemDosePerTime |> map) -> ItemDosePerTime
            | _ when s = (ItemDoseRate |> map) -> ItemDoseRate
            | _ when s = (ItemDoseQuantityAdjust |> map) -> ItemDoseQuantityAdjust
            | _ when s = (ItemDosePerTimeAdjust |> map) -> ItemDosePerTimeAdjust
            | _ when s = (ItemDoseRateAdjust |> map) -> ItemDoseRateAdjust
            | _ when s = (ComponentComponentQuantity |> map) -> ComponentComponentQuantity
            | _ when s = (ComponentOrderableQuantity |> map) -> ComponentOrderableQuantity
            | _ when s = (ComponentOrderableConcentration |> map) -> ComponentOrderableConcentration
            | _ when s = (ComponentOrderableCount |> map) -> ComponentOrderableCount
            | _ when s = (ComponentDoseQuantity |> map) ->  ComponentDoseQuantity
            | _ when s = (ComponentDosePerTime |> map) ->  ComponentDosePerTime
            | _ when s = (ComponentDoseRate |> map) -> ComponentDoseRate
            | _ when s = (ComponentDoseQuantityAdjust |> map) -> ComponentDoseQuantityAdjust
            | _ when s = (ComponentDosePerTimeAdjust |> map) -> ComponentDosePerTimeAdjust
            | _ when s = (ComponentDoseRateAdjust |> map) -> ComponentDoseRateAdjust
            | _ when s = (ComponentOrderCount  |> map) ->  ComponentOrderCount
            | _ when s = (OrderableOrderableQuantity |> map) -> OrderableOrderableQuantity
            | _ when s = (OrderableDoseCount |> map) -> OrderableDoseCount
            | _ when s = (OrderableDoseQuantity  |> map) -> OrderableDoseQuantity
            | _ when s = (OrderableDosePerTime |> map) -> OrderableDosePerTime
            | _ when s = (OrderableDoseRate |> map) -> OrderableDoseRate
            | _ when s = (OrderableDoseQuantityAdjust |> map) -> OrderableDoseQuantityAdjust
            | _ when s = (OrderableDosePerTimeAdjust |> map) -> OrderableDosePerTimeAdjust
            | _ when s = (OrderableDoseRateAdjust |> map) -> OrderableDoseRateAdjust
            | _ when s = (OrderableOrderQuantity |> map) -> OrderableOrderQuantity
            | _ when s = (OrderableOrderCount |> map) -> OrderableOrderCount
            | _ when s = (OrderPrescriptionFrequency |> map) -> OrderPrescriptionFrequency
            | _ when s = (OrderPrescriptionTime |> map) -> OrderPrescriptionTime
            | _ when s = (OrderAdjustQuantity |> map) -> OrderAdjustQuantity
            | _ -> $"cannot map {s} to an OrderMapping" |> failwith
        *)

        let equationMappings =
            [

                ProductMapping [ ItemComponentQuantity;ItemComponentConcentration;ComponentComponentQuantity ]
                ProductMapping [ ItemOrderableQuantity;ItemOrderableConcentration;OrderableOrderableQuantity ]
                ProductMapping [ ItemOrderableQuantity;ItemComponentConcentration;ComponentOrderableQuantity ]
                ProductMapping [ ItemDoseQuantity;ItemComponentConcentration;ComponentDoseQuantity ]
                ProductMapping [ ItemDoseQuantity;ItemOrderableConcentration;OrderableDoseQuantity ]
                ProductMapping [ ItemDoseQuantity;ItemDoseRate;OrderPrescriptionTime ]
                ProductMapping [ ItemDoseQuantity;ItemDoseQuantityAdjust;OrderAdjustQuantity ]
                ProductMapping [ ItemDosePerTime;ItemComponentConcentration;ComponentDosePerTime ]
                ProductMapping [ ItemDosePerTime;ItemOrderableConcentration;OrderableDosePerTime ]
                ProductMapping [ ItemDosePerTime;ItemDoseQuantity;OrderPrescriptionFrequency ]
                ProductMapping [ ItemDosePerTime;ItemDosePerTimeAdjust;OrderAdjustQuantity ]
                ProductMapping [ ItemDoseRate;ItemComponentConcentration;ComponentDoseRate ]
                ProductMapping [ ItemDoseRate;ItemOrderableConcentration;OrderableDoseRate ]
                ProductMapping [ ItemDoseRate;ItemDoseRateAdjust;OrderAdjustQuantity ]
                ProductMapping [ ItemOrderQuantity;ItemDosePerTime;OrderOrderTime ]
                ProductMapping [ ItemOrderQuantity;ItemDoseRate;OrderOrderTime ]
                ProductMapping [ ItemDoseQuantityAdjust;ItemComponentConcentration;ComponentDoseQuantityAdjust ]
                ProductMapping [ ItemDoseQuantityAdjust;ItemOrderableConcentration;OrderableDoseQuantityAdjust ]
                ProductMapping [ ItemDoseQuantityAdjust;ItemDoseRateAdjust;OrderPrescriptionTime ]
                ProductMapping [ ItemDosePerTimeAdjust;ItemComponentConcentration;ComponentDosePerTimeAdjust ]
                ProductMapping [ ItemDosePerTimeAdjust;ItemOrderableConcentration;OrderableDosePerTimeAdjust ]
                ProductMapping [ ItemDosePerTimeAdjust;ItemDoseQuantityAdjust;OrderPrescriptionFrequency ]
                ProductMapping [ ItemDoseRateAdjust;ItemComponentConcentration;ComponentDoseRateAdjust ]
                ProductMapping [ ItemDoseRateAdjust;ItemOrderableConcentration;OrderableDoseRateAdjust ]
                ProductMapping [ ComponentOrderableQuantity;ComponentOrderableConcentration;OrderableOrderableQuantity ]
                ProductMapping [ ComponentOrderableQuantity;ComponentComponentQuantity;ComponentOrderableCount ]
                ProductMapping [ ComponentOrderQuantity;ComponentComponentQuantity;ComponentOrderCount ]
                ProductMapping [ ComponentOrderQuantity;ComponentDosePerTime;OrderOrderTime ]
                ProductMapping [ ComponentOrderQuantity;ComponentDoseRate;OrderOrderTime ]
                ProductMapping [ ComponentDoseQuantity;ComponentOrderableConcentration;OrderableDoseQuantity ]
                ProductMapping [ ComponentDoseQuantity;ComponentDoseRate;OrderPrescriptionTime ]
                ProductMapping [ ComponentDoseQuantity;ComponentDoseQuantityAdjust;OrderAdjustQuantity ]
                ProductMapping [ ComponentDosePerTime;ComponentOrderableConcentration;OrderableDosePerTime ]
                ProductMapping [ ComponentDosePerTime;ComponentDoseQuantity;OrderPrescriptionFrequency ]
                ProductMapping [ ComponentDosePerTime;ComponentDosePerTimeAdjust;OrderAdjustQuantity ]
                ProductMapping [ ComponentDoseRate;ComponentOrderableConcentration;OrderableDoseRate ]
                ProductMapping [ ComponentDoseRate;ComponentDoseRateAdjust;OrderAdjustQuantity ]
                ProductMapping [ ComponentDoseQuantityAdjust;ComponentOrderableConcentration;OrderableDoseQuantityAdjust ]
                ProductMapping [ ComponentDoseQuantityAdjust;ComponentDoseRateAdjust;OrderPrescriptionTime ]
                ProductMapping [ ComponentDosePerTimeAdjust;ComponentOrderableConcentration;OrderableDosePerTimeAdjust ]
                ProductMapping [ ComponentDosePerTimeAdjust;ComponentDoseQuantityAdjust;OrderPrescriptionFrequency ]
                ProductMapping [ ComponentDoseRateAdjust;ComponentOrderableConcentration;OrderableDoseRateAdjust ]
                ProductMapping [ OrderableOrderQuantity;OrderableOrderCount;OrderableOrderableQuantity ]
                ProductMapping [ OrderableOrderQuantity;OrderableDosePerTime;OrderOrderTime ]
                ProductMapping [ OrderableOrderQuantity;OrderableDoseRate;OrderOrderTime ]
                ProductMapping [ OrderableDoseQuantity;OrderableDoseRate;OrderPrescriptionTime ]
                ProductMapping [ OrderableDoseQuantity;OrderableDoseQuantityAdjust;OrderAdjustQuantity ]
                ProductMapping [ OrderableDosePerTime;OrderableDoseQuantity;OrderPrescriptionFrequency ]
                ProductMapping [ OrderableDosePerTime;OrderableDosePerTimeAdjust;OrderAdjustQuantity ]
                ProductMapping [ OrderableDoseRate;OrderableDoseRateAdjust;OrderAdjustQuantity ]
                ProductMapping [ OrderableDoseQuantityAdjust;OrderableDoseRateAdjust;OrderPrescriptionTime ]
                ProductMapping [ OrderableDosePerTimeAdjust;OrderableDoseQuantityAdjust;OrderPrescriptionFrequency ]
                SumMapping [ OrderableOrderableQuantity;ComponentOrderableQuantity ]
                SumMapping [ OrderableDoseQuantity;ComponentDoseQuantity ]
                SumMapping [ OrderableDosePerTime;ComponentDosePerTime ]
                SumMapping [ OrderableDoseRate;ComponentDoseRate ]
            ]

