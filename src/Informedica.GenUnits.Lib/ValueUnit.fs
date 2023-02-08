namespace Informedica.GenUnits.Lib

open MathNet.Numerics


type Unit =
    | NoUnit
    | CombiUnit of Unit * Operator * Unit
    | General of (string * BigRational)
    | Count of CountUnit
    | Mass of MassUnit
    | Volume of VolumeUnit
    | Time of TimeUnit
    | Molar of MolarUnit
    | InterNatUnit of IUnit
    | Weight of WeightUnit
    | Height of HeightUnit
    | BSA of BSAUnit
and CountUnit =
    | Times of BigRational
and MassUnit =
    | KiloGram of BigRational
    | Gram of BigRational
    | MilliGram of BigRational
    | MicroGram of BigRational
    | NanoGram of BigRational
and VolumeUnit =
    | Liter of BigRational
    | DeciLiter of BigRational
    | MilliLiter of BigRational
    | MicroLiter of BigRational
    | Droplet of BigRational
and TimeUnit =
    | Year of BigRational
    | Month of BigRational
    | Week of BigRational
    | Day of BigRational
    | Hour of BigRational
    | Minute of BigRational
    | Second of BigRational
and MolarUnit =
    | Mol of BigRational
    | MilliMol of BigRational
and IUnit =
    | MIU of BigRational
    | IU of BigRational
and WeightUnit =
    | WeightKiloGram of BigRational
    | WeightGram of BigRational
and HeightUnit =
    | HeightMeter of BigRational
    | HeightCentiMeter of BigRational
and BSAUnit =
    | M2 of BigRational
and Operator =
    | OpTimes
    | OpPer
    | OpPlus
    | OpMinus


type ValueUnit = ValueUnit of  BigRational[] * Unit



module Group =


    type Group =
        | NoGroup
        | GeneralGroup of string
        | CountGroup
        | MassGroup
        | VolumeGroup
        | TimeGroup
        | MolarGroup
        | InterNatUnitGroup
        | WeightGroup
        | HeightGroup
        | BSAGroup
        | CombiGroup of (Group * Operator * Group)



module ValueUnit =


    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL


    /// Transforms an operator to a string
    let opToStr op =
        match op with
        | OpPer -> "/"
        | OpTimes -> "x"
        | OpPlus -> "+"
        | OpMinus -> "-"


    /// Transforms an operator to a string
    /// (*, /, +, -), throws an error if
    /// no match
    let opFromString s =
        match s with
        | _ when s = "/" -> OpPer
        | _ when s = "*" -> OpPer
        | _ when s = "+" -> OpPer
        | _ when s = "-" -> OpPer
        | _ -> failwith  <| $"Cannot parse %s{s} to operand"


    /// Apply a function f to the
    /// value of a unit
    let apply f u =
        let rec app u =
            match u with
            | NoUnit -> u
            | General (s, n) -> (s, n |> f) |> General
            | Count g ->
                match g with
                | Times n -> n |> f |> Times |> Count
            | Mass g  ->
                match g with
                | KiloGram n  -> n |> f |> KiloGram
                | Gram n      -> n |> f |> Gram
                | MilliGram n -> n |> f |> MilliGram
                | MicroGram n -> n |> f |> MicroGram
                | NanoGram n  -> n |> f |> NanoGram
                |> Mass
            | Volume g  ->
                match g with
                | Liter n      -> n |> f |> Liter
                | DeciLiter n  -> n |> f |> DeciLiter
                | MilliLiter n -> n |> f |> MilliLiter
                | MicroLiter n -> n |> f |> MicroLiter
                | Droplet n    -> n |> f |> Droplet
                |> Volume
            | Time g  ->
                match g with
                | Year n   -> n |> f |> Year
                | Month n  -> n |> f |> Month
                | Week n   -> n |> f |> Week
                | Day n    -> n |> f |> Day
                | Hour n   -> n |> f |> Hour
                | Minute n -> n |> f |> Minute
                | Second n -> n |> f |> Second
                |> Time
            | Molar g ->
                match g with
                | Mol n      -> n |> f |> Mol
                | MilliMol n -> n |> f |> MilliMol
                |> Molar
            | InterNatUnit g ->
                match g with
                | MIU n -> n |> f |> MIU
                | IU n  -> n |> f |> IU
                |> InterNatUnit
            | Weight g ->
                match g with
                | WeightKiloGram n -> n |> f |> WeightKiloGram
                | WeightGram n     -> n |> f |> WeightGram
                |> Weight
            | Height g ->
                match g with
                | HeightMeter n      -> n |> f |> HeightMeter
                | HeightCentiMeter n -> n |> f |> HeightCentiMeter
                |> Height
            | BSA g ->
                match g with
                | M2 n -> n |> f |> M2 |> BSA
            | CombiUnit (u1, op, u2) ->
                (app u1, op, app u2) |> CombiUnit

        app u


    /// Change the value of a unit
    /// the the value v
    let setUnitValue v =
        let f = fun _ -> v
        apply f


    /// Get the value of the unit
    let getUnitValue u =
        let rec app u =
            match u with
            | NoUnit -> None
            | General (_, n) -> n |> Some
            | Count g ->
                match g with
                | Times n -> n |> Some
            | Mass g  ->
                match g with
                | Gram n      -> n |> Some
                | KiloGram n  -> n |> Some
                | MilliGram n -> n |> Some
                | MicroGram n -> n |> Some
                | NanoGram n  -> n |> Some
            | Volume g  ->
                match g with
                | Liter n      -> n |> Some
                | DeciLiter n  -> n |> Some
                | MilliLiter n -> n |> Some
                | MicroLiter n -> n |> Some
                | Droplet n    -> n |> Some
            | Time g  ->
                match g with
                | Year n   -> n |> Some
                | Month n  -> n |> Some
                | Week n   -> n |> Some
                | Day n    -> n |> Some
                | Hour n   -> n |> Some
                | Minute n -> n |> Some
                | Second n -> n |> Some
            | Molar g ->
                match g with
                | Mol n      -> n |> Some
                | MilliMol n -> n |> Some
            | InterNatUnit g ->
                match g with
                | MIU n -> n |> Some
                | IU n  -> n |> Some
            | Weight g ->
                match g with
                | WeightKiloGram n -> n |> Some
                | WeightGram n     -> n |> Some
            | Height g ->
                match g with
                | HeightMeter n      -> n |> Some
                | HeightCentiMeter n -> n |> Some
            | BSA g ->
                match g with
                | M2 n -> n |> Some
            | CombiUnit _ -> None

        app u


    module Group =

        /// Transform a unit to the
        /// unit group
        let unitToGroup u =
            let rec get u =
                match u with
                    | NoUnit         -> Group.NoGroup
                    | General (n, _) -> Group.GeneralGroup n
                    | Count _        -> Group.CountGroup
                    | Mass _         -> Group.MassGroup
                    | Volume _       -> Group.VolumeGroup
                    | Time _         -> Group.TimeGroup
                    | Molar _        -> Group.MolarGroup
                    | InterNatUnit _ -> Group.InterNatUnitGroup
                    | Weight _       -> Group.WeightGroup
                    | Height _       -> Group.HeightGroup
                    | BSA _          -> Group.BSAGroup
                    | CombiUnit (ul, op, ur) ->
                        (get ul, op, get ur) |> Group.CombiGroup

            get u


        /// Check whether a group g1
        /// contains group g2, i.e.
        /// g1 |> contains g2 checks
        /// whether groupe g1 contains g2
        let contains g2 g1 =
            let rec cont g =
                match g with
                | Group.GeneralGroup _
                | Group.NoGroup
                | Group.CountGroup
                | Group.MassGroup
                | Group.VolumeGroup
                | Group.TimeGroup
                | Group.MolarGroup
                | Group.InterNatUnitGroup
                | Group.WeightGroup
                | Group.HeightGroup
                | Group.BSAGroup -> g = g2
                | Group.CombiGroup (gl, _, gr) ->
                    cont gl || cont gr

            cont g1


        /// Checks whether u1 contains
        /// the same unit groups as u2
        let eqsGroup u1 u2 =
            if u1 = u2 then true
            else
                let g1 = u1 |> unitToGroup
                let g2 = u2 |> unitToGroup

                g1 = g2


        /// Transforms a group g to a string
        let toString g =
            let rec str g s =
                match g with
                | Group.NoGroup -> ""
                | Group.GeneralGroup _ -> "General"
                | Group.CountGroup -> "Count"
                | Group.MassGroup -> "Mass"
                | Group.VolumeGroup -> "Volume"
                | Group.TimeGroup -> "Time"
                | Group.MolarGroup -> "Molar"
                | Group.InterNatUnitGroup -> "IUnit"
                | Group.WeightGroup -> "Weight"
                | Group.HeightGroup -> "Height"
                | Group.BSAGroup -> "BSA"
                | Group.CombiGroup (gl, op, gr) ->
                    let gls = str gl s
                    let grs = str gr s

                    gls + (op |> opToStr) + grs

            str g ""


        /// Get all the units that belong to a group in a list
        let getGroupUnits = function
                | Group.NoGroup -> [ NoUnit ]
                | Group.GeneralGroup n -> [ (n, 1N) |> General ]
                | Group.CountGroup -> [ 1N |> Times |> Count ]
                | Group.MassGroup ->
                    [
                        1N |> KiloGram |> Mass
                        1N |> Gram |> Mass
                        1N |> MilliGram |> Mass
                        1N |> MicroGram |> Mass
                        1N |> NanoGram |> Mass
                    ]
                | Group.VolumeGroup ->
                    [
                        1N |> Liter |> Volume
                        1N |> DeciLiter |> Volume
                        1N |> MilliLiter |> Volume
                        1N |> MicroLiter |> Volume
                    ]
                | Group.TimeGroup ->
                    [
                        1N |> Year |> Time
                        1N |> Month |> Time
                        1N |> Week |> Time
                        1N |> Day |> Time
                        1N |> Hour |> Time
                        1N |> Minute |> Time
                        1N |> Second |> Time
                    ]
                | Group.MolarGroup ->
                    [
                        1N |> Mol |> Molar
                        1N |> MilliMol |> Molar
                    ]
                | Group.InterNatUnitGroup ->
                    [
                        1N |> MIU |> InterNatUnit
                        1N |> IU |> InterNatUnit
                    ]
                | Group.WeightGroup ->
                    [
                        1N |> WeightKiloGram |> Weight
                        1N |> WeightGram |> Weight
                    ]
                | Group.HeightGroup ->
                    [
                        1N |> HeightMeter |> Height
                        1N |> HeightCentiMeter |> Height
                    ]
                | Group.BSAGroup -> [ 1N |> M2 |> BSA ]
                | Group.CombiGroup _ -> []


        /// Get all the units that belong to group
        /// or a combination of groups
        let getUnits g =
            let rec get g =
                match g with
                | Group.CombiGroup (gl, op, gr) ->
                    [
                        for ul in gl |> get do
                            for ur in gr |> get do
                                (ul, op, ur) |> CombiUnit
                    ]
                | _ -> g |> getGroupUnits

            get g


        module internal GroupItem =

            type Group = Group.Group

            type GroupItem =
                | GroupItem of Group
                | OperatorItem of Operator


            let toList g =
                let rec parse g acc =
                    match g with
                    | Group.CombiGroup (gl, op, gr) ->
                        let gll = parse gl acc
                        let grl = parse gr acc

                        gll @ [(op |> OperatorItem)] @ grl
                    | _ ->
                        (g |> GroupItem)::acc

                parse g []



    module Multipliers =

        let one = 1N
        let kilo = 1000N
        let deci = 1N / 10N
        let centi = deci / 10N
        let milli = 1N / kilo
        let micro = milli / kilo
        let nano = micro / kilo

        let second = 1N
        let minute = 60N * second
        let hour = minute * minute
        let day = 24N * hour
        let week = 7N * day
        let year = (365N + (1N / 4N)) * day
        let month = year / 12N

        let inline toBase m v  = v * m
        let inline toUnit m v  = v / m


        /// Get the multiplier of a unit
        /// (also when this is a combination of units)
        let getMultiplier u =
            let rec get u m =
                match u with
                | NoUnit -> one
                | General (_, n) -> n * one
                | Count g ->
                    match g with
                    | Times n -> n * one
                | Mass g  ->
                    match g with
                    | KiloGram n  -> n * kilo
                    | Gram n      -> n * one
                    | MilliGram n -> n * milli
                    | MicroGram n -> n * micro
                    | NanoGram n  -> n * nano
                | Volume g  ->
                    match g with
                    | Liter n      -> n * one
                    | DeciLiter n  -> n * deci
                    | MilliLiter n -> n * milli
                    | MicroLiter n -> n * micro
                    | Droplet n    -> n * (milli / 20N)
                | Time g  ->
                    match g with
                    | Year n   -> n * year
                    | Month n  -> n * month
                    | Week n   -> n * week
                    | Day n    -> n * day
                    | Hour n   -> n * hour
                    | Minute n -> n * minute
                    | Second n -> n * second
                | Molar g ->
                    match g with
                    | Mol n      -> n * one
                    | MilliMol n -> n * milli
                | InterNatUnit g ->
                    match g with
                    | MIU n -> n * kilo * kilo
                    | IU n  -> n * one
                | Weight g ->
                    match g with
                    | WeightKiloGram n -> n * kilo
                    | WeightGram n     -> n * one
                | Height g ->
                    match g with
                    | HeightMeter n      -> n * one
                    | HeightCentiMeter n -> n * centi
                | BSA g ->
                    match g with
                    | M2 n -> n * one
                | CombiUnit (u1, op, u2) ->
                    let m1 = get u1 m
                    let m2 = get u2 m

                    match op with
                    | OpTimes -> m1 * m2
                    | OpPer   -> m1 / m2
                    | OpMinus | OpPlus -> m

            get u 1N


    /// Create a ValueUnit from a value v
    /// (a bigrational array) and a unit u
    /// Makes sure there are nog duplicates.
    let create u v = (v |> Array.distinct, u) |> ValueUnit


    /// An empty ValueUnit that has no value
    /// and no unit, i.e. an empty array with
    /// NoUnit.
    let empty = create NoUnit [||]


    /// Create a a ValueUnit from a single
    /// value v and a unit u
    let createSingle u v = [|v|] |> create u


    /// Creates a ValueUnit with syntax
    /// v |> WithUnit u
    let withUnit u v =
        v
        |> create u


    /// Creates a 'single value' ValueUnit with syntax
    /// v |> singleWithUnit u
    let singleWithUnit u v = [|v|] |> withUnit u


    let withValue v u = create u v


    let withSingleValue v u = [| v |] |> create u


    let generalUnit v s = (s, v) |> General


    let generalValueUnit n v s = create (generalUnit v s) n


    let generalSingleValueUnit n v s = generalValueUnit [|n|] v s


    let get (ValueUnit (v, u)) = v, u


    let getValue (ValueUnit (v, _)) = v


    let getUnit (ValueUnit (_,u )) = u


    let getGroup = getUnit >> Group.unitToGroup


    let isCountUnit = Group.eqsGroup (1N |> Times |> Count)


    let isSingleValue = getValue >> Array.length >> ((=) 1)


    let valueToBase u v = v |> Multipliers.toBase (u |> Multipliers.getMultiplier)


    let toBaseValue (ValueUnit (v, u)) = v |> Array.map (valueToBase u)


    let valueToUnit u v = v |> Multipliers.toUnit (u |> Multipliers.getMultiplier)


    let toUnitValue (ValueUnit (v, u)) = v |> Array.map (valueToUnit u)


    let toBase vu =
        let v, u = vu |> get
        v
        |> Array.map (valueToBase u)
        |> create u


    let toUnit vu =
        let v, u = vu |> get
        v
        |> Array.map (valueToUnit u)
        |> create u


    let zero u = [|0N|] |> create u


    let one u = [|1N|] |> create u


    let count = 1N |> Times |> Count


    let eqsGroup vu1 vu2 =
        let u1 = vu1 |> getUnit
        let u2 = vu2 |> getUnit
        u1 |> Group.eqsGroup u2


    // ToDo: need to check if this is correct!!
    let createCombiUnit (u1, op, u2)  =
        if u1 = NoUnit && u2 = NoUnit then NoUnit
        else
            match op with
            | OpPer ->
                match u1, u2 with
                // this is not enough when u2 is combiunit but
                // contains u1!
                | _ when u1 |> Group.eqsGroup u2 ->
                    let v1 = (u1 |> getUnitValue)
                    let v2 = (u2 |> getUnitValue)
                    match v1, v2 with
                    | Some x1, Some x2 ->
                        count |> setUnitValue (x1 / x2)
                    | _ -> count
                | _ when u2 |> Group.eqsGroup count ->
                    let v1 = u1 |> getUnitValue
                    let v2 = u2 |> getUnitValue
                    match v1, v2 with
                    | Some x1, Some x2 ->
                        u1 |> setUnitValue (x1 / x2)
                    | _ -> u1
                | _ -> (u1, OpPer, u2) |> CombiUnit
            | OpTimes ->
                match u1, u2 with
                | _ when u1 |> Group.eqsGroup count &&
                         u2 |> Group.eqsGroup count ->
                    let v1 = u1 |> getUnitValue
                    let v2 = u2 |> getUnitValue
                    match v1, v2 with
                    | Some x1, Some x2 ->
                        u1 |> setUnitValue (x1 * x2)
                    | _ -> u1
                | _ when u1 |> Group.eqsGroup count ->
                    let v1 = u1 |> getUnitValue
                    let v2 = u2 |> getUnitValue
                    match v1, v2 with
                    | Some x1, Some x2 ->
                        u2 |> setUnitValue (x1 * x2)
                    | _ -> u2
                | _ when u2 |> Group.eqsGroup count ->
                    let v1 = u1 |> getUnitValue
                    let v2 = u2 |> getUnitValue
                    match v1, v2 with
                    | Some x1, Some x2 ->
                        u1 |> setUnitValue (x1 * x2)
                    | _ -> u1
                | _ -> (u1, OpTimes, u2) |> CombiUnit
            | OpPlus | OpMinus ->
                match u1, u2 with
                | _ when u1 |> Group.eqsGroup u2 ->
                    let v1 = u1 |> getUnitValue
                    let v2 = u2 |> getUnitValue
                    match v1, v2 with
                    | Some x1, Some x2 ->
                        u1 |> setUnitValue (x1 + x2)
                    | _ -> u1
                | _ -> (u1, op, u2) |> CombiUnit


    let per u2 u1 = (u1, OpPer, u2)     |> createCombiUnit


    let times u2 u1 = (u1, OpTimes, u2) |> createCombiUnit


    let plus u2 u1 = (u1, OpPlus, u2)   |> createCombiUnit


    let minus u2 u1 = (u1, OpMinus, u2) |> createCombiUnit


    let hasUnit u2 u1 =
        let rec find u =
            match u with
            | CombiUnit (lu, _, ru) ->
                if lu = u2 || ru = u2 then true
                else
                    find lu || (find ru)
            | _ ->
                u = u2
        find u1


    let isSimpleUnit u =
        match u with
        | CombiUnit _ -> false
        | _ -> true


    module private UnitItem =

        type UnitItem =
            | UnitItem of Unit
            | OpPlusMinItem of Operator
            | OpMultItem of Operator
            | OpDivItem of Operator


        let listToUnit ul =
            let rec toUnit ul u =
                match ul with
                | []       -> u
                | ui::rest ->
                    match u with
                    | NoUnit ->
                        match ui with
                        | UnitItem u'    -> u'
                        | _-> NoUnit
                        |> toUnit rest
                    | _ ->
                        match ul with
                        | oi::ui::rest ->
                            match oi, ui with
                            | OpDivItem op,     UnitItem ur
                            | OpPlusMinItem op, UnitItem ur
                            | OpMultItem op,    UnitItem ur ->
                                createCombiUnit (u, op, ur)
                                |> toUnit rest
                            | _ -> u
                        | _ -> u

            toUnit ul NoUnit



    let rec getUnits u =
        match u with
        | CombiUnit (ul, _, ur) ->
            ul
            |> getUnits
            |> List.append (ur |> getUnits)
        | _ -> [ u ]


    let simplify vu =
        let u = vu |> getUnit

        let simpl u =
            // separate numerators from denominators
            let rec numDenom b u =
                match u with
                | CombiUnit(ul, OpTimes, ur) ->
                    let lns, lds = ul |> numDenom b
                    let rns, rds = ur |> numDenom b
                    lns @ rns, lds @ rds

                | CombiUnit(ul, OpPer, ur) ->
                    if b then
                        let lns, lds = ul |> numDenom true
                        let rns, rds = ur |> numDenom false
                        lns @ rns, lds @ rds
                    else
                        let lns, lds = ur |> numDenom true
                        let rns, rds = ul |> numDenom false
                        lns @ rns, lds @ rds
                | _ -> if b then (u |> getUnits, []) else ([], u |> getUnits)
            // build a unit from a list of numerators and denominators
            let rec build ns ds (b, u) =
                match ns with
                | [] ->
                    match ds with
                    | [] -> (b, u)
                    | _ ->
                        // TODO Was the List.rev needed here (times is commutative?)
                        let d = ds |> List.reduce times
                        if u = NoUnit then
                            Count(Times 1N) |> per d
                        else u |> per d
                        |> fun u -> (b, u)
                | h::tail ->
                    if ds |> List.exists (Group.eqsGroup h) then
                        build tail (ds |> List.removeFirst (Group.eqsGroup h)) (true, u)
                    else
                        let b = b || ((u |> Group.eqsGroup count) || (h |> Group.eqsGroup count))
                        if u = NoUnit then h
                        else u |> times h
                        |> fun u -> build tail ds (b, u)

            let ns, ds = u |> numDenom true

            (false, NoUnit)
            |> build ns ds
            |> (fun (b, u) -> if u = NoUnit then (b, count) else (b, u))

        if u = NoUnit then
            vu
        else
            u
            |> simpl
            |> (fun (b, u') ->
                vu
                |> toBaseValue
                |> create (if b then u' else u)
                |> toUnitValue
                |> create (if b then u' else u)
            )


    let calc b op vu1 vu2 =

        let (ValueUnit (_, u1)) = vu1
        let (ValueUnit (_, u2)) = vu2
        // calculate value in base
        let v =
            let vs1 = vu1 |> toBaseValue
            let vs2 = vu2 |> toBaseValue
            Array.allPairs vs1 vs2
            |> Array.map (fun (v1, v2) -> v1 |> op <| v2)
        // calculate new combi unit
        let u =
            match op with
            | BigRational.Mult    -> u1 |> times u2
            | BigRational.Div     -> u1 |> per u2
            | BigRational.Add
            | BigRational.Subtr   ->
                if u1 |> Group.eqsGroup u2 then u2
                else
                    failwith <| $"cannot add or subtract different units %A{u1} %A{u2}"
        // recreate valueunit with base value and combined unit
        v
        |> create u
        // calculate to the new combiunit
        |> toUnitValue
        // recreate again to final value unit
        |> create u
        |> fun vu -> if b then vu |> simplify else vu


    let cmp cp vu1 vu2 =
        // ToDo need better eqsGroup like mg/kg/day = (mg/kg)/day = (mg/kg*day) <> mg/(kg/day) = mg*day/kg
        //if vu1 |> eqsGroup vu2 |> not then false
        //else
        let vs1 = vu1 |> toBaseValue
        let vs2 = vu2 |> toBaseValue
        Array.allPairs vs1 vs2
        |> Array.forall (fun (v1, v2)  ->
            v1 |> cp <| v2
        )


    let applyToValue fValue vu =
        let u = vu |> getUnit
        vu
        |> getValue
        |> fValue
        |> create u


    // ToDo check logic
    let applyToValues fArr fValue vu =
        let u = vu |> getUnit
        vu
        |> getValue
        |> fArr fValue
        |> create u



    let filterValues = applyToValues Array.filter


    let mapValues = applyToValues Array.map


    let validate fValid errMsg vu =
        if vu |> getValue |> fValid then vu |> Ok
        else
            errMsg
            |> Error



    let eq = cmp (=)


    let gt = cmp (>)


    let st = cmp (<)


    let gte = cmp (>=)


    let ste = cmp (<=)


    let cmpToStr cp =
        let z = 1N |> Times |> Count |> zero
        let o = 1N |> Times |> Count |> one

        match cp with
        | _ when (z |> cp <| z) && not (z |> cp <| o) && not (o |> cp <| z) -> "="
        | _ when (z |> cp <| z) && (z |> cp <| o) && not (o |> cp <| z) -> "<="
        | _ when (z |> cp <| z) && not (z |> cp <| o) && (o |> cp <| z) -> ">="
        | _ when not (z |> cp <| z) && (z |> cp <| o) && not (o |> cp <| z) -> "<"
        | _ when not (z |> cp <| z) && not (z |> cp <| o) && (o |> cp <| z) -> ">"
        | _ -> "unknown comparison"


    let convertTo u vu =
        let _, u_ = vu |> get
        if u = u_ then vu
        else
            vu
            |> toBaseValue
            |> create u
            |> toUnitValue
            |> create u


    module Units =

        type Localization = English | Dutch


        type Verbal = Long | Short


        type Language =
            {
                Eng : string
                Dut : string
            }


        let getDutch (lang : Language) = lang.Dut


        let getEnglish (lang : Language) = lang.Eng


        type UnitDetails =
            {
                Unit : Unit
                Group : Group.Group
                Abbreviation : Language
                Name : Language
                Synonyms : string list
            }


        let apply f (ud : UnitDetails) = f ud


        let get = apply id


        let getUnit ud = (ud |> get).Unit


        let create un gr ab nm sy =
            {
                Unit = un
                Group = gr
                Abbreviation = ab
                Name = nm
                Synonyms = sy
            }


        let createGeneral n v =
            let un = (n, v) |> General
            let ab = { Eng = n; Dut = n }
            let nm = { Eng = n; Dut = n }

            create un (Group.GeneralGroup n) ab nm []


        let getGroup ud = (ud |> get).Group


        let getName ud = (ud |> get).Name


        let getAbbreviation ud = (ud |> get).Abbreviation


        let getEnglishName = getName >> getEnglish


        let getDutchName = getName >> getDutch


        let getEnglishAbbreviation = getAbbreviation >> getEnglish


        let getDutchAbbreviation = getAbbreviation >> getDutch


        let getUnitString loc verb =
            match loc with
            | English ->
                match verb with
                | Short -> getEnglishAbbreviation
                | Long  -> getEnglishName
            | Dutch ->
                match verb with
                | Short -> getDutchAbbreviation
                | Long  -> getDutchName


        module General =

            let toGeneral = General
            let general n = (n, 1N) |> toGeneral


        module Count =

            let toCount = Count

            let nTimes n = n |> Times |> toCount

            let times = 1N |> nTimes


        module Mass =

            let toMass = Mass

            let nKiloGram n = n |> KiloGram |> toMass
            let nGram n = n |> Gram |> toMass
            let nMilliGram n = n |> MilliGram |> toMass
            let nMicroGram n = n |> MicroGram |> toMass
            let nNanoGram n = n |> NanoGram |> toMass

            let kiloGram = 1N |> nKiloGram
            let gram = 1N |> nGram
            let milliGram = 1N |> nMilliGram
            let microGram = 1N |> nMicroGram
            let nanoGram = 1N |> nNanoGram


        module Weight =

            let toWeight = Weight

            let nKiloGram n = n |> WeightKiloGram |> toWeight
            let nGram n = n |> WeightGram |> toWeight

            let kiloGram = 1N |> nKiloGram
            let gram = 1N |> nGram


        module Volume =

            let toVolume = Volume

            let nLiter n =  n |> Liter |> toVolume
            let nDeciLiter n =  n |> DeciLiter |> toVolume
            let nMilliLiter n =  n |> MilliLiter |> toVolume
            let nMicroLiter n =  n |> MicroLiter |> toVolume
            let nDroplet n = n |> Droplet |> toVolume

            let liter =  1N |> nLiter
            let deciLiter =  1N |> nDeciLiter
            let milliLiter =  1N |> nMilliLiter
            let microLiter =  1N |> nMicroLiter
            let droplet = 1N |> nDroplet


        module Time =

            let toTime = Time

            let nYear n = n |>  Year |>  toTime
            let nMonth n = n |>  Month |>  toTime
            let nWeek n = n |>  Week |>  toTime
            let nDay n = n |>  Day |>  toTime
            let nHour n = n |>  Hour |>  toTime
            let nMinute n = n |>  Minute |>  toTime
            let nSecond n = n |>  Second |>  toTime

            let year = 1N |> nYear
            let month = 1N |> nMonth
            let week = 1N |> nWeek
            let day = 1N |> nDay
            let hour = 1N |> nHour
            let minute = 1N |> nMinute
            let second = 1N |> nSecond


        module Molar =

            let toMolar  = Molar

            let nMol n = n |>  Mol |> toMolar
            let nMilliMol n = n |> MilliMol |> toMolar

            let mol = 1N |> nMol
            let milliMol = 1N |> nMilliMol


        module InterNatUnit =

            let toInterNatUnit = InterNatUnit

            let nMIU n = n |> MIU |> toInterNatUnit
            let nIU n = n |> IU |> toInterNatUnit

            let MIU = 1N |> nMIU
            let IU = 1N |> nIU


        module Height =

            let toHeight = Height

            let nMeter n = n |>  HeightMeter |> toHeight
            let nCentiMeter n = n |> HeightCentiMeter |> toHeight

            let meter = 1N |>  HeightMeter |> toHeight
            let centiMeter = 1N |> HeightCentiMeter |> toHeight


        module BSA =

            let toBSA = BSA

            let nM2 n = n |> M2 |> toBSA

            let M2 = 1N |> nM2


        let units =
            [
                { Unit = Count.times; Group = Group.NoGroup;  Abbreviation = { Eng = "x"; Dut = "x" }; Name = { Eng = "times"; Dut = "keer" }; Synonyms = [] }

                { Unit = Mass.kiloGram; Group = Group.NoGroup;  Abbreviation = { Eng = "kg"; Dut = "kg" }; Name = { Eng = "kilogram"; Dut = "kilogram" }; Synonyms = [] }
                { Unit = Mass.gram; Group = Group.NoGroup;  Abbreviation = { Eng = "g"; Dut = "g" }; Name = { Eng = "gram"; Dut = "gram" }; Synonyms = ["gr"] }
                { Unit = Mass.milliGram; Group = Group.NoGroup;  Abbreviation = { Eng = "mg"; Dut = "mg" }; Name = { Eng = "milligram"; Dut = "milligram" }; Synonyms = ["millig"; "milligr"] }
                { Unit = Mass.microGram; Group = Group.NoGroup;  Abbreviation = { Eng = "microg"; Dut = "microg" }; Name = { Eng = "microgram"; Dut = "microgram" }; Synonyms = ["mcg"; "µg"; "mcgr"] }
                { Unit = Mass.nanoGram; Group = Group.NoGroup;  Abbreviation = { Eng = "nanog"; Dut = "nanog" }; Name = { Eng = "nanogram"; Dut = "nanogram" }; Synonyms = ["nanogr"; "ng"] }

                { Unit = Volume.liter; Group = Group.NoGroup;  Abbreviation = { Eng = "l"; Dut = "l" }; Name = { Eng = "liter"; Dut = "liter" }; Synonyms = ["ltr"] }
                { Unit = Volume.deciLiter; Group = Group.NoGroup;  Abbreviation = { Eng = "dl"; Dut = "dl" }; Name = { Eng = "deciliter"; Dut = "deciliter" }; Synonyms = ["decil"] }
                { Unit = Volume.milliLiter; Group = Group.NoGroup;  Abbreviation = { Eng = "ml"; Dut = "ml" }; Name = { Eng = "milliliter"; Dut = "milliliter" }; Synonyms = ["millil"] }
                { Unit = Volume.microLiter; Group = Group.NoGroup;  Abbreviation = { Eng = "microl"; Dut = "microl" }; Name = { Eng = "microliter"; Dut = "microliter" }; Synonyms = ["µl"] }
                { Unit = Volume.droplet; Group = Group.NoGroup;  Abbreviation = { Eng = "dr"; Dut = "dr" }; Name = { Eng = "droplet"; Dut = "druppel" }; Synonyms = [ "drop" ] }

                { Unit = Time.year; Group = Group.NoGroup;  Abbreviation = { Eng = "yr"; Dut = "jr" }; Name = { Eng = "year"; Dut = "jaar" }; Synonyms = ["years"; "jaren"] }
                { Unit = Time.month; Group = Group.NoGroup;  Abbreviation = { Eng = "mo"; Dut = "mnd" }; Name = { Eng = "month"; Dut = "maand" }; Synonyms = ["months"; "maanden"] }
                { Unit = Time.week; Group = Group.NoGroup;  Abbreviation = { Eng = "wk"; Dut = "wk" }; Name = { Eng = "week"; Dut = "week" }; Synonyms = ["weeks"; "weken"] }
                { Unit = Time.day; Group = Group.NoGroup;  Abbreviation = { Eng = "day"; Dut = "dag" }; Name = { Eng = "day"; Dut = "dag" }; Synonyms = ["days"; "dagen"] }
                { Unit = Time.hour; Group = Group.NoGroup;  Abbreviation = { Eng = "hr"; Dut = "uur" }; Name = { Eng = "hour"; Dut = "uur" }; Synonyms = ["hours"; "uren"] }
                { Unit = Time.minute; Group = Group.NoGroup;  Abbreviation = { Eng = "min"; Dut = "min" }; Name = { Eng = "minute"; Dut = "minuut" }; Synonyms = ["minutes"; "minuten"] }
                { Unit = Time.second; Group = Group.NoGroup;  Abbreviation = { Eng = "sec"; Dut = "sec" }; Name = { Eng = "second"; Dut = "seconde" }; Synonyms = ["s"] }

                { Unit = Molar.mol; Group = Group.NoGroup;  Abbreviation = { Eng = "mol"; Dut = "mol" }; Name = { Eng = "mol"; Dut = "mol" }; Synonyms = [] }
                { Unit = Molar.milliMol; Group = Group.NoGroup;  Abbreviation = { Eng = "mmol"; Dut = "mmol" }; Name = { Eng = "millimol"; Dut = "millimol" }; Synonyms = [] }

                { Unit = InterNatUnit.IU; Group = Group.NoGroup;  Abbreviation = { Eng = "IE"; Dut = "IE" }; Name = { Eng = "IE"; Dut = "IE" }; Synonyms = [ "E"; "U"; "IU"] }
                { Unit = InterNatUnit.MIU; Group = Group.NoGroup;  Abbreviation = { Eng = "miljIE"; Dut = "miljIE" }; Name = { Eng = "millionIE"; Dut = "miljoenIE" }; Synonyms = [ "milj.IE"; "milj.E"] }

                { Unit = Weight.kiloGram; Group = Group.NoGroup;  Abbreviation = { Eng = "kg"; Dut = "kg" }; Name = { Eng = "kilogram"; Dut = "kilogram" }; Synonyms = [] }
                { Unit = Weight.gram; Group = Group.NoGroup;  Abbreviation = { Eng = "g"; Dut = "g" }; Name = { Eng = "gram"; Dut = "gram" }; Synonyms = [ "gr" ] }

                { Unit = BSA.M2; Group = Group.NoGroup;  Abbreviation = { Eng = "m2"; Dut = "m2" }; Name = { Eng = "square meter"; Dut = "vierkante meter" }; Synonyms = ["gr"] }

            ]
            |> List.map (fun ud -> { ud with Group = ud.Unit |> Group.unitToGroup })


        let mapUnit = function
        | NoUnit -> (1N, NoUnit)
        | General (n, v) -> (v, ((n, 1N) |> General))
        | Count g ->
            match g with
            | Times n -> (n, Count.times)
        | Mass g  ->
            match g with
            | KiloGram n  -> (n, Mass.kiloGram)
            | Gram n      -> (n, Mass.gram)
            | MilliGram n -> (n, Mass.milliGram)
            | MicroGram n -> (n, Mass.microGram)
            | NanoGram n  -> (n, Mass.nanoGram)
        | Volume g  ->
            match g with
            | Liter n      -> (n, Volume.liter)
            | DeciLiter n  -> (n, Volume.deciLiter)
            | MilliLiter n -> (n, Volume.milliLiter)
            | MicroLiter n -> (n, Volume.microLiter)
            | Droplet n    -> (n, Volume.droplet)
        | Time g  ->
            match g with
            | Year n   -> (n, Time.year)
            | Month n  -> (n, Time.month)
            | Week n   -> (n, Time.week)
            | Day n    -> (n, Time.day)
            | Hour n   -> (n, Time.hour)
            | Minute n -> (n, Time.minute)
            | Second n -> (n, Time.second)
        | Molar g ->
            match g with
            | Mol n      -> (n, Molar.mol)
            | MilliMol n -> (n, Molar.milliMol)
        | InterNatUnit g ->
            match g with
            | MIU n -> (n, InterNatUnit.MIU)
            | IU n  -> (n, InterNatUnit.IU)
        | Weight g ->
            match g with
            | WeightKiloGram n -> (n, Weight.kiloGram)
            | WeightGram n     -> (n, Weight.gram)
        | Height g ->
            match g with
            | HeightMeter n      -> (n, Height.meter)
            | HeightCentiMeter n -> (n, Height.centiMeter)
        | BSA g ->
            match g with
            | M2 n -> (n, BSA.M2)
        | CombiUnit (u1, op, u2) ->
            failwith <| $"Cannot map combined unit %A{(u1, op, u2) |> CombiUnit}"


        let tryFind u =
            match units |> List.tryFind (fun udt -> udt.Unit = u) with
            | Some udt -> Some udt
            | None     -> None


        let fromString s =
            match s |> String.splitAt '[' with
            | [|us;gs|] ->
                let gs = gs |> String.replace "]" ""

                let eqsUnit (udt: UnitDetails) =
                    udt.Abbreviation.Dut |> String.equalsCapInsens us ||
                    udt.Abbreviation.Eng |> String.equalsCapInsens us ||
                    udt.Name.Dut |> String.equalsCapInsens us ||
                    udt.Name.Eng |> String.equalsCapInsens us ||
                    udt.Synonyms |> List.exists (String.equalsCapInsens us)

                let eqsGroup (udt: UnitDetails) =
                    udt.Group |> Group.toString |> String.equalsCapInsens gs

                match units |> List.tryFind (fun udt -> udt |> eqsUnit && udt |> eqsGroup) with
                | Some udt -> udt.Unit
                | None     -> generalUnit 1N s
                |> Some

            | _ -> None



        let toString loc verb u =
            let gtost u g = u + "[" + (g |> Group.toString) + "]"

            let rec str u =
                match u with
                | NoUnit -> ""

                | CombiUnit (ul, op, ur) ->
                    let uls = str ul
                    let urs = str ur

                    uls + (op |> opToStr) + urs

                | General (n, v) ->
                    let ustr = n // + "[General]"
                    if v > 1N then
                        (1N |> BigRational.toString) + ustr
                    else ustr

                | _ ->
                    let v, u = u |> mapUnit
                    match u |> tryFind with
                    | Some udt ->
                        match loc with
                        | English ->
                            match verb with
                            | Short -> udt.Group |> gtost udt.Abbreviation.Eng
                            | Long  -> udt.Group |> gtost udt.Name.Eng
                        | Dutch ->
                            match verb with
                            | Short -> udt.Group |> gtost udt.Abbreviation.Dut
                            | Long  -> udt.Group |> gtost udt.Name.Dut
                    | None -> ""
                    |> (fun s ->
                        if s = "" then ""
                        else
                            if v = 1N then s
                            else
                                (v |> BigRational.toString) + " " + s
                    )

            str u


        let toStringDutchShort = toString Dutch Short
        let toStringDutchLong  = toString Dutch Long
        let toStringEngShort   = toString English Short
        let toStringEngLong    = toString English Long


    /// Get the user readable string version
    /// of a unit, i.e. without unit group between
    /// brackets
    let unitToReadableString u =
        u
        |> Units.toString Units.Dutch Units.Short
        |> String.removeBrackets


    let toString brf loc verb vu =
        let v, u = vu |> get

        $"{v |> Array.map brf |> Array.toReadableString} {Units.toString loc verb u}"


    let toStringDutchShort = toString BigRational.toString Units.Dutch Units.Short
    let toStringDutchLong  = toString BigRational.toString Units.Dutch Units.Long
    let toStringEngShort   = toString BigRational.toString Units.English Units.Short
    let toStringEngLong    = toString BigRational.toString Units.English Units.Long

    let toStringFloatDutchShort = toString (BigRational.toDecimal >> string) Units.Dutch Units.Short
    let toStringFloatDutchLong  = toString (BigRational.toDecimal >> string) Units.Dutch Units.Long
    let toStringFloatEngShort   = toString (BigRational.toDecimal >> string) Units.English Units.Short
    let toStringFloatEngLong    = toString (BigRational.toDecimal >> string) Units.English Units.Long


    /// Turn a `ValueUnit` `vu` into
    /// a string using precision `prec`.
    let toStringPrec prec vu =
        let v, u = vu |> get

        let vs =
            v
            |> Array.map BigRational.toDecimal
            |> Array.map (Decimal.fixPrecision prec)
            |> Array.toReadableString

        let us =
            u
            |> unitToReadableString

        vs + " " + us


    let fromString s =

        let fs s =
            let dels = "#"

            let ufs s =
                // ToDo doesn't work with units with spaces
                match s |> String.trim |> String.split " " with
                | [ug] ->
                    match Units.fromString ug with
                    | Some u' -> u' |> setUnitValue 1N
                    | None      -> failwith <| $"Not a valid unit: %s{ug}"

                | [v;ug] ->
                    match v |> BigRational.tryParse with
                    | None ->
                        failwith <| $"Cannot parse string: %s{s} with value: %s{v}"
                    | Some v' ->
                        match Units.fromString ug with
                        | Some u' -> u' |> setUnitValue v'
                        | None     -> failwith <| $"Not a valid unit: %s{ug}"
                | _ -> failwith <| $"Cannot parse string %s{s}"

                |> UnitItem.UnitItem

            let rec parse ul usl =

                match usl with
                | []   -> ul
                | [us] ->
                    ul @ [us |> ufs]

                | us::os::rest ->
                    let ui = us |> ufs
                    let oi =
                        let o = os |> opFromString
                        match o with
                        | OpPer   -> o |> UnitItem.OpDivItem
                        | OpTimes -> o |> UnitItem.OpMultItem
                        | OpPlus | OpMinus -> o |> UnitItem.OpPlusMinItem

                    rest |> parse (ul @ [ui;oi])

            s
            |> String.replace "*" (dels + "*" + dels)
            |> String.replace "/" (dels + "/" + dels)
            |> String.replace "+" (dels + "+" + dels)
            |> String.replace "-" (dels + "-" + dels)
            |> String.split dels
            |> parse []
            |> UnitItem.listToUnit

        match s |> String.split " " with
        | vs::rest ->
            match vs |> BigRational.tryParse with
            | None ->
                failwith <| $"Cannot parse string %s{s}"
            | Some v ->
                let u =
                    rest
                    |> String.concat " "
                    |> String.trim
                    |> fs
                ([|v|], u) |> ValueUnit
        | _ ->
            if s = "" then failwith "Cannot parse empty string"
            else failwith <| $"Cannot parse string %s{s}"


    module Operators =

        let (*) vu1 vu2 = calc true (*) vu1 vu2

        let (/) vu1 vu2 = calc true (/) vu1 vu2

        let (+) vu1 vu2 = calc true (+) vu1 vu2

        let (-) vu1 vu2 = calc true (-) vu1 vu2

        let (=?) vu1 vu2 = cmp (=) vu1 vu2

        let (>?) vu1 vu2 = cmp (>) vu1 vu2

        let (<?) vu1 vu2 = cmp (<) vu1 vu2

        let (>=?) vu1 vu2 = cmp (>=) vu1 vu2

        let (<=?) vu1 vu2 = cmp (<=) vu1 vu2

        let (==>) vu u = vu |> convertTo u



    module Dto =


        [<Literal>]
        let english = "english"

        [<Literal>]
        let dutch = "dutch"

        type Dto () =
            member val Value = [||] with get, set
            member val Unit = "" with get, set
            member val Group = "" with get, set
            member val Short = true with get, set
            member val Language = "" with get, set

        let dto () = Dto ()

        let toString (dto : Dto) =
            $"%A{dto.Value} %s{dto.Unit}"

        let toDto short lang vu =
            let isLang s l =
                l
                |> String.trim
                |> String.toLower
                |> (fun l -> s |> String.startsWith l)
            let l =
                match lang with
                | _ when lang |> isLang english ->
                    Units.English |> Some
                | _ when lang |> isLang dutch ->
                    Units.Dutch |> Some
                | _ -> None

            match l with
            | None -> None
            | Some l ->
                let s =
                    if short then Units.Short
                    else Units.Long

                let v, u = vu |> get
                let v = v |> Array.map BigRational.toDecimal
                let g =
                    u
                    |> Group.unitToGroup
                    |> Group.toString
                let u =
                    u
                    |> Units.toString l s
                    |> String.removeBrackets

                let dto = dto ()
                dto.Value <- v
                dto.Unit <- u
                dto.Group <- g
                dto.Language <- lang
                dto.Short <- short

                dto |> Some

        let toDtoDutchShort vu  =  vu |>toDto true dutch    |> Option.get
        let toDtoDutchLong vu    =  vu |>toDto false dutch   |> Option.get
        let toDtoEnglishShort vu =  vu |>toDto true english  |> Option.get
        let toDtoEnglishLong vu  =  vu |>toDto false english |> Option.get

        let fromDto (dto: Dto) =
            let v = dto.Value |> Array.map BigRational.fromDecimal
            $"%s{dto.Unit}[%s{dto.Group}]"
            |> Units.fromString
            |> function
            | Some u ->
                v
                |> create u
                |> Some
            | _ -> None


type ValueUnit with

    static member (*) (vu1, vu2) = ValueUnit.calc true (*) vu1 vu2

    static member (/) (vu1, vu2) = ValueUnit.calc true (/) vu1 vu2

    static member (+) (vu1, vu2) = ValueUnit.calc true (+) vu1 vu2

    static member (-) (vu1, vu2) = ValueUnit.calc true (-) vu1 vu2

    static member (=?) (vu1, vu2) = ValueUnit.cmp (=) vu1 vu2

    static member (>?) (vu1, vu2) = ValueUnit.cmp (>) vu1 vu2

    static member (<?) (vu1, vu2) = ValueUnit.cmp (<) vu1 vu2

    static member (>=?) (vu1, vu2) = ValueUnit.cmp (>=) vu1 vu2

    static member (<=?) (vu1, vu2) = ValueUnit.cmp (<=) vu1 vu2

    static member (==>) (vu, u) = vu |> ValueUnit.convertTo u




