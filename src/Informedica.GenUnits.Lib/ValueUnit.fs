namespace Informedica.GenUnits.Lib


module List =

    let remove1 pred =
        List.fold (fun acc x ->
            let b, xs = acc
            if b then (true, x::(acc |> snd))
            else
                if x |> pred then (true, xs)
                else (false, x::(acc |> snd))
        ) (false, [])
        >> snd


module ValueUnit =

    open MathNet.Numerics

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL


    type Value = BigRational

    type Name = string


    type ValueUnit = ValueUnit of  Value * Unit
    and Unit =
        | NoUnit
        | CombiUnit of Unit * Operator * Unit
        | General of (Name * Value)
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
        | Times of Times
    and MassUnit =
        | KiloGram of KiloGram
        | Gram of Gram
        | MilliGram of MilliGram
        | MicroGram of MicroGram
        | NanoGram of NanoGram
    and VolumeUnit =
        | Liter of Liter
        | DeciLiter of DeciLiter
        | MilliLiter of MilliLiter
        | MicroLiter of MicroLiter
    and TimeUnit =
        | Year of Year
        | Month of Month
        | Week of Week
        | Day of Day
        | Hour of Hour
        | Minute of Minute
        | Second of Second
    and MolarUnit =
        | Mol of Mol
        | MilliMol of MilliMol
    and IUnit =
        | MIU of MIU
        | IU of IU
    and WeightUnit =
        | WeightKiloGram of KiloGram
        | WeightGram of Gram
    and HeightUnit =
        | HeightMeter of Meter
        | HeightCentiMeter of CentiMeter
    and BSAUnit =
        | M2 of M2
    and Operator =
        | OpTimes
        | OpPer
        | OpPlus
        | OpMinus
    // Count
    and Times = BigRational
    // InterNatUnit
    and IU  = BigRational
    and MIU = BigRational
    // Mass
    and KiloGram  = BigRational
    and Gram      = BigRational
    and MilliGram = BigRational
    and MicroGram = BigRational
    and NanoGram  = BigRational
    // Volume
    and Liter      = BigRational
    and DeciLiter  = BigRational
    and MilliLiter = BigRational
    and MicroLiter = BigRational
    // Time
    and Second = BigRational
    and Minute = BigRational
    and Hour   = BigRational
    and Day    = BigRational
    and Week   = BigRational
    and Month  = BigRational
    and Year   = BigRational
    // Height
    and CentiMeter = BigRational
    and Meter      = BigRational
    // Molar
    and Mol      = BigRational
    and MilliMol = BigRational
    // BSA
    and M2 = BigRational


    let opToStr op =
        match op with
        | OpPer -> "/"
        | OpTimes -> "x"
        | OpPlus -> "+"
        | OpMinus -> "-"


    let opFromString s =
        match s with
        | _ when s = "/" -> OpPer
        | _ when s = "*" -> OpPer
        | _ when s = "+" -> OpPer
        | _ when s = "-" -> OpPer
        | _ -> failwith  <| sprintf "Cannot parse %s to operand" s


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

    let setUnitValue v =
        let f = fun _ -> v
        apply f


    let getUnitValue u = id


    module Group =

        type Group =
            | NoGroup
            | GeneralGroup of Name
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


        let unitToGroup u =
            let rec get u =
                match u with
                    | NoUnit         -> NoGroup
                    | General (n, _) -> GeneralGroup n
                    | Count _        -> CountGroup
                    | Mass _         -> MassGroup
                    | Volume _       -> VolumeGroup
                    | Time _         -> TimeGroup
                    | Molar _        -> MolarGroup
                    | InterNatUnit _ -> InterNatUnitGroup
                    | Weight _       -> WeightGroup
                    | Height _       -> HeightGroup
                    | BSA _          -> BSAGroup
                    | CombiUnit (ul, op, ur) ->
                        (get ul, op, get ur) |> CombiGroup

            get u


        let contains g2 g1 =
            let rec cont g =
                match g with
                | GeneralGroup _
                | NoGroup
                | CountGroup
                | MassGroup
                | VolumeGroup
                | TimeGroup
                | MolarGroup
                | InterNatUnitGroup
                | WeightGroup
                | HeightGroup
                | BSAGroup -> g = g2
                | CombiGroup (gl, _, gr) ->
                    cont gl || cont gr

            cont g1


        let eqsGroup u1 u2 =
            if u1 = u2 then true
            else
                let g1 = u1 |> unitToGroup
                let g2 = u2 |> unitToGroup

                g1 = g2


        let toString g =
            let rec str g s =
                match g with
                | NoGroup -> ""
                | GeneralGroup _ -> "General"
                | CountGroup -> "Count"
                | MassGroup -> "Mass"
                | VolumeGroup -> "Volume"
                | TimeGroup -> "Time"
                | MolarGroup -> "Molar"
                | InterNatUnitGroup -> "Internat. Unit"
                | WeightGroup -> "Weight"
                | HeightGroup -> "Height"
                | BSAGroup -> "BSA"
                | CombiGroup (gl, op, gr) ->
                    let gls = str gl s
                    let grs = str gr s

                    gls + (op |> opToStr) + grs

            str g ""


        let getGroupUnits = function
                | NoGroup -> [ NoUnit ]
                | GeneralGroup n -> [ (n, 1N) |> General ]
                | CountGroup -> [ 1N |> Times |> Count ]
                | MassGroup ->
                    [
                        1N |> KiloGram |> Mass
                        1N |> Gram |> Mass
                        1N |> MilliGram |> Mass
                        1N |> MicroGram |> Mass
                        1N |> NanoGram |> Mass
                    ]
                | VolumeGroup ->
                    [
                        1N |> Liter |> Volume
                        1N |> DeciLiter |> Volume
                        1N |> MilliLiter |> Volume
                        1N |> MicroLiter |> Volume
                    ]
                | TimeGroup ->
                    [
                        1N |> Year |> Time
                        1N |> Month |> Time
                        1N |> Week |> Time
                        1N |> Day |> Time
                        1N |> Hour |> Time
                        1N |> Minute |> Time
                        1N |> Second |> Time
                    ]
                | MolarGroup ->
                    [
                        1N |> Mol |> Molar
                        1N |> MilliMol |> Molar
                    ]
                | InterNatUnitGroup ->
                    [
                        1N |> MIU |> InterNatUnit
                        1N |> IU |> InterNatUnit
                    ]
                | WeightGroup ->
                    [
                        1N |> WeightKiloGram |> Weight
                        1N |> WeightGram |> Weight
                    ]
                | HeightGroup ->
                    [
                        1N |> HeightMeter |> Height
                        1N |> HeightCentiMeter |> Height
                    ]
                | BSAGroup -> [ 1N |> M2 |> BSA ]
                | CombiGroup _ -> []


        let getUnits g =
            let rec get g =
                match g with
                | CombiGroup (gl, op, gr) ->
                    [
                        for ul in gl |> get do
                            for ur in gr |> get do
                                yield (ul, op, ur) |> CombiUnit
                    ]
                | _ -> g |> getGroupUnits

            get g


        module internal GroupItem =

            type GroupItem =
                | GroupItem of Group
                | OperatorItem of Operator


            let toList g =
                let rec parse g acc =
                    match g with
                    | CombiGroup (gl, op, gr) ->
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



    let create u v = (v, u) |> ValueUnit


    let generalUnit v s = (s, v) |> General


    let generalValueUnit n v s = create (generalUnit v s) n


    let get (ValueUnit (v, u)) = v, u


    let isCountUnit = Group.eqsGroup (1N |> Times |> Count)


    let toBase (ValueUnit (v, u)) = v |> Multipliers.toBase (u |> Multipliers.getMultiplier)


    let toUnit (ValueUnit (v, u)) = v |> Multipliers.toUnit (u |> Multipliers.getMultiplier)


    let count = 1N |> Times |> Count


    let createCombiUnit (u1, op, u2)  =
        if u1 = NoUnit && u2 = NoUnit then NoUnit
        else
            match op with
            | OpPer ->
                match u1, u2 with
                | _ when u1 |> Group.eqsGroup u2 ->
                    let v1 = u1 |> Multipliers.getMultiplier
                    let v2 = u2 |> Multipliers.getMultiplier
                    count |> setUnitValue (v1 / v2)
                | _ when u2 |> Group.eqsGroup count ->
                    let v1 = u1 |> Multipliers.getMultiplier
                    let v2 = u2 |> Multipliers.getMultiplier
                    u1 |> setUnitValue (v1 / v2)
                | _ -> (u1, OpPer, u2) |> CombiUnit
            | OpTimes ->
                match u1, u2 with
                | _ when u1 |> Group.eqsGroup count ->
                    let v1 = u1 |> Multipliers.getMultiplier
                    let v2 = u2 |> Multipliers.getMultiplier
                    u2 |> setUnitValue (v1 * v2)
                | _ when u2 |> Group.eqsGroup count ->
                    let v1 = u1 |> Multipliers.getMultiplier
                    let v2 = u2 |> Multipliers.getMultiplier
                    u1 |> setUnitValue (v1 * v2)
                | _ when u1 |> Group.eqsGroup count &&
                         u2 |> Group.eqsGroup count ->
                    let v1 = u1 |> Multipliers.getMultiplier
                    let v2 = u2 |> Multipliers.getMultiplier
                    u1 |> setUnitValue (v1 * v2)
                | _ -> (u1, OpTimes, u2) |> CombiUnit
            | OpPlus | OpMinus ->
                match u1, u2 with
                | _ when u1 |> Group.eqsGroup u2 ->
                    let v1 = u1 |> Multipliers.getMultiplier
                    let v2 = u2 |> Multipliers.getMultiplier
                    u1 |> setUnitValue (v1 + v2)
                | _ -> (u1, op, u2) |> CombiUnit


    let per u2 u1 = (u1, OpPer, u2) |> createCombiUnit


    let times u2 u1 = (u1, OpTimes, u2) |> createCombiUnit


    let plus u2 u1 = (u1, OpPlus, u2) |> createCombiUnit


    let minus u2 u1 = (u1, OpMinus, u2) |> createCombiUnit


    let remove rm u =

        let rec rem u rm =
            let eqs = Group.eqsGroup rm

            match u with
            | CombiUnit (u1, op, u2) ->
                match u1 |> eqs,  u2 |> eqs with
                | true,  true  -> count
                | false, true  -> createCombiUnit (u1, op, count)
                | true,  false -> createCombiUnit (count, op, u2)
                | false, false ->
                    createCombiUnit ((rem u1 rm), op, (rem u2 rm))
            | _ ->
                if u |> eqs then count
                else u

        rem u rm


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


        let unitToList u =
            let rec toList u =
                match u with
                | CombiUnit (ul, op, ur) ->
                    let op =
                        match op with
                        | OpPer -> op |> OpDivItem
                        | OpPlus | OpMinus -> op |> OpPlusMinItem
                        | OpTimes -> op |> OpMultItem
                    (toList ul) @ [ op ] @ (toList ur)
                | _ -> [ u |> UnitItem ]

            toList u


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


        let eqs ui1 ui2 =
            match ui1, ui2 with
            | UnitItem u1, UnitItem u2 ->
                u1 |> Group.eqsGroup u2
            | _ -> false


        let isUnitItem ui =
            match ui with
            | UnitItem _ -> true
            | _          -> false


    let rec getUnits u =
        match u with
        | CombiUnit (ul, op, ur) ->
            ul
            |> getUnits
            |> List.append (ur |> getUnits)
        | _ -> [ u ]


    let simplify vu =
        let (_, u) = vu |> get

        let simpl u =
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

            let rec build ns ds u =
                match ns, ds with
                | [], _ ->
                    match ds with
                    | [] -> u
                    | _ ->
                        let d = ds |> List.reduce times
                        if u = NoUnit then
                            Count(Times 1N) |> per d
                        else u |> per d
                | h::tail, _ ->
                    if ds |> List.exists (Group.eqsGroup h) then
                        build tail (ds |> List.remove1 (Group.eqsGroup h)) u
                    else
                        if u = NoUnit then h
                        else u |> times h
                        |> build tail ds

            let ns, ds = u |> numDenom true

            NoUnit
            |> build ns ds
            |> (fun u -> if u = NoUnit then count else u)

        u
        |> function
        | _ when u = NoUnit -> vu
        | _ ->
            u
            |> simpl
            |> (fun u ->
                vu
                |> toBase
                |> create u
                |> toUnit
                |> create u
            )



    let calc op vu1 vu2 =

        let (ValueUnit (_, u1)) = vu1
        let (ValueUnit (_, u2)) = vu2

        let v = vu1 |> toBase |> op <| (vu2 |> toBase)

        let u =
            match op with
            | BigRational.Mult    -> (u1, OpTimes, u2) |> CombiUnit
            | BigRational.Div     -> (u1, OpPer,   u2) |> CombiUnit
            | BigRational.Add
            | BigRational.Subtr   ->
                if u1 |> Group.eqsGroup u2 then u2
                else
                    failwith <| sprintf "cannot add or subtract different units %A %A" u1 u2
            | BigRational.NoMatch -> failwith <| sprintf "invalid operator %A" op

        v
        |> create u
        |> toUnit
        |> create u
        |> simplify


    let cmp cp vu1 vu2 =
        (vu1 |> toBase) |> cp <| (vu2 |> toBase)


    let eq = cmp (=)


    let gt = cmp (>)


    let st = cmp (<)


    let gte = cmp (>=)


    let ste = cmp (<=)


    let convertTo u vu =
        let _, u_ = vu |> get
        if u = u_ then vu
        else
            vu
            |> toBase
            |> create u
            |> toUnit
            |> create u



    type ValueUnit with

        static member (*) (vu1, vu2) = calc (*) vu1 vu2

        static member (/) (vu1, vu2) = calc (/) vu1 vu2

        static member (+) (vu1, vu2) = calc (+) vu1 vu2

        static member (-) (vu1, vu2) = calc (-) vu1 vu2

        static member (=?) (vu1, vu2) = cmp (=) vu1 vu2

        static member (>?) (vu1, vu2) = cmp (>) vu1 vu2

        static member (<?) (vu1, vu2) = cmp (<) vu1 vu2

        static member (>=?) (vu1, vu2) = cmp (>=) vu1 vu2

        static member (<=?) (vu1, vu2) = cmp (<=) vu1 vu2

        static member (==>) (vu, u) = vu |> convertTo u



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

            let liter =  1N |> nLiter
            let deciLiter =  1N |> nDeciLiter
            let milliLiter =  1N |> nMilliLiter
            let microLiter =  1N |> nMicroLiter


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

                { Unit = Time.year; Group = Group.NoGroup;  Abbreviation = { Eng = "yr"; Dut = "jr" }; Name = { Eng = "year"; Dut = "jaar" }; Synonyms = ["years"; "jaren"] }
                { Unit = Time.month; Group = Group.NoGroup;  Abbreviation = { Eng = "mo"; Dut = "mnd" }; Name = { Eng = "month"; Dut = "maand" }; Synonyms = [] }
                { Unit = Time.week; Group = Group.NoGroup;  Abbreviation = { Eng = "wk"; Dut = "wk" }; Name = { Eng = "week"; Dut = "week" }; Synonyms = ["weeks"; "weken"] }
                { Unit = Time.day; Group = Group.NoGroup;  Abbreviation = { Eng = "day"; Dut = "dag" }; Name = { Eng = "day"; Dut = "dag" }; Synonyms = [] }
                { Unit = Time.hour; Group = Group.NoGroup;  Abbreviation = { Eng = "hr"; Dut = "uur" }; Name = { Eng = "hour"; Dut = "uur" }; Synonyms = [] }
                { Unit = Time.minute; Group = Group.NoGroup;  Abbreviation = { Eng = "min"; Dut = "min" }; Name = { Eng = "minute"; Dut = "minuut" }; Synonyms = [] }
                { Unit = Time.second; Group = Group.NoGroup;  Abbreviation = { Eng = "sec"; Dut = "sec" }; Name = { Eng = "second"; Dut = "seconde" }; Synonyms = [] }

                { Unit = Molar.mol; Group = Group.NoGroup;  Abbreviation = { Eng = "mol"; Dut = "mol" }; Name = { Eng = "mol"; Dut = "mol" }; Synonyms = [] }
                { Unit = Molar.milliMol; Group = Group.NoGroup;  Abbreviation = { Eng = "mmol"; Dut = "mmol" }; Name = { Eng = "millimol"; Dut = "millimol" }; Synonyms = [] }

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
            failwith <| sprintf "Cannot map combined unit %A" ((u1, op, u2) |> CombiUnit)


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
                    let (v, u) = u |> mapUnit
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


    let toString loc verb vu =
        let v, u = vu |> get

        (v |> BigRational.toString) + " " + (Units.toString loc verb u)


    let toStringDutchShort = toString Units.Dutch Units.Short
    let toStringDutchLong = toString Units.Dutch Units.Long
    let toStringEngShort = toString Units.English Units.Short
    let toStringEngLong = toString Units.English Units.Long


    let fromString s =

        let fs s =
            let dels = "#"

            let ufs s =
                match s |> String.trim |> String.split " " with
                | [ug] ->
                    match Units.fromString ug with
                    | Some (u') -> u' |> setUnitValue 1N
                    | None      -> failwith <| sprintf "Not a valid unit: %s" ug

                | [v;ug] ->
                    match v |> BigRational.tryParse with
                    | None ->
                        failwith <| sprintf "Cannot parse string: %s with value: %s" s v
                    | Some v' ->
                        match Units.fromString ug with
                        | Some (u') -> u' |> setUnitValue v'
                        | None     -> failwith <| sprintf "Not a valid unit: %s" ug
                | _ -> failwith <| sprintf "Cannot parse string %s" s

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
                failwith <| sprintf "Cannot parse string %s" s
            | Some v ->
                let u =
                    rest
                    |> String.concat " "
                    |> String.trim
                    |> fs
                (v, u) |> ValueUnit
        | _ ->
            if s = "" then failwith "Cannot parse empty string"
            else failwith <| sprintf "Cannot parse string %s" s



