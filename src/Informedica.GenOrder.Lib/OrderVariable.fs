namespace Informedica.GenOrder.Lib



/// Functions that deal with the `VariableUnit` type
module OrderVariable =

    open MathNet.Numerics

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL
    open Informedica.GenCore.Lib
    open Informedica.GenCore.Lib.Types.GenOrder
    open Informedica.GenSolver.Lib
    open Informedica.GenUnits.Lib

    module ValueRange = Variable.ValueRange
    module Minimum    = ValueRange.Minimum
    module Maximum    = ValueRange.Maximum
    module Increment  = ValueRange.Increment
    module ValueSet   = ValueRange.ValueSet

    module VariableDto = Variable.Dto
    module Units = ValueUnit.Units
    module Multipliers = ValueUnit.Multipliers


    module Constraints =


        let create min incr max vs =
            {
                Min = min
                Max = max
                Incr = incr
                Values = vs
            }


        let toString (cs : Constraints) =
            let toStr = BigRational.toFloat >> Double.toStringNumberNLWithoutTrailingZerosFixPrecision 3

            match cs.Values with
            | None ->
                let min = cs.Min |> Option.map Minimum.toBoolBigRational
                let max = cs.Max |> Option.map Maximum.toBoolBigRational
                let incr = cs.Incr |> Option.map (Increment.toList >> Set.ofSeq)
                MinIncrMax.toStringNL toStr min incr max
            | Some vs ->
                vs
                |> ValueSet.toSet
                |> Set.map toStr
                |> String.concat ", "


    /// Create a `OrderVariable` with preset values
    let create n min incr max vs un cs =
        ValueRange.create true min incr max vs
        |> fun vlr ->
            let var = Variable.create id n vlr
            {
                Constraints = cs
                Variable = var
                Unit = un
            }


    /// Create a new `VariableUnit` with
    /// `Name` **nm** and `Unit` **un**
    let createNew n un =
        let min = Minimum.create false 0N |> Some

        Constraints.create min None None None
        |> create n min None None None un


    /// Apply **f** to `VariableUnit` **vru**
    let apply f (ovar: OrderVariable) = ovar |> f


    /// Utility function to facilitate type inference
    let get = apply id


    /// Get all record fields from a `VariableUnit`
    let getVariableUnit { Variable = var; Unit = un } =
        var, un


    /// Get the `Variable` from a `VariableUnit`
    let getVar = getVariableUnit >> fst


    /// Get the `Variable.Name` from a `VariableUnit` **vru**
    let getName ovar = (ovar |> getVar).Name


    let eqsName ovar1 ovar2 = (ovar1 |> getName) = (ovar2 |> getName)



    /// Get the `Unit` from a `VariableUnit`
    let getUnit = getVariableUnit >> snd


    let hasUnit = getUnit >> ((<>) Unit.NoUnit)


    let scale n ovar =
        let calc = (*) n
        { ovar with
            Constraints =
                { ovar.Constraints with
                    Min = ovar.Constraints.Min |> Option.map (Minimum.map calc calc)
                    Max = ovar.Constraints.Max |> Option.map (Maximum.map calc calc)
                    Incr = ovar.Constraints.Incr |> Option.map (Increment.map calc)
                    Values = ovar.Constraints.Values |> Option.map (ValueSet.map calc)
                }
            Variable =
                ovar.Variable |> Variable.scale n
        }


    let toBase ovar =
        let u = ovar |> getUnit
        let n = u |> Multipliers.getMultiplier
        ovar |> scale n


    let toUnit ovar =
        let u = ovar |> getUnit
        let n = 1N / (u |> Multipliers.getMultiplier)
        ovar |> scale n


    let applyConstraints (ovar : OrderVariable) =
        { ovar with
            Variable =
                ovar.Variable.Values
                |> ValueRange.setOptMin ovar.Constraints.Min
                |> ValueRange.setOptMax ovar.Constraints.Max
                |> ValueRange.setOptIncr ovar.Constraints.Incr
                |> ValueRange.setOptVs ovar.Constraints.Values
                |> Variable.setValueRange true ovar.Variable
        }


    let fromOrdVar toOvar c ovars a =
        ovars
        |> List.tryFind (eqsName (a |> toOvar))
        |> Option.map c
        |> Option.defaultValue a


    /// Set the 'Name' to the `Variable` of the `VariableUnit`
    let setName nm ovar =
        { ovar with
            Variable = ovar.Variable |> Variable.setName nm
        }


    let setUnit u ovar : OrderVariable =
        { ovar with Unit = u }


    /// Get the string representation of a `VariableUnit` **vru**
    let toString exact ovar =
        let ns = ovar |> getName |> Variable.Name.toString
        let us = ovar.Unit |> ValueUnit.unitToString

        if us |> String.isNullOrWhiteSpace then ""
        else
            ns +
            (ovar.Variable
            |> Variable.getValueRange
            |> ValueRange.toString exact) + " " + us


    /// Returns the values with the string equivalents
    /// of an order variable value set
    let toValueUnitStringList get n x =
        x
        |> get
        |> getVar
        |> Variable.getValueRange
        |> Variable.ValueRange.getValSet
        |> function
        | Some (ValueSet.ValueSet vs) ->
            vs
            |> Seq.map (fun vs ->
                vs, x |> get |> getUnit
            )
            |> Seq.map (fun (v, u) ->
                let vs =
                    v
                    |> BigRational.toFloat
                    |> fun x ->
                        match n with
                        | Some n -> x |> Double.fixPrecision n
                        | None   -> x
                    |> string

                u
                |> ValueUnit.unitToString
                |> String.removeTextBetweenBrackets
                |> fun us -> v, $"%s{vs} %s{us}"
            )
        | None -> Seq.empty


    let toValueUnitString get n x =
        let toStr =
            BigRational.toFloat >>
            Double.toStringNumberNLWithoutTrailingZerosFixPrecision n

        let unt =
            x |> get |> getUnit
            |> ValueUnit.unitToString
            |> String.removeTextBetweenBrackets

        x
        |> get
        |> getVar
        |> Variable.getValueRange
        |> fun vr ->
            let min = vr |> ValueRange.getMin |> Option.map Minimum.toBoolBigRational
            let incr = vr |> ValueRange.getIncr |> Option.map (Increment.toList >> Set.ofList)
            let max = vr |> ValueRange.getMax |> Option.map Maximum.toBoolBigRational
            match vr |> ValueRange.getValSet with
            | Some vs ->
                vs
                |> ValueSet.toSet
                |> Set.map toStr
                |> Set.toSeq
                |> String.concat ", "
            | None ->
                MinIncrMax.toStringNL toStr min incr max
        |> fun s -> $"%s{s} %s{unt}"


    let getUnits vu =
        (vu |> get).Unit
        |> ValueUnit.getUnits


    let calcUnit op (vru1, vru2) =
        let u1 = vru1 |> getUnit
        let u2 = vru2 |> getUnit

        ValueUnit.calcUnit op u1 u2
        |> createNew ("result" |> Variable.Name.createExc)


    type OrderVariableCalc =
         | Mult
         | Div
         | Add
         | Subtr with

        static member (?<-) (op, vru1, vru2) =
            match op with
            | Mult  -> calcUnit (*) (vru1, vru2)
            | Div   -> calcUnit (/) (vru1, vru2)
            | Add   -> calcUnit (+) (vru1, vru2)
            | Subtr -> calcUnit (-) (vru1, vru2)


    module Operators =

        let inline (^*) vru1 vru2 = (?<-) Mult vru1 vru2

        let inline (^/) vru1 vru2 = (?<-) Mult vru1 vru2

        let inline (^+) vru1 vru2 = (?<-) Mult vru1 vru2

        let inline (^-) vru1 vru2 = (?<-) Mult vru1 vru2



    /// Type and functions to handle the `Dto`
    /// data transfer type for a `VariableUnit`
    module Dto =

        module ValueRange = Variable.ValueRange

        type VarDto () =
            member val Min : BigRational option = None with get, set
            member val MinIncl = false with get, set
            member val Incr : BigRational list = [] with get, set
            member val Max : BigRational option = None with get, set
            member val MaxIncl = false with get, set
            member val Vals : BigRational list = [] with get, set


        type Dto () =
            member val Name = "" with get, set
            member val Unit = "" with get, set
            member val Constraints = VarDto () with get, set
            member val Variable = VarDto () with get, set


        let dto () = Dto ()


        let fromDto (dto: Dto) =
            let un =
                if dto.Unit |> String.isNullOrWhiteSpace then Unit.NoUnit
                else
                    dto.Unit
                    |> ValueUnit.unitFromString
                    |> Option.defaultValue Unit.NoUnit

            let cs =
                let vs =
                    dto.Constraints.Vals
                    |> function
                    | [] -> None
                    | xs -> xs |> Set.ofList |> ValueSet.create |> Some

                let incr =
                    dto.Constraints.Incr
                    |> function
                    | [] -> None
                    | xs -> xs |> Set.ofList |> Increment.create |> Some

                let min  = dto.Constraints.Min  |> Option.map  (Minimum.create  dto.Constraints.MinIncl)
                let max  = dto.Constraints.Max  |> Option.map  (Maximum.create  dto.Constraints.MaxIncl)
                Constraints.create min incr max vs

            let n = dto.Name |> Name.fromString
            let vals =
                dto.Variable.Vals
                |> function
                | [] -> None
                | xs -> xs |> Set.ofList |> ValueSet.create |> Some

            let incr =
                dto.Variable.Incr
                |> function
                | [] -> None
                | xs -> xs |> Set.ofList |> Increment.create |> Some

            let min  = dto.Variable.Min  |> Option.map  (Minimum.create  dto.Variable.MinIncl)
            let max  = dto.Variable.Max  |> Option.map  (Maximum.create  dto.Variable.MaxIncl)

            create n min incr max vals un cs


        let toDto (ovar : OrderVariable) =
            let dto = dto ()
            let vr =
                ovar
                |> getVar
                |> Variable.getValueRange

            dto.Name <-
                ovar |> getName |> Name.toString
            dto.Unit <-
                ovar |> getUnit |> ValueUnit.unitToString

            dto.Variable.Vals <-
                vr
                |> ValueRange.getValSet
                |> Option.map (ValueSet.toSet >> Set.toList)
                |> Option.defaultValue []
            dto.Variable.Incr <-
                vr
                |> ValueRange.getIncr
                |> Option.map Increment.toList
                |> Option.defaultValue []
            dto.Variable.Min <-
                vr
                |> ValueRange.getMin
                |> Option.map Minimum.toBigRational
            dto.Variable.MinIncl <-
                vr
                |> ValueRange.getMin
                |> Option.map Minimum.isIncl
                |> Option.defaultValue false
            dto.Variable.Max <-
                vr
                |> ValueRange.getMax
                |> Option.map Maximum.toBigRational
            dto.Variable.MaxIncl <-
                vr
                |> ValueRange.getMax
                |> Option.map Maximum.isIncl
                |> Option.defaultValue false

            dto.Constraints.Vals <-
                ovar.Constraints.Values
                |> Option.map (ValueSet.toSet >> Set.toList)
                |> Option.defaultValue []
            dto.Constraints.Incr <-
                ovar.Constraints.Incr
                |> Option.map Increment.toList
                |> Option.defaultValue []

            dto.Constraints.Min <-
                ovar.Constraints.Min
                |> Option.map Minimum.toBigRational
            dto.Constraints.MinIncl <-
                ovar.Constraints.Min
                |> Option.map Minimum.isIncl
                |> Option.defaultValue false
            dto.Constraints.Max <-
                ovar.Constraints.Max
                |> Option.map Maximum.toBigRational
            dto.Constraints.MaxIncl <-
                ovar.Constraints.Max
                |> Option.map Maximum.isIncl
                |> Option.defaultValue false

            dto



    /// Type and functions that represent a count
    module Count =

        let count = Count.Count


        let [<Literal>] name = "cnt"


        /// Turn `Count` in a `VariableUnit`
        let toOrdVar (Count.Count cnt) = cnt


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString


        let getUnits = toOrdVar >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> count


        /// Set a `Count` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar count


        /// Create a `Count` with name **n**
        let create n =
            Units.Count.times
            |> createNew (n |> Name.add name)
            |> Count.Count


        /// Turn a `Count` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `Count` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toValueUnitString = toValueUnitString toOrdVar


        let toBase = toOrdVar >> toBase >> count


        let toUnit = toOrdVar >> toUnit >> count


        let applyConstraints = toOrdVar >> applyConstraints >> count




    /// Type and functions that represent a time
    module Time =

        let time = Time.Time


        let [<Literal>] name = "tme"


        /// Turn `Time` in a `VariableUnit`
        let toOrdVar (Time.Time tme) = tme


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString


        let getUnits = toOrdVar >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> time


        /// Set a `Time` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar time


        /// Create a `Time` with name **n**
        /// with `Unit` **un**
        let create n un =
            un
            |> createNew (n |> Name.add name)
            |> Time.Time


        /// Turn a `Time` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `Time` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toValueUnitString = toValueUnitString toOrdVar


        let toBase = toOrdVar >> toBase >> time


        let toUnit = toOrdVar >> toUnit >> time


        let applyConstraints = toOrdVar >> applyConstraints >> time



    /// Type and functions that represent a frequency
    module Frequency =


        let [<Literal>] name = "frq"


        /// Turn `Frequency` in a `VariableUnit`
        let toOrdVar (Frequency frq) = frq


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString


        let getUnits = toOrdVar >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Frequency


        /// Set a `Frequency` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar Frequency


        /// Create a `Frequency` with name **n**
        /// with `Unit` time unit **tu**
        let create n tu =
            match tu with
            | Unit.NoUnit -> Unit.NoUnit
            | _ ->
                Units.Count.times
                |> ValueUnit.per tu
            |> createNew (n |> Name.add name)
            |> Frequency


        /// Turn a `Frequency` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `Frequency` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toValueUnitString = toValueUnitString toOrdVar


        let toBase = toOrdVar >> toBase >> Frequency


        let toUnit = toOrdVar >> toUnit >> Frequency


        let applyConstraints = toOrdVar >> applyConstraints >> Frequency



    /// Type and functions that represent a concentration,
    /// and a concentration is a quantity per time
    module Concentration =


        let [<Literal>] name = "cnc"


        /// Turn `Concentration` in a `VariableUnit`
        let toOrdVar (Concentration cnc) = cnc


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString


        let getUnits =
            toOrdVar
            >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Concentration


        /// Set a `Concentration` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar Concentration


        /// Create a `Concentration` with name **n**
        /// and `Unit` **un** per shape unit **su**
        let create n un su =
            match un, su with
            | Unit.NoUnit, _
            | _, Unit.NoUnit -> Unit.NoUnit
            | _ ->
                un
                |> ValueUnit.per su
            |> createNew (n |> Name.add name)
            |> Concentration


        /// Turn a `Concentration` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `Concentration` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toValueUnitString = toValueUnitString toOrdVar


        let toBase = toOrdVar >> toBase >> Concentration


        let toUnit = toOrdVar >> toUnit >> Concentration


        let applyConstraints = toOrdVar >> applyConstraints >> Concentration



    /// Type and functions that represent a quantity
    module Quantity =


        let [<Literal>] name = "qty"


        /// Turn `Quantity` in a `VariableUnit`
        let toOrdVar (Quantity qty) = qty


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString


        let getUnits =
            toOrdVar
            >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Quantity


        /// Set a `Quantity` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar Quantity


        /// Create a `Quantity` with name **n**
        /// and `Unit` **un**
        let create n un =
            un
            |> createNew (n |> Name.add name)
            |> Quantity


        /// Turn a `Quantity` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `Quantity` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toValueUnitString = toValueUnitString toOrdVar


        let toBase = toOrdVar >> toBase >> Quantity


        let toUnit = toOrdVar >> toUnit >> Quantity


        let applyConstraints = toOrdVar >> applyConstraints >> Quantity



    /// Type and functions that represent a quantity per time
    module PerTime =


        let [<Literal>] name = "ptm"


        /// Turn `PerTime` in a `VariableUnit`
        let toOrdVar (PerTime ptm) = ptm


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toOrdVar
            >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> PerTime


        /// Set a `PerTime` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar PerTime


        /// Create a `PerTime` with name **n**
        /// and `Unit` **un** and time unit **tu**
        let create n un tu =
            match un with
            | Unit.NoUnit -> Unit.NoUnit
            | _ ->
                un
                |> ValueUnit.per tu
            |> createNew (n |> Name.add name)
            |> PerTime


        /// Turn a `PerTime` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `PerTime` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toValueUnitString = toValueUnitString toOrdVar


        let toBase = toOrdVar >> toBase >> PerTime


        let toUnit = toOrdVar >> toUnit >> PerTime


        let applyConstraints = toOrdVar >> applyConstraints >> PerTime



    module Rate =


        let [<Literal>] name = "rte"


        /// Turn `PerTime` in a `VariableUnit`
        let toOrdVar (Rate rte) = rte


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toOrdVar
            >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Rate


        /// Set a `PerTime` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar Rate


        /// Create a `PerTime` with name **n**
        /// and `Unit` **un** and time unit **tu**
        let create n un tu =
            match un with
            | Unit.NoUnit -> Unit.NoUnit
            | _ ->
                un
                |> ValueUnit.per tu
            |> createNew (n |> Name.add name)
            |> Rate


        /// Turn a `PerTime` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `PerTime` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toValueUnitString = toValueUnitString toOrdVar


        let toBase = toOrdVar >> toBase >> Rate


        let toUnit = toOrdVar >> toUnit >> Rate


        let applyConstraints = toOrdVar >> applyConstraints >> Rate



    /// Type and functions that represent a total
    module Total =


        let [<Literal>] name = "tot"


        /// Turn `Quantity` in a `VariableUnit`
        let toOrdVar (Total tot) = tot


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString


        let getUnits =
            toOrdVar
            >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Total


        /// Set a `Quantity` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar Total


        /// Create a `Quantity` with name **n**
        /// and `Unit` **un**
        let create n un =
            un
            |> createNew (n |> Name.add name)
            |> Total


        /// Turn a `Quantity` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `Quantity` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toValueUnitString = toValueUnitString toOrdVar


        let toBase = toOrdVar >> toBase >> Total


        let toUnit = toOrdVar >> toUnit >> Total


        let applyConstraints = toOrdVar >> applyConstraints >> Total



    /// Type and functions that represent a adjusted quantity,
    /// and a adjusted quantity is a quantity per time
    module QuantityAdjust =


        let [<Literal>] name = "qty_adj"


        /// Turn `QuantityAdjust` in a `VariableUnit`
        let toOrdVar (QuantityAdjust qty_adj) = qty_adj


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toOrdVar
            >> getUnits

        let toDto = toOrdVar >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> QuantityAdjust

        /// Set a `QuantityAdjust` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar QuantityAdjust


        /// Create a `QuantityAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj**
        let create n un adj =
            match un, adj with
            | Unit.NoUnit, _
            | _, Unit.NoUnit -> Unit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
            |> createNew (n |> Name.add name)
            |> QuantityAdjust


        /// Turn a `QuantityAdjust` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `QuantityAdjust` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toValueUnitString = toValueUnitString toOrdVar


        let toBase = toOrdVar >> toBase >> QuantityAdjust


        let toUnit = toOrdVar >> toUnit >> QuantityAdjust


        let applyConstraints = toOrdVar >> applyConstraints >> QuantityAdjust



    /// Type and functions that represent a adjusted total,
    /// and a adjusted total is a quantity per time
    module PerTimeAdjust =


        let [<Literal>] name = "ptm_adj"


        /// Turn `TotalAdjust` in a `VariableUnit`
        let toOrdVar (PerTimeAdjust ptm_adj) = ptm_adj


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString


        let getUnits =
            toOrdVar
            >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> PerTimeAdjust


        /// Set a `TotalAdjust` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar PerTimeAdjust


        /// Create a `TotalAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj** per time unit **tu**
        let create n un adj tu =
            match un, adj, tu with
            | Unit.NoUnit, _, _
            | _, Unit.NoUnit, _
            | _, _, Unit.NoUnit -> Unit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
                |> ValueUnit.per tu
            |> createNew (n |> Name.add name)
            |> PerTimeAdjust


        /// Turn a `TotalAdjust` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `TotalAdjust` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toValueUnitString = toValueUnitString toOrdVar


        let toBase = toOrdVar >> toBase >> PerTimeAdjust


        let toUnit = toOrdVar >> toUnit >> PerTimeAdjust


        let applyConstraints = toOrdVar >> applyConstraints >> PerTimeAdjust



    /// Type and functions that represent a adjusted total,
    /// and a adjusted total is a quantity per time
    module RateAdjust =


        let [<Literal>] name = "rte_adj"


        /// Turn `TotalAdjust` in a `VariableUnit`
        let toOrdVar (RateAdjust rte_adj) = rte_adj


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString


        let getUnits =
            toOrdVar
            >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> RateAdjust


        /// Set a `TotalAdjust` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar RateAdjust


        /// Create a `TotalAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj** per time unit **tu**
        let create n un adj tu =
            match un, adj, tu with
            | Unit.NoUnit, _, _
            | _, Unit.NoUnit, _
            | _, _, Unit.NoUnit -> Unit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
                |> ValueUnit.per tu
            |> createNew (n |> Name.add name)
            |> RateAdjust


        /// Turn a `TotalAdjust` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `TotalAdjust` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toValueUnitString = toValueUnitString toOrdVar


        let toBase = toOrdVar >> toBase >> RateAdjust


        let toUnit = toOrdVar >> toUnit >> RateAdjust


        let applyConstraints = toOrdVar >> applyConstraints >> RateAdjust



    /// Type and functions that represent a adjusted quantity,
    /// and a adjusted quantity is a quantity per time
    module TotalAdjust =


        let [<Literal>] name = "tot_adj"


        /// Turn `QuantityAdjust` in a `VariableUnit`
        let toOrdVar (TotalAdjust tot_adj) = tot_adj


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toOrdVar
            >> getUnits

        let toDto = toOrdVar >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> TotalAdjust

        /// Set a `QuantityAdjust` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar TotalAdjust


        /// Create a `QuantityAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj**
        let create n un adj =
            match un, adj with
            | Unit.NoUnit, _
            | _, Unit.NoUnit -> Unit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
            |> createNew (n |> Name.add name)
            |> TotalAdjust


        /// Turn a `QuantityAdjust` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `QuantityAdjust` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toValueUnitString = toValueUnitString toOrdVar


        let toBase = toOrdVar >> toBase >> TotalAdjust


        let toUnit = toOrdVar >> toUnit >> TotalAdjust


        let applyConstraints = toOrdVar >> applyConstraints >> TotalAdjust

