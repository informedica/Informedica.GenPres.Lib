namespace Informedica.GenOrder.Lib



/// Functions that deal with the `VariableUnit` type
module OrderVariable =

    open MathNet.Numerics

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL
    open Informedica.GenCore.Lib.Ranges
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


    let createVr c u v = 
        [|v|] 
        |> ValueUnit.create u
        |> c

    let createMinIncl = createVr (Minimum.create true)
    let createMinExcl = createVr (Minimum.create false)
    let createMaxIncl = createVr (Maximum.create true)
    let createMaxExcl = createVr (Maximum.create false)
    let createIncr = createVr Increment.create
    let createValSet u v = 
        v 
        |> Array.ofSeq
        |> ValueUnit.create u
        |> ValueSet.create


    /// Create a `OrderVariable` with preset values
    let create un var vr =
        {
            Variable = var
            Constraints = vr
            Unit = un
        }


    /// Create a new `VariableUnit` with
    /// `Name` **nm** and `Unit` **un**
    let createNew n un = 
        let var = n |> Variable.empty
        
        var 
        |> Variable.getValueRange
        |> create un var


    let hasUnit (ovar : OrderVariable) = ovar.Unit <> NoUnit


    /// Apply **f** to `VariableUnit` **vru**
    let apply f (ovar: OrderVariable) = ovar |> f


    /// Utility function to facilitate type inference
    let get = apply id


    /// Get all record fields from a `VariableUnit`
    let getVariable { Variable = var } = var
 

    /// Get the `Variable` from a `VariableUnit`
    let getValues ovar =  (ovar |> getVariable).Values 


    /// Get the `Variable.Name` from a `VariableUnit` **vru**
    let getName ovar = (ovar |> getVariable).Name


    let eqsName ovar1 ovar2 = (ovar1 |> getName) = (ovar2 |> getName)


    let applyConstraints (ovar : OrderVariable) =
        { ovar with
            Variable =
                ovar.Constraints
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


    /// Get the string representation of a `VariableUnit` **vru**
    let toString exact ovar =
        let ns = ovar |> getName |> Variable.Name.toString
        ns +
        (ovar.Variable
        |> Variable.getValueRange
        |> ValueRange.toString exact)


    /// Returns the values with the string equivalents
    /// of an order variable value set
    let toValueUnitStringList get x =
        x
        |> get
        |> getVariable
        |> Variable.getValueRange
        |> Variable.ValueRange.getValSet
        |> function
        | Some (ValueSet.ValueSet vs) ->
            vs
            |> ValueUnit.toStringDutchShort
        | None -> ""


    let toValueUnitString get x =
        x
        |> get
        |> getVariable
        |> Variable.getValueRange
        |> fun vr ->
            match vr |> ValueRange.getValSet with
            | Some vs ->
                vs
                |> ValueSet.toSet
                |> ValueUnit.toStringDutchShort
            | None ->
                let min = vr |> ValueRange.getMin |> Option.map Minimum.toBoolValueUnit
                let incr = vr |> ValueRange.getIncr |> Option.map (Increment.toValueUnit >> List.singleton)
                let max = vr |> ValueRange.getMax |> Option.map Maximum.toBoolValueUnit

                MinIncrMax.Calculator.toStringNL ValueUnit.toStringDutchShort min incr max



    /// Type and functions to handle the `Dto`
    /// data transfer type for a `VariableUnit`
    module Dto =


        type Dto () =
            member val Variable = Variable.Dto.dto () with get, set
            member val Constraints = Variable.Dto.dto () with get, set
            member val Unit = "" with get, set


        let dto () = Dto ()


        let fromDto (dto: Dto) =
            dto.Unit 
            |> Units.fromString
            |> Option.map (fun un ->
                dto.Constraints 
                |> Variable.Dto.fromDto
                |> Variable.getValueRange
                |> create un (dto.Variable |> Variable.Dto.fromDto)
            )


        let toDto (ovar : OrderVariable) =
            let dto = dto ()
            dto.Variable <- ovar.Variable |> Variable.Dto.toDto
            dto.Constraints <- 
                ovar.Constraints 
                |> Variable.createSucc ovar.Variable.Name
                |> Variable.Dto.toDto

            dto



    /// Type and functions that represent a count
    module Count =

        let count = Count.Count


        let [<Literal>] name = "cnt"


        /// Turn `Count` in a `VariableUnit`
        let toOrdVar (Count.Count cnt) = cnt


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Option.map count


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


        let applyConstraints = toOrdVar >> applyConstraints >> count




    /// Type and functions that represent a time
    module Time =

        let time = Time.Time


        let [<Literal>] name = "tme"


        /// Turn `Time` in a `VariableUnit`
        let toOrdVar (Time.Time tme) = tme


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Option.map time


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



        let applyConstraints = toOrdVar >> applyConstraints >> time



    /// Type and functions that represent a frequency
    module Frequency =


        let [<Literal>] name = "frq"


        /// Turn `Frequency` in a `VariableUnit`
        let toOrdVar (Frequency frq) = frq



        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Option.map Frequency


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


        let applyConstraints = toOrdVar >> applyConstraints >> Frequency



    /// Type and functions that represent a concentration,
    /// and a concentration is a quantity per time
    module Concentration =


        let [<Literal>] name = "cnc"


        /// Turn `Concentration` in a `VariableUnit`
        let toOrdVar (Concentration cnc) = cnc



        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Option.map Concentration


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



        let applyConstraints = toOrdVar >> applyConstraints >> Concentration



    /// Type and functions that represent a quantity
    module Quantity =


        let [<Literal>] name = "qty"


        /// Turn `Quantity` in a `VariableUnit`
        let toOrdVar (Quantity qty) = qty



        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Option.map Quantity


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


        let applyConstraints = toOrdVar >> applyConstraints >> Quantity



    /// Type and functions that represent a quantity per time
    module PerTime =


        let [<Literal>] name = "ptm"


        /// Turn `PerTime` in a `VariableUnit`
        let toOrdVar (PerTime ptm) = ptm


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Option.map PerTime


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


        let applyConstraints = toOrdVar >> applyConstraints >> PerTime



    module Rate =


        let [<Literal>] name = "rte"


        /// Turn `PerTime` in a `VariableUnit`
        let toOrdVar (Rate rte) = rte


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Option.map Rate


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


        let applyConstraints = toOrdVar >> applyConstraints >> Rate



    /// Type and functions that represent a total
    module Total =


        let [<Literal>] name = "tot"


        /// Turn `Quantity` in a `VariableUnit`
        let toOrdVar (Total tot) = tot


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Option.map Total


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


        let applyConstraints = toOrdVar >> applyConstraints >> Total



    /// Type and functions that represent a adjusted quantity,
    /// and a adjusted quantity is a quantity per time
    module QuantityAdjust =


        let [<Literal>] name = "qty_adj"


        /// Turn `QuantityAdjust` in a `VariableUnit`
        let toOrdVar (QuantityAdjust qty_adj) = qty_adj



        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Option.map QuantityAdjust

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


        let applyConstraints = toOrdVar >> applyConstraints >> QuantityAdjust



    /// Type and functions that represent a adjusted total,
    /// and a adjusted total is a quantity per time
    module PerTimeAdjust =


        let [<Literal>] name = "ptm_adj"


        /// Turn `TotalAdjust` in a `VariableUnit`
        let toOrdVar (PerTimeAdjust ptm_adj) = ptm_adj


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Option.map PerTimeAdjust


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


        let applyConstraints = toOrdVar >> applyConstraints >> PerTimeAdjust



    /// Type and functions that represent a adjusted total,
    /// and a adjusted total is a quantity per time
    module RateAdjust =


        let [<Literal>] name = "rte_adj"


        /// Turn `TotalAdjust` in a `VariableUnit`
        let toOrdVar (RateAdjust rte_adj) = rte_adj


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Option.map RateAdjust


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


        let applyConstraints = toOrdVar >> applyConstraints >> RateAdjust



    /// Type and functions that represent a adjusted quantity,
    /// and a adjusted quantity is a quantity per time
    module TotalAdjust =


        let [<Literal>] name = "tot_adj"


        /// Turn `QuantityAdjust` in a `VariableUnit`
        let toOrdVar (TotalAdjust tot_adj) = tot_adj

        let toDto = toOrdVar >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Option.map TotalAdjust

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


        let applyConstraints = toOrdVar >> applyConstraints >> TotalAdjust

