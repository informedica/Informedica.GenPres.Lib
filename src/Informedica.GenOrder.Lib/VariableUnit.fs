namespace Informedica.GenOrder.Lib

/// Functions that deal with the `VariableUnit` type
module VariableUnit =

    open MathNet.Numerics

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL
    open Informedica.GenUnits.Lib

    open Informedica.GenSolver.Lib.Types
    open Types
    open WrappedString
    
    module Variable    = Informedica.GenSolver.Lib.Variable
    module ValueRange  = Variable.ValueRange
    module Minimum = ValueRange.Minimum
    module Maximum = ValueRange.Maximum
    module Increment = ValueRange.Increment
    module Equation    = Informedica.GenSolver.Lib.Equation

    module VariableDto = Informedica.GenSolver.Lib.Variable.Dto
    module Units = ValueUnit.Units

    type Unit = ValueUnit.Unit


    /// Create a new `VariableUnit` with
    /// `Name` **nm** and `Unit` **un**
    let createNew n un = 
        let var = 
            Variable.createSucc n ValueRange.unrestricted
            |> Variable.setNonZeroOrNegative
        { Variable = var; Unit = un } 
        
    /// Create a `VariableUnit` with preset values
    let create n vs min incr max un =

        ValueRange.create vs min incr max
        |> function
        | vlr ->
            let var = Variable.create id n vlr        
            { Variable = var; Unit = un }


    /// Create a `VariableUnit` with
    /// `Variable` **var** and `Unit` **un**
    let withVar un var =    
        { Variable = var; Unit = un } 

    /// Apply **f** to `VariableUnit` **vru**
    let apply f (vru: VariableUnit) = vru |> f

    /// Utitlity function to facitlitate type inference
    let get = apply id

    /// Get all record fiels from a `VariableUnit`
    let getAll { Variable = var; Unit = un } =
        var, un

    /// Get the `Variable` from a `VariableUnit`
    let getVar = apply (fun vu -> vu.Variable)

    /// Get the `Variable.Name` from a `VariableUnit` **vru**
    let getName vru = (vru |> getVar).Name


    /// Get the `Unit` from a `VariableUnit`
    let getUnit = apply (fun vu -> vu.Unit)

    let hasUnit = getUnit >> ((<>) ValueUnit.NoUnit)


    /// Try find the first element with **n**
    /// in a list of list **xsl**
    /// with a function **get** to
    /// get **n** from an element
    let tryFind get n xsl =
        let pred x = x |> get = n
        match xsl |> List.filter (fun xs -> xs |> List.exists pred) with
        | [] -> None
        | xs::_ -> xs |> List.find pred |> Some   


    /// Try find the first `VariableUnit` with 
    /// a specific `Name` in a list of lists
    let tryFindVarUnt = tryFind getName

    /// Set a specific `VariableUnit` with
    /// a `Variable` from a 
    /// list of `Variable` lists **vrll** 
    /// that has the same name as **vru**.
    /// Return the unmodified **vru** if 
    /// no `Variable` can be found. 
    /// **c** is used to construct the specific
    /// variable and **toVar** to extract the
    /// current variable from **vru**
    let fromVar tovru c eqs a = 
        let n =
            a 
            |> tovru
            |> getName
        let v =  eqs |> tryFindVarUnt n
        
        match v with
        | Some x -> 
            x 
            |> c
        | _   -> 
            a 


    /// Set the 'Name' to the `Variable` of the `VariableUnit`
    let setName nm vru = 
        { vru with
            Variable = vru.Variable |> Variable.setName nm }   
           
    let setUnit u vru : VariableUnit =
        { vru with Unit = u }

    let valueToBase v vru =
        let u = vru |> getUnit

        v
        |> ValueUnit.valueToBase u


    let setValue c set v vru =
        let i = 
            vru
            |> valueToBase (v |> List.head)
            |> c

        vru
        |> getVar
        |> Variable.getValueRange
        |> set i
        |> fun vr ->
            { vru with 
                Variable = 
                    vr 
                    |> Variable.setValueRange vru.Variable }


    let setMinIncl =
        setValue (Minimum.createMin true) ValueRange.setMin
        

    let setMaxIncl =
        setValue (Maximum.createMax true) ValueRange.setMax

    let setIncr vs vru =

        let incr =
            vs
            |> List.map (fun v -> vru |> valueToBase v)
            |> Set.ofList
            |> Increment.createIncr

        vru
        |> getVar
        |> Variable.getValueRange
        |> ValueRange.setIncr incr
        |> fun vr ->
            { vru with 
                Variable = 
                    vr 
                    |> Variable.setValueRange vru.Variable }


    let setVals vs vru =
        let vs =
            vs
            |> List.map (fun v -> vru |> valueToBase v)
            |> Set.ofList

        vru
        |> getVar
        |> Variable.getValueRange
        |> ValueRange.setValues vs
        |> fun vr ->
            { vru with 
                Variable = 
                    vr 
                    |> Variable.setValueRange vru.Variable }


    /// Get the string representation of a `VariableUnit` **vru**
    let toString exact vru = 
        let ns = vru |> getName |> Variable.Name.toString
        let us = vru.Unit |> ValueUnit.unitToString

        if us |> String.isNullOrWhiteSpace then ""
        else
            ns +
            (vru.Variable 
            |> Variable.getValueRange
            |> ValueRange.toStringWithUnit exact vru.Unit) + " " + us


    let getBaseValues =
        getVar
        >> Variable.getValueRange
        >> Variable.ValueRange.getValueSet
        >> function
        | Some vs -> vs 
        | None -> Set.empty
    

    // ToDo change this to valuerange contains!!
    let hasBaseValue v =
        getBaseValues
        >> Seq.exists ((=) v)


    let containsUnitValue v vru =
        let u = vru |> getUnit
        // ToDo create util function in GenUnits.Lib
        let v = 
            ValueUnit.create u v
            |> ValueUnit.toBase
        
        vru
        |> getVar
        |> Variable.getValueRange
        |> ValueRange.contains v


    let getUnitValues vru =
        vru
        |> getBaseValues
        |> Seq.map (fun vs -> 
            vs, vru |> getUnit
        )
        |> Seq.map (fun (v, u) ->
            v
            |> ValueUnit.create u
            |> ValueUnit.toUnit
        )
        
    let toValueUnitStringList get n x =
        x
        |> get
        |> getVar
        |> Variable.getValueRange
        |> Variable.ValueRange.getValueSet
        |> function 
        | Some vs ->
            vs
            |> Seq.map (fun vs -> 
                vs, x |> get |> getUnit
            )
            |> Seq.map (fun (v, u) ->
                v
                |> ValueUnit.create u
                |> ValueUnit.toUnit
                |> fun v ->
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
                    |> fun us -> v, sprintf "%s %s" vs us
            )
        | None -> Seq.empty


    let getUnits vu =
        (vu |> get).Unit
        |> ValueUnit.getUnits


    /// Helper functions for `Informedica.GenSolver.Variable.Name` type
    module Name =
     
        module Name = Informedica.GenSolver.Lib.Variable.Name


        /// Create a `Name` from a list of strings that 
        let create ns = ns |> String.concat "." |> Name.createExc

        let toString = Name.toString


    let calcUnit op (vru1, vru2) =
        let u1 = vru1 |> getUnit
        let u2 = vru2 |> getUnit

        ValueUnit.calcUnit op u1 u2
        |> createNew ("result" |> Variable.Name.createExc)

    type VariableUnitCalc =
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
                    
        type Dto () = 
            member val Name = "" with get, set
            member val Unit = "" with get, set
            member val Vals : BigRational list = [] with get, set
            member val Min : BigRational option = None with get, set
            member val MinIncl = false with get, set
            member val Incr : BigRational list = [] with get, set
            member val Max : BigRational option = None with get, set
            member val MaxIncl = false with get, set 

        let dto () = Dto ()

        let fromDto (dto: Dto) =
            let un =
                if dto.Unit |> String.isNullOrWhiteSpace then 
                    ValueUnit.NoUnit
                else
                    dto.Unit
                    |> ValueUnit.unitFromString
                    |> function
                    | Some u -> u
                    | None -> ValueUnit.NoUnit

            let toBase = 
                ValueUnit.create un
                >> ValueUnit.toBase

            let map  = Option.map
            let none = Option.none
            let bind = Option.bind

            let n    = [ dto.Name ] |> Name.create 
            let vals = 
                match dto.Vals with
                | [] -> None
                | _ ->
                    dto.Vals
                    |> List.map toBase 
                    |> Set.ofList 
                    |> Some

            let min  = dto.Min  |> map  (toBase >> Minimum.createMin  dto.MinIncl)
            let incr = 
                dto.Incr
                |> List.map toBase 
                |> function
                | [] -> None
                | incr -> 
                    incr
                    |> Set.ofList
                    |> Increment.createIncr |>Some

            let max  = dto.Max  |> map  (toBase >> Maximum.createMax  dto.MaxIncl)

            create n vals min incr max un

        let toDto (vu : VariableUnit) =
            let toUnit = 
                ValueUnit.create vu.Unit
                >> ValueUnit.toUnit

            let dto = dto ()
            let vr =
                vu 
                |> getVar 
                |> Variable.getValueRange
            let min, inclMin = 
                vr 
                |> ValueRange.getMin
                |> function 
                | Some m -> 
                    m 
                    |> Minimum.minToBigRational
                    |> toUnit
                    |> Some, m |> Minimum.isMinIncl
                | None -> None, false
            let max, inclMax = 
                vr 
                |> ValueRange.getMax
                |> function 
                | Some m -> 
                    m 
                    |> Maximum.maxToBigRational
                    |> toUnit
                    |> Some, 
                    m |> Maximum.isMaxIncl
                | None -> None, false

            dto.Name <- 
                vu |> getName |> Name.toString
            dto.Unit <-
                vu |> getUnit |> ValueUnit.unitToString
            dto.Vals <-
                vr
                |> ValueRange.getValueSet
                |> function
                | Some vs ->
                    vs
                    |> Set.toList
                    |> List.map toUnit
                | None -> []

            dto.Incr <-
                vr
                |> ValueRange.getIncr
                |> function
                | None -> []
                | Some i -> 
                    i
                    |> Increment.incrToBigRationalSet 
                    |> Set.toList
                    |> List.map toUnit

            dto.Min <- min
            dto.MinIncl <- inclMin
            dto.Max <- max
            dto.MaxIncl <- inclMax

            dto


    /// Type and functions that represent a frequency
    module Frequency =

        /// String representation of the type
        let name = "Freq"

        /// Turn `Frequency` in a `VariableUnit`
        let toVarUnt (Frequency freq) = freq

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits = toVarUnt >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Frequency

        /// Set a `Frequency` with a `Variable`
        /// in a list fromVariable` lists
        let fromVar = fromVar toVarUnt Frequency 

        /// Create a `Frequency` with a preset `Variable`
        let create n vs min incr max un = 
            create n vs min incr max un 
            |> Frequency
        
        /// Create a `Frequency` with name **n**
        /// with `Unit` time unit **tu**
        let frequency n tu = 
            let n = [name] |> List.append n |> Name.create

            match tu with 
            | ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ -> 
                Units.Count.times
                |> ValueUnit.per tu
            |> createNew n 
            |> Frequency

        /// Turn a `Frequency` to a string
        let toString = toVarUnt >> (toString false)

        /// Print a `Frequency` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toVarUnt

        let getBaseValues = toVarUnt >> getBaseValues

        let getUnitValues = toVarUnt >> getUnitValues


    /// Type and functions that represent a time
    module Time = 

        /// String representation of the type
        let name = "Time"

        /// Turn `Time` in a `VariableUnit`
        let toVarUnt (Time time) = time

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits = toVarUnt >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Time
        
        /// Set a `Time` with a `Variable`
        /// in a list fromVariable` lists
        let fromVar = fromVar toVarUnt Time 

        /// Create a `Time` with a preset `Variable`
        let create n vs min incr max un = 
            create n vs min incr max un 
            |> Time
            
        /// Create a `Time` with name **n**
        /// with `Unit` **un**
        let time n un = 
            let n = [name] |> List.append n |> Name.create
            
            createNew n un 
            |> Time

        /// Turn a `Time` to a string
        let toString = toVarUnt >> (toString false)

        /// Print a `Time` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toVarUnt

        let getBaseValues = toVarUnt >> getBaseValues

        let getUnitValues = toVarUnt >> getUnitValues


    /// Type and functions that represent a count
    module Count =    
    
        /// String representation of the type
        let name = "Count" 


        /// Turn `Count` in a `VariableUnit`
        let toVarUnt (Count qty) = qty

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits = toVarUnt >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Count
        
        /// Set a `Count` with a `Variable`
        /// in a list fromVariable` lists
        let fromVar = fromVar toVarUnt Count 

        /// Create a `Count` with a preset `Variable`
        let create n vs min incr max un = 
            create n vs min incr max un 
            |> Count
            
        /// Create a `Count` with name **n**
        let count n = 
            let n = [name] |> List.append n |> Name.create
            let un = Units.Count.times
            createNew n un |> Count

        /// Turn a `Count` to a string
        let toString = toVarUnt >> (toString false)

        /// Print a `Count` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toVarUnt

        let getBaseValues = toVarUnt >> getBaseValues

        let getUnitValues = toVarUnt >> getUnitValues

    /// Type and functions that represent a quantity
    module Quantity =    
    
        /// String representation of the type
        let name = "Qty" 


        /// Turn `Quantity` in a `VariableUnit`
        let toVarUnt (Quantity qty) = qty

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits
        
        let toDto = toVarUnt >> Dto.toDto
        
        let fromDto dto = dto |> Dto.fromDto |> Quantity

        /// Set a `Quantity` with a `Variable`
        /// in a list fromVariable` lists
        let fromVar = fromVar toVarUnt Quantity 
        
        /// Create a `Quantity` with a preset `Variable`
        let create n vs min incr max un = 
            create n vs min incr max un 
            |> Quantity

        /// Create a `Quantity` with name **n**
        /// and `Unit` **un**
        let quantity n un = 
            let n = [name] |> List.append n |> Name.create
            
            createNew n un 
            |> Quantity 

        /// Set the name of the quantity `Variable` to **n**
        let setName n qty =
            let n = [n |> Name.toString; name] |> Name.create
            qty |> toVarUnt |> setName n |> Quantity

        /// Turn a `Quantity` to a string
        let toString = toVarUnt >> (toString false)

        /// Print a `Quantity` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toVarUnt

        let getBaseValues = toVarUnt >> getBaseValues

        let getUnitValues = toVarUnt >> getUnitValues


    /// Type and functions that represent a total,
    /// and a total is a quantity over a time period
    module Total =
    
        /// String representation of the type of the type
        let name = "Total" 


        /// Turn `Total` in a `VariableUnit`
        let toVarUnt (Total tot) = tot

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Total

        /// Set a `Total` with a `Variable`
        /// in a list fromVariable` lists
        let fromVar = fromVar toVarUnt Total 
        
        /// Create a `Total` with a preset `Variable`
        let create n vs min incr max un = 
            create n vs min incr max un 
            |> Total
        
        /// Create a `Total` with name **n**
        /// and `Unit` **un** and time unit **tu**
        let total n un tu = 
            let n = [name] |> List.append n |> Name.create
            
            match un with
            | ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un 
                |> ValueUnit.per tu
            |> createNew n 
            |> Total

        /// Set the name of the total `Variable` to **n**
        let setName n tot =
            let n = [n |> Name.toString; name] |> Name.create
            tot |> toVarUnt |> setName n |> Total

        /// Turn a `Total` to a string
        let toString = toVarUnt >> (toString false)

        /// Print a `Total` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toVarUnt

        let getBaseValues = toVarUnt >> getBaseValues

        let getUnitValues = toVarUnt >> getUnitValues



    /// Type and functions that represent a rate,
    /// and a rate is a quantity per time
    module Rate =
    
        /// String representation of the type
        let name = "Rate" 


        /// Turn `Rate` in a `VariableUnit`
        let toVarUnt (Rate rate) = rate

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Rate

        /// Set a `Rate` with a `Variable`
        /// in a list fromVariable` lists
        let fromVar = fromVar toVarUnt Rate 

        /// Create a `Rate` with a preset `Variable`
        let create n vs min incr max un = 
            create n vs min incr max un 
            |> Rate
                
        /// Create a `Rate` with name **n**
        /// and `Unit` **un** per time unit **tu**
        let rate n un1 un2 = 
            let n = [name] |> List.append n |> Name.create
            
            match un1, un2 with
            | ValueUnit.NoUnit, _ 
            | _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un1
                |> ValueUnit.per un2
            |> createNew n 
            |> Rate

        /// Set the name of the rate `Variable` to **n**
        let setName n rte =
            let n = [n |> Name.toString; name] |> Name.create
            rte |> toVarUnt |> setName n |> Rate

        /// Turn a `Rate` to a string
        let toString = toVarUnt >> (toString false)

        /// Print a `Rate` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toVarUnt
        
        let getBaseValues = toVarUnt >> getBaseValues

        let getUnitValues = toVarUnt >> getUnitValues



    /// Type and functions that represent a concentration,
    /// and a concentration is a quantity per time
    module Concentration =
    
        /// String representation of the type
        let name = "Conc" 


        /// Turn `Concentration` in a `VariableUnit`
        let toVarUnt (Concentration conc) = conc

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Concentration

        /// Set a `Concentration` with a `Variable`
        /// in a list fromVariable` lists
        let fromVar = fromVar toVarUnt Concentration 
        
        /// Create a `Concentration` with a preset `Variable`
        let create n vs min incr max un = 
            un
            |> create n vs min incr max
            |> Concentration
                
        /// Create a `Concentration` with name **n**
        /// and `Unit` **un** per shape unit **su**
        let conc n un su = 
            let n = [name] |> List.append n |> Name.create
            
            match un, su with
            | ValueUnit.NoUnit, _
            | _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per su
            |> createNew n
            |> Concentration

        /// Turn a `Concentration` to a string
        let toString = toVarUnt >> (toString false)

        /// Print a `Concentration` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toVarUnt

        let getBaseValues = toVarUnt >> getBaseValues

        let getUnitValues = toVarUnt >> getUnitValues


    /// Type and functions that represent a adjusted quantity,
    /// and a adjusted quantity is a quantity per time
    module QuantityAdjust =    
    
        /// String representation of the type
        let name = "QtyAdjust"


        /// Turn `QuantityAdjust` in a `VariableUnit`
        let toVarUnt (QuantityAdjust qty) = qty

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> QuantityAdjust

        /// Set a `QuantityAdjust` with a `Variable`
        /// in a list fromVariable` lists
        let fromVar = fromVar toVarUnt QuantityAdjust 

        /// Create a `QuantityAdjust` with a preset `Variable`
        let create n vs min incr max un = 
            un
            |> create n vs min incr max  
            |> QuantityAdjust
        
        /// Create a `QuantityAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj**
        let quantityAdjust n un adj = 
            let n = [name] |> List.append n |> Name.create

            match un, adj with
            | ValueUnit.NoUnit, _ 
            | _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
            |> createNew n 
            |> QuantityAdjust

        /// Set the name of the quantity adjust `Variable` to **n**
        let setName n qty =
            let n = [n |> Name.toString; name] |> Name.create
            qty |> toVarUnt |> setName n |> QuantityAdjust

        /// Turn a `QuantityAdjust` to a string
        let toString = toVarUnt >> (toString false)

        /// Print a `QuantityAdjust` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toVarUnt

        let getBaseValues = toVarUnt >> getBaseValues

        let getUnitValues = toVarUnt >> getUnitValues


    /// Type and functions that represent a adjusted total,
    /// and a adjusted total is a quantity per time
    module TotalAdjust =
    
        /// String representation of the type
        let name = "TotalAdjust" 


        /// Turn `TotalAdjust` in a `VariableUnit`
        let toVarUnt (TotalAdjust tot) = tot

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> TotalAdjust

        /// Set a `TotalAdjust` with a `Variable`
        /// in a list fromVariable` lists
        let fromVar = fromVar toVarUnt TotalAdjust 

        /// Create a `TotalAdjust` with a preset `Variable`
        let create n vs min incr max un = 
            un 
            |> create n vs min incr max  
            |> TotalAdjust
                
        /// Create a `TotalAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj** per time unit **tu**
        let totalAdjust n un adj tu = 
            let n = [name] |> List.append n |> Name.create

            match un, adj, tu with
            | ValueUnit.NoUnit, _, _ 
            | _, ValueUnit.NoUnit, _ 
            | _, _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
                |> ValueUnit.per tu
            |> createNew n 
            |> TotalAdjust

        /// Set the name of the total adjust `Variable` to **n**
        let setName n tot =
            let n = [n |> Name.toString; name] |> Name.create
            tot |> toVarUnt |> setName n |> TotalAdjust

        /// Turn a `TotalAdjust` to a string
        let toString = toVarUnt >> (toString false)

        /// Print a `TotalAdjust` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toVarUnt

        let getBaseValues = toVarUnt >> getBaseValues

        let getUnitValues = toVarUnt >> getUnitValues



    /// Type and functions that represent a adjusted rate,
    /// and a adjusted rate is a quantity per time unit
    module RateAdjust =

        /// String representation of the type
        let name = "RateAdjust" 


        /// Turn `RateAdjust` in a `VariableUnit`
        let toVarUnt (RateAdjust rate) = rate

        /// Set a `RateAdjust` with a `Variable`
        /// in a list fromVariable` lists
        let fromVar = fromVar toVarUnt RateAdjust 

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits
        
        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> RateAdjust

        /// Create a `RateAdjust` with a preset `Variable`
        let create n vs min incr max un =
            un
            |> create n vs min incr max
            |> RateAdjust
        
        /// Create a `RateAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj** per time unit **tu**
        let rateAdjust n un adj tu = 
            let n = [name] |> List.append n |> Name.create

            match un, adj, tu with
            | ValueUnit.NoUnit, _, _ 
            | _, ValueUnit.NoUnit, _ 
            | _, _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
                |> ValueUnit.per tu
            |> createNew n 
            |> RateAdjust

        /// Set the name of the rate adjust `Variable` to **n**
        let setName n rte =
            let n = [n |> Name.toString; name] |> Name.create
            rte 
            |> toVarUnt 
            |> setName n 
            |> RateAdjust

        /// Turn a `RateAdjust` to a string
        let toString = toVarUnt >> (toString false)

        /// Print a `RateAdjust` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toVarUnt

        let getBaseValues = toVarUnt >> getBaseValues

        let getUnitValues = toVarUnt >> getUnitValues



    /// Type and functions that represent a dose,
    /// and a dose is a dose quantity, total and rate
    module Dose =

        module QT = Quantity
        module TL = Total
        module RT = Rate


        /// Create a `Dose` 
        let create qty tot rte = (qty, tot, rte) |> Dose

        let get (Dose(qty, tot, rte)) = qty, tot, rte

        let getQuantity = get >> (fun (qty, _, _) -> qty)

        let getTotal = get >> (fun (_, tot, _) -> tot)

        let getRate = get >> (fun (_, _, rte) -> rte)

        /// Turn `Dose` in a dose quantity, total and rate `VariableUnit`
        let getQty = get >> (fun (qty, _, _) -> qty)
        let toVarUnt (Dose(qty, total, rate)) = 
            qty |> QT.toVarUnt, total |> TL.toVarUnt, rate |> RT.toVarUnt

        let toDto = 
            toVarUnt
            >> (fun (q, t, r) -> q |> Dto.toDto, t |> Dto.toDto, r |> Dto.toDto)

        let fromDto (q, t, r) =
            (q |> Dto.fromDto |> Quantity ,
             t |> Dto.fromDto |> Total ,
             r |> Dto.fromDto |> Rate) |> Dose

        /// Set a `Dose` with a quantity, total and rate `Variable` 
        /// in a list of `Variable` lists
        let fromVar eqs (Dose(qty, tot, rte)) = 
            let qty = fromVar QT.toVarUnt Quantity eqs qty
            let tot = fromVar TL.toVarUnt Total    eqs tot
            let rte = fromVar RT.toVarUnt Rate     eqs rte
            (qty, tot, rte) |> Dose

        /// Create a `Dose` with name **n**
        /// and `Unit` **un** per time unit **tu**
        let dose n un ttu rtu = 
            let qty   = QT.quantity n un
            let total = TL.total    n un ttu
            let rate  = RT.rate     n un rtu

            (qty, total, rate) 
            |> Dose 

        /// Get the common name of a `Dose`
        let getName (Dose(qty, _, _)) =
            qty 
            |> QT.toVarUnt 
            |> getName 
            |> Name.toString 
            |> String.split "." 
            |> List.head


        /// Set the `Name` **n** to the dose `Variable`s
        let setName n (Dose(qty, tot, rte)) =
            (
                qty |> QT.setName n,
                tot |> TL.setName n,
                rte |> RT.setName n
            ) |> Dose
            

        /// Turn a `Dose` to a string
        let toString (Dose(qty, tot, rte))  =
            [
                qty |> QT.toString
                tot |> TL.toString
                rte |> RT.toString
            ]


    /// Type and functions that represent an adjusted dose,
    /// and a dose is an adjusted dose quantity, total and rate
    module DoseAdjust =

        module QT = QuantityAdjust
        module TL = TotalAdjust
        module RT = RateAdjust


        /// Create a `DoseAdjust` 
        let create qty tot rte = (qty, tot, rte) |> DoseAdjust

        let get (DoseAdjust(qty, tot, rte)) = qty, tot, rte

        let getQuantity = get >> (fun (qty, _, _) -> qty)

        let getTotal = get >> (fun (_, tot, _) -> tot)

        let getRate = get >> (fun (_, _, rte) -> rte)

        /// Turn `DoseAdjust` in an adjusted quantity, total and rate `VariableUnit`
        let toVarUnt (DoseAdjust(qty, total, rate)) = 
            qty |> QT.toVarUnt, total |> TL.toVarUnt, rate |> RT.toVarUnt

        let toDto = 
            toVarUnt
            >> (fun (q, t, r) -> q |> Dto.toDto, t |> Dto.toDto, r |> Dto.toDto)

        let fromDto (q, t, r) =
            (q |> Dto.fromDto |> QuantityAdjust ,
             t |> Dto.fromDto |> TotalAdjust ,
             r |> Dto.fromDto |> RateAdjust) |> DoseAdjust

        /// Set a `DoseAdjust` with an adjusted quantity, total and rate `Variable` 
        /// in a list of `Variable` lists
        let fromVar eqs (DoseAdjust(qty, tot, rte)) = 
            let qty = fromVar QT.toVarUnt QuantityAdjust eqs qty
            let tot = fromVar TL.toVarUnt TotalAdjust    eqs tot
            let rte = fromVar RT.toVarUnt RateAdjust     eqs rte
            (qty, tot, rte) |> DoseAdjust

        /// Create a `DoseAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj** per time unit **tu**
        let doseAdjust n un adj ttu rtu = 
            let qty   = QT.quantityAdjust n un adj
            let total = TL.totalAdjust    n un adj ttu
            let rate  = RT.rateAdjust     n un adj rtu

            (qty, total, rate) 
            |> DoseAdjust

        /// Get the common name of a `DoseAdjust`
        let getName (DoseAdjust(qty, _, _)) =
            qty 
            |> QT.toVarUnt 
            |> getName 
            |> Name.toString 
            |> String.split "." 
            |> List.head

        /// Set the `Name` **n** to the dose `Variable`s
        let setName n (DoseAdjust(qty, tot, rte)) =
            (
                qty |> QT.setName n,
                tot |> TL.setName n,
                rte |> RT.setName n
            ) |> DoseAdjust

        /// Turn a `DoseAdjust` to a string
        let toString (DoseAdjust(qty, tot, rte))  =
            [
                qty |> QT.toString
                tot |> TL.toString
                rte |> RT.toString
            ]

