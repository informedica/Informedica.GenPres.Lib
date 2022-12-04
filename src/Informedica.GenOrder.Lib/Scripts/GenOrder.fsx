
#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: FParsec"


#r "../../Informedica.Utils.Lib/bin/Debug/net6.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.GenUnits.Lib/bin/Debug/net6.0/Informedica.GenUnits.Lib.dll"
#r "../../Informedica.GenSolver.Lib/bin/Debug/net6.0/Informedica.GenSolver.Lib.dll"


open Microsoft.FSharp.Core

[<AutoOpen>]
module Types =

    open System
    open MathNet.Numerics
    open Informedica.GenUnits.Lib
    open Informedica.GenSolver.Lib.Types

    type Unit = ValueUnit.Unit


    /// A `VariableUnit` is the combination of
    /// an `Informedica.GenSolver.Lib.Variable` with
    /// an `Informedica.GenUnits.Lib.Unit`
    /// The `Variable` stores the base values according
    /// to the `Unit`
    type VariableUnit =
        {
            /// Stores the values/range
            Variable:  Variable
            /// Stores the unit
            Unit: Unit
        }


    /// Type that represents a frequency
    type Frequency = Frequency of VariableUnit


    /// Type that represents a time
    type Time = Time of VariableUnit


    /// Type that represents a count
    type Count = Count of VariableUnit


    /// Type that represents a quantity
    type Quantity = Quantity of VariableUnit


    /// Type that represents a total
    type Total = Total of VariableUnit


    /// Type that represents a rate
    type Rate = Rate of VariableUnit


    /// Type that represents a concentration
    type Concentration = Concentration of VariableUnit


    /// Type that represents a adjusted quantity
    type QuantityAdjust = QuantityAdjust of VariableUnit


    /// Type that represents a adjusted total
    type TotalAdjust = TotalAdjust of VariableUnit


    /// Type that represents a adjusted rate
    type RateAdjust = RateAdjust of VariableUnit


    /// Type that represents a dose quantity, total and rate
    type Dose = Dose of Quantity * Total * Rate


    /// Type that represents an adjusted dose quantity, total and rate
    type DoseAdjust = DoseAdjust of QuantityAdjust * TotalAdjust * RateAdjust

    /// An order equation is either a product equation or a
    /// sum equation
    type OrderEquation =
        | OrderProductEquation of VariableUnit * VariableUnit list
        | OrderSumEquation of VariableUnit * VariableUnit list

    /// An Id is represented by a string
    type Id = Id of string


    /// Models an `Item` in a `Component`
    type Item =
        {
            /// The id of the Order
            OrderId: Id
            /// The name of the item
            Name: Name
            /// The quantity of an `Item` in a `Component`
            ComponentQuantity: Quantity
            /// The quantity of an `Item` in an `Orderable`
            OrderableQuantity: Quantity
            /// The `Item` concentration in a `Component`
            ComponentConcentration: Concentration
            /// The  `Item` concentration in an `Orderable`
            OrderableConcentration: Concentration
            /// The `Item` `Dose`, i.e. quantity, total and rate of `Item` administered
            Dose: Dose
            // The `Item` `DoseAdjust`,  i.e. adjusted quantity, total and rate of `Item` administered
            DoseAdjust: DoseAdjust
        }



    /// Models in a `Component` in and `Orderable`
    type Component =
        {
            /// The id of a `Component`
            OrderId: Id
            /// The name of a `Component`
            Name: Name
            /// The quantity of a `Component`
            ComponentQuantity: Quantity
            /// The quantity of a `Component` in an `Orderable`
            OrderableQuantity: Quantity
            /// The count of a `Component` in an `Orderable`
            OrderableCount: Count
            /// The quantity of a `Component` in an `Order`
            OrderQuantity: Quantity
            /// The count of a `Component` in an `Order`
            OrderCount: Count
            /// The concentration of a `Component` in an `Orderable`
            OrderableConcentration: Concentration
            // The `Component` `Dose`,
            /// i.e. quantity, total and rate of `Component` administered
            Dose: Dose
            // The `Component` `DoseAdjust`,
            /// i.e. adjusted quantity, total and rate of `Component` administered
            DoseAdjust: DoseAdjust
            /// The `Item`s in a `Component`
            Items: Item list
        }


    /// Models an `Orderable`
    type Orderable =
        {
            /// The order id of
            OrderId: Id
            /// The name of the orderable
            Name: Name
            // The shape of an orderable
            Shape : string
            /// The quantity of an orderable
            OrderableQuantity: Quantity
            /// The quantity of an orderable in an order
            OrderQuantity: Quantity
            /// The orderable count in an order
            OrderCount: Count
            // The count of doses in an orderable quantity
            DoseCount: Count
            /// The dose of an orderable
            Dose: Dose
            /// The adjusted dose of an orderable
            DoseAdjust: DoseAdjust
            /// The list of components in an orderable
            Components: Component list
        }


    /// There is always a `Start` or
    /// both a `StartStop`
    type StartStop =
        | Start of DateTime
        | StartStop of DateTime * DateTime


    /// Models an order
    type Order =
        {
            /// The id of an order
            Id: Id
            /// Used to adjust doses
            Adjust: Quantity
            /// That what can be ordered
            Orderable: Orderable
            /// How the orderable is prescribed
            Prescription: Prescription
            /// The route of administration of the order
            Route: string // Route.T
            /// The start stop date of the order
            StartStop: StartStop
        }

    /// Type that represents a prescription
    and Prescription =
        /// A process
        | Process
        /// A continuous infusion
        | Continuous
        /// A discontinuous prescription with a frequency
        | Discontinuous of Frequency
        /// A discontinuous prescription with both frequency and time
        | Timed of Frequency * Time


    /// Mapping of an order to variables
    type OrderMapping =
        // Quantity of an Item in a Component
        | ItemComponentQuantity
        // Concentration of an Item in a Component
        | ItemComponentConcentration
        // Quantity of an Item in an Orderable
        | ItemOrderableQuantity
        // Concentration of an Item in an Orderable
        | ItemOrderableConcentration
        // Dose Quantity of an Item
        | ItemDoseQuantity
        // Dose Total of an Item
        | ItemDoseTotal
        // Dose Rate of an Item
        | ItemDoseRate
        // Dose Total of an Item in an Order
        | ItemDoseOrder
        // Adjusted Dose Quantity of an Item
        | ItemDoseQuantityAdjust
        // Adjusted Dose Total of an Item
        | ItemDoseTotalAdjust
        // Adjusted Dose Rate of an Item
        | ItemDoseRateAdjust
        // Adjusted Total Dose Rate of an Item in an Order
        | ItemDoseOrderAdjust
        // Quantity of a Component
        | ComponentComponentQuantity
        // Quantity of a Component in an Orderable
        | ComponentOrderableQuantity
        // Concentration of a Component in an Orderable
        | ComponentOrderableConcentration
        // Amount of Components in an Orderable
        | ComponentOrderableCount
        // Quantity of Component in an Order
        | ComponentOrderQuantity
        // Amount of Components in an Order
        | ComponentOrderCount
        // Dose Quantity of a Component
        | ComponentDoseQuantity
        // Dose Total of a Component
        | ComponentDoseTotal
        // Dose Rate of a Component
        | ComponentDoseRate
        // Adjusted Dose Quantity of a Component
        | ComponentDoseQuantityAdjust
        // Adjusted Dose Total of a Component
        | ComponentDoseTotalAdjust
        // Adjusted Dose Rate of a Component
        | ComponentDoseRateAdjust
        // Quantity of an Orderable
        | OrderableOrderableQuantity
        // Amount of Dose Quantity in an Orderable Quantity
        | OrderableDoseCount
        // Dose Quantity of an Orderable
        | OrderableDoseQuantity
        // Dose Total of an Orderable
        | OrderableDoseTotal
        // Dose Rate of an Orderable
        | OrderableDoseRate
        // Adjusted Dose Quantity of an Orderable
        | OrderableDoseQuantityAdjust
        // Adjusted Dose Total of an Orderable
        | OrderableDoseTotalAdjust
        // Adjusted Dose Rate of an Orderable
        | OrderableDoseAdjustRate
        // Prescription Frequency
        | OrderPresFreq
        // Prescription Time
        | OrderPresTime
        // Quantity of an Orderable in an Order
        | OrderableOrderQuantity
        // Amount of Orderables in an Order
        | OrderableOrderCount
        // Order Adjust Quantity
        | OrderAdjustQty


    /// The different possible order types
    type OrderType =
        | AnyOrder
        | ProcessOrder
        | ContinuousOrder
        | DiscontinuousOrder
        | TimedOrder


    /// Relation between shape and route
    type RouteShape =
        | AnyRouteShape
        | IntravenousFluid
        | OralFluid
        | OralSolid
        | RectalSolid

    /// Constrained that can be applied to a
    /// variable in an equation.
    type DrugConstraint =
       {
            Name : string
            Mapping : OrderMapping
            Property : Property
            RouteShape : RouteShape
            OrderType : OrderType
        }

    /// The representation of a drug order that
    /// can be derived by a drug product inventory
    /// and the related dose rule
    type DrugOrder =
        {
            /// Identifies the specific drug order
            Id:  string
            /// The name of the order
            Name : string
            /// The list of drug products that can be used for the order
            Products : ProductComponent list
            /// The quantities of the drug order
            Quantities :  BigRational list
            /// The unit the `DrugOrder` is measured in,
            /// i.e. of the `Quantities`
            Unit : string
            /// The time unit to be used when using a frequency
            TimeUnit : string
            /// The time unit to be used when using a rate
            RateUnit : string
            /// The shape of the products
            Shape : string
            /// The route by which the order is applied
            Route : string
            // The type of order
            OrderType : OrderType
        }
    /// The product components that are used by the drug order
    and ProductComponent =
        {
            /// The name of the product
            Name : string
            /// The quantities of the product
            /// Note: measured in the same unit as
            /// the `DrugOrder` unit
            Quantities : BigRational list
            /// The "divisibility" of the products
            Divisible : BigRational
            /// The time unit used for frequency
            TimeUnit : string
            /// The time unit used for rate
            RateUnit : string
            /// The list of substances contained in the product
            Substances: SubstanceItem list
        }
    and SubstanceItem =
        {
            /// The name of the substance
            Name : string
            /// The possible concentrations of the substance
            /// in the products
            Concentrations : BigRational list
            /// The possible quantities of the substance in the orderable
            OrderableQuantities : BigRational list
            /// The unit by which the substance is
            /// measured.
            Unit : string
            /// The unit used for the dose
            DoseUnit : string
            /// The time unit used for the frequency
            TimeUnit : string
            /// The time unit used for the rate
            RateUnit : string
        }

    /// The constraints that can be applied
    /// and the order
    type ConstrainedOrder = DrugConstraint list * Order

    /// The dose limits that can be applied
    type DoseLimit =
        {
            /// The substance name to which the dose limits
            /// are applied
            SubstanceName : string
            /// maps to ItemDoseQty
            MinDoseQuantity : BigRational option
            /// maps to ItemDoseQty
            MaxDoseQuantity : BigRational option
            /// maps to ItemDoseAdjustQtyAdjust
            MinDoseQuantityAdjust : BigRational option
            /// maps to ItemDoseAdjustQtyAdjust
            MaxDoseQuantityAdjust : BigRational option
            /// maps to ItemDoseTotal
            MinDoseTotal : BigRational option
            /// maps to ItemDoseTotal
            MaxDoseTotal : BigRational option
            /// maps to ItemDoseAdjustTotalAdjust
            MinDoseTotalAdjust : BigRational option
            /// maps to ItemDoseAdjustTotalAdjust
            MaxDoseTotalAdjust : BigRational option
            /// maps to ItemDoseRate
            MinDoseRate : BigRational option
            /// maps to ItemDoseRate
            MaxDoseRate : BigRational option
            /// maps to ItemDoseAdjustRateAdjust
            MinDoseRateAdjust : BigRational option
            /// maps to ItemDoseAdjustRateAdjust
            MaxDoseRateAdjust : BigRational option
        }


    type DoseRule =
        {
            // selector properties
            Indication : string
            Medication : string
            Shape : string
            Route : string
            Age : BigRational option * BigRational option
            Weight : BigRational option * BigRational option
            GestAge : BigRational option * BigRational option
            PostAge : BigRational option * BigRational option
            /// maps to OrderTyp
            OrderType : OrderType
            /// maps to PresFreq
            Frequencies : BigRational list
            /// maps to OrderableDoseRate
            Rates : BigRational list
            /// maps to PresTime
            MinTime : BigRational option
            /// maps to PresTime
            MaxTime : BigRational option
            DoseUnit : string
            AdjUnit : string
            TimeUnit : string
            RateUnit : string
            Limits : DoseLimit list
        }


    type SolutionLimit =
        {
            SubstanceName : string
            /// maps to ItemOrderableQty
            Quantities : BigRational list
            /// maps to ItemOrderableConc
            MinConcentration : BigRational option
            /// maps to ItemOrderableConc
            MaxConcentration : BigRational option
        }


    type SolutionRule =
        {
            Medication : string
            Age : BigRational option * BigRational option
            Weight : BigRational option * BigRational option
            Solutions : string list
            /// maps to OrderableOrderableQty
            Quantities : BigRational list
            RateUnit : string
            /// maps to OrderableDoseCount
            DoseCount : BigRational list
            Limits : SolutionLimit list
        }


    type Substance =
        {
            Name : string
            Unit : string
            Quantities : BigRational list
            Concentrations : BigRational list
        }


    type Product =
        {
            Name : string
            Shape : string
            Unit : string
            Divisible : BigRational option
            Quantities : BigRational list
            Substances : Substance list
        }


    type Scenario =
        {
            No : int
            Indication : string
            Name : string
            Shape : string
            Route : string
            Prescription : string
            Preparation : string
            Administration : string
        }


    module Exceptions =

        type Message =
            | OrderCouldNotBeSolved of string * Order


    module Events =

        type Event =
            | SolverReplaceUnit of (Name * Unit)
            | OrderSolveStarted of Order
            | OrderSolveFinished of Order
            | OrderSolveConstraintsStarted of Order * Constraint list
            | OrderSolveConstraintsFinished of Order * Constraint list
            | OrderScenario of string
            | OrderScenarioWithNameValue of Order * Name * BigRational


    module Logging =

        open Informedica.GenSolver.Lib.Types.Logging

        type OrderMessage =
            | OrderException of Exceptions.Message
            | OrderEvent of Events.Event
            interface IMessage



[<AutoOpen>]
module Utils =

    open System
    open System.IO
    open System.Net.Http

    open Informedica.Utils.Lib.BCL


    module Csv =


        type DataType =
            | StringData
            | FloatData
            | FloatOptionData

        let tryCast dt (x: string) =
            match dt with
            | StringData -> box (x.Trim())
            | FloatData ->
                match Double.TryParse(x) with
                | true, n -> n |> box
                | _ ->
                    $"cannot parse {x} to double"
                    |> failwith
            | FloatOptionData ->
                match Double.TryParse(x) with
                | true, n -> n |> Some |> box
                | _ -> None |> box


        let getColumn dt columns sl s =
            columns
            |> Array.tryFindIndex ((=) s)
            |> function
                | None ->
                    $"""cannot find column {s} in {columns |> String.concat ", "}"""
                    |> failwith
                | Some i ->
                    sl
                    |> Array.item i
                    |> tryCast dt


        let getStringColumn columns sl s =
            getColumn StringData columns sl s |> unbox<string>


        let getFloatColumn columns sl s =
            getColumn FloatData columns sl s |> unbox<float>


        let getFloatOptionColumn columns sl s =
            getColumn FloatOptionData columns sl s
            |> unbox<float option>


        let parseCSV (s: string) =
            s.Split("\n")
            |> Array.filter (String.isNullOrWhiteSpace >> not)
            |> Array.map (String.replace "\",\"" "|")
            |> Array.map (String.replace "\"" "")
            |> Array.map (fun s ->
                s.Split("|")
                |> Array.map (fun s -> s.Trim())
            )


    module Web =

        let createUrl sheet id =
            $"https://docs.google.com/spreadsheets/d/{id}/gviz/tq?tqx=out:csv&sheet={sheet}"

        //https://docs.google.com/spreadsheets/d/1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g/edit?usp=sharing
        [<Literal>]
        let dataUrlId = "1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g"

        let client = new HttpClient()

        let download url =
            async {
                use! resp = client.GetAsync(Uri(url)) |> Async.AwaitTask
                use! stream = resp.Content.ReadAsStreamAsync() |> Async.AwaitTask
                use reader = new StreamReader(stream)
                return reader.ReadToEnd()
            }


        let getDataFromSheet sheet =
            createUrl sheet dataUrlId
            |> download
            |> Async.RunSynchronously
            |> Csv.parseCSV



module Logging =

    open System

    module SolverLogging = Informedica.GenSolver.Lib.Logging
    module LoggingType = Informedica.GenSolver.Lib.Types.Logging

    let private log level (logger : LoggingType.Logger) msg =
        msg
        |> fun m ->
            {
                LoggingType.TimeStamp = DateTime.Now
                LoggingType.Level = level
                LoggingType.Message = m
            }
            |> logger.Log


    let logInfo logger msg =
        msg
        |> Logging.OrderEvent
        |> log LoggingType.Informative logger


    let logWarning logger msg =
        msg
        |> Logging.OrderEvent
        |> log LoggingType.Warning logger

    let logError (logger : LoggingType.Logger) msg =
        msg
        |> Logging.OrderException
        |> log LoggingType.Error logger



module Exceptions =

        /// Equation exception
        exception OrderException of Exceptions.Message

        /// Raise an `EquationException` with `Message` `m`.
        let raiseExc log m o =
            match log with
            | Some log ->
                printfn $"logging error {m}"
                (m, o)
                |> Exceptions.OrderCouldNotBeSolved
                |> Logging.logError log

            | None -> ()

            (m, o) |> Exceptions.OrderCouldNotBeSolved |> OrderException |> raise




/// Types and functions to deal with
/// value primitives
[<AutoOpen>]
module WrappedString =

    open Informedica.Utils.Lib.BCL

    /// Type and functions that
    /// deal with an identifier
    module Id =

        let create s = s |> Id

        let lift f = fun (Id s) -> s |> f |> create

        let toString (Id s) = s

    /// Helper functions for `Informedica.GenSolver.Variable.Name` type
    module Name =

        open Informedica.GenSolver.Lib

        module Name = Variable.Name

        [<Literal>]
        let concatWith = "."

        /// Create a `Name` from a list of strings that
        let create ns = ns |> String.concat concatWith |> Name.createExc


        let toString  = Name.toString


        let toStringList = Name.toString >> (String.split concatWith)



/// Helper functions to
/// facilitate the use of the
/// `Informedica.GenUnits.Lib`
module ValueUnit =

    open MathNet.Numerics

    open Informedica.Utils.Lib.BCL
    open Informedica.GenUnits.Lib

    open ValueUnit


    let valueToBase u v =
        create u v
        |> toBase


    let unitToString =
        Units.toString Units.Dutch Units.Short


    let unitFromString s =
        if s |> String.isNullOrWhiteSpace then None
        else
            try
                // ugly hack need to fix this
                // in the units lib
                let s =
                    s
                    |> String.replace "x[Count]" "#"
                    |> String.replace "x" "/"
                    |> String.replace "#" "x[Count]"

                "1 " + s
                |> fromString
                |> get
                |> snd
                |> Some
            with
            | _ ->
                printfn $"could not parse to unit: %s{s}"
                None


    let calcUnit op u1 u2 =

        match u1, u2 with
        | NoUnit, _
        | _, NoUnit -> NoUnit
        | u1, u2 ->
            let vu1 = 1N |> create u1
            let vu2 = 1N |> create u2

            vu1 |> op <| vu2
            |> get
            |> snd


    module Units =

        let noUnit = NoUnit



module ValueRange =

    open Informedica.GenUnits.Lib
    open Informedica.GenSolver.Lib.Variable.ValueRange


    /// Convert a `ValueRange` to a `string`.
    let toStringWithUnit exact un vr =
        let toUnit = ValueUnit.create un >> ValueUnit.toUnit

        let fVs vs =
            vs
            |> ValueSet.map toUnit
            |> Some
            |> print exact None None None

        let unr = print exact None None None None

        let print min incr max = print exact min incr max None

        let fMin min =
            print (min |> Minimum.map toUnit toUnit |> Some) None None

        let fMax max =
            print None None (max |> Maximum.map toUnit toUnit |> Some)

        let fMinMax (min, max) =
            print
                (min |> Minimum.map toUnit toUnit |> Some)
                None
                (max |> Maximum.map toUnit toUnit |> Some)

        let fIncr incr =
            print None (incr |> Increment.map toUnit |> Some) None

        let fMinIncr (min, incr) =
            print
                (min |> Minimum.map toUnit toUnit |> Some)
                (incr |> Increment.map toUnit |> Some)
                None

        let fIncrMax (incr, max) =
            print
                None
                (incr |> Increment.map toUnit |> Some)
                (max |> Maximum.map toUnit toUnit |> Some)

        let fMinIncrMax (min, incr, max) =
            print
                (min |> Minimum.map toUnit toUnit |> Some)
                (incr |> Increment.map toUnit |> Some)
                (max |> Maximum.map toUnit toUnit |> Some)

        vr |> apply unr fMin fMax fMinMax fIncr fMinIncr fIncrMax fMinIncrMax fVs



/// Functions that deal with the `VariableUnit` type
module VariableUnit =

    open MathNet.Numerics

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL

    open Informedica.GenSolver.Lib.Types

    module Variable   = Informedica.GenSolver.Lib.Variable
    module ValueRange = Variable.ValueRange
    module Minimum    = ValueRange.Minimum
    module Maximum    = ValueRange.Maximum
    module Increment  = ValueRange.Increment
    module ValueSet   = ValueRange.ValueSet
    module Equation   = Informedica.GenSolver.Lib.Equation

    module VariableDto = Variable.Dto
    module ValueUnit = Informedica.GenUnits.Lib.ValueUnit
    module Units = ValueUnit.Units

    type Unit = ValueUnit.Unit


    /// Create a `VariableUnit` with preset values
    let create n min incr max vs un =
        ValueRange.create true min incr max vs
        |> fun vlr ->
            let var = Variable.create id n vlr
            { Variable = var; Unit = un }


    /// Create a new `VariableUnit` with
    /// `Name` **nm** and `Unit` **un**
    let createNew n un = create n None None None None un


    /// Create a `VariableUnit` with
    /// `Variable` **var** and `Unit` **un**
    let withVar un var =
        { Variable = var; Unit = un }


    /// Apply **f** to `VariableUnit` **vru**
    let apply f (vru: VariableUnit) = vru |> f


    /// Utility function to facilitate type inference
    let get = apply id


    /// Get all record fields from a `VariableUnit`
    let getMembers { Variable = var; Unit = un } =
        var, un


    /// Get the `Variable` from a `VariableUnit`
    let getVar = getMembers >> fst


    /// Get the `Variable.Name` from a `VariableUnit` **vru**
    let getName vru = (vru |> getVar).Name


    /// Get the `Unit` from a `VariableUnit`
    let getUnit = getMembers >> snd


    let hasUnit = getUnit >> ((<>) ValueUnit.NoUnit)


    /// Try find the first `VariableUnit` with
    /// a specific `Name` in a list of lists
    let tryFindVarUnt = List.tryFindFirst getName


    /// Set a specific `VariableUnit` with
    /// a `Variable` from a
    /// list of `Variable` lists **vrll**
    /// that has the same name as **vru**.
    /// Return the unmodified **vru** if
    /// no `Variable` can be found.
    /// **c** is used to construct the specific
    /// variable and **toVar** to extract the
    /// current variable from **vru**
    let fromVar toVru c eqs a =
        let n =
            a
            |> toVru
            |> getName

        eqs
        |> tryFindVarUnt n
        |> Option.map c
        |> Option.defaultValue a


    /// Set the 'Name' to the `Variable` of the `VariableUnit`
    let setName nm vru =
        { vru with
            Variable = vru.Variable |> Variable.setName nm
        }

    let setUnit u vru : VariableUnit =
        { vru with Unit = u }

    let valueToBase v vru =
        v |> ValueUnit.valueToBase (vru |> getUnit)


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
        >> Variable.ValueRange.getValSet
        >> Option.map Variable.ValueRange.ValueSet.toSet
        >> Option.defaultValue Set.empty


    let getUnitValues vru =
        vru
        |> getBaseValues
        |> Seq.map (fun vs ->
            vs, vru |> getUnit
        )
        |> Seq.map (fun (v, u) ->
            v
            |> ValueUnit.valueToUnit u
        )


    let containsBaseValue v =
        getVar
        >> Variable.getValueRange
        >> ValueRange.contains v


    let containsUnitValue v vru =
        let u = vru |> getUnit

        vru
        |> getVar
        |> Variable.getValueRange
        |> ValueRange.contains (v |> ValueUnit.valueToBase u)


    let toValueUnitStringList get n x =
        x
        |> get
        |> getVar
        |> Variable.getValueRange
        |> Variable.ValueRange.getValSet
        |> function
        | Some (ValueSet vs) ->
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
                    |> fun us -> v, $"%s{vs} %s{us}"
            )
        | None -> Seq.empty


    let getUnits vu =
        (vu |> get).Unit
        |> ValueUnit.getUnits


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
            member val Min : BigRational option = None with get, set
            member val MinIncl = false with get, set
            member val Incr : BigRational list = [] with get, set
            member val Max : BigRational option = None with get, set
            member val MaxIncl = false with get, set
            member val Vals : BigRational list = [] with get, set

        let dto () = Dto ()

        let fromDto (dto: Dto) =
            let un =
                if dto.Unit |> String.isNullOrWhiteSpace then ValueUnit.NoUnit
                else
                    dto.Unit
                    |> ValueUnit.unitFromString
                    |> Option.defaultValue ValueUnit.NoUnit

            let toBase =
                ValueUnit.create un
                >> ValueUnit.toBase

            let n    = [ dto.Name ] |> Name.create
            let vals =
                dto.Vals
                |> List.map toBase
                |> function
                | [] -> None
                | xs -> xs |> Set.ofList |> ValueSet.create |> Some

            let incr =
                dto.Incr
                |> List.map toBase
                |> function
                | [] -> None
                | xs -> xs |> Set.ofList |> Increment.create |> Some


            let min  = dto.Min  |> Option.map  (toBase >> Minimum.create  dto.MinIncl)
            let max  = dto.Max  |> Option.map  (toBase >> Maximum.create  dto.MaxIncl)

            create n min incr max vals un

        let toDto (vu : VariableUnit) =
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
                    |> Minimum.toBigRational
                    |> ValueUnit.valueToUnit vu.Unit
                    |> Some, m |> Minimum.isIncl
                | None -> None, false
            let max, inclMax =
                vr
                |> ValueRange.getMax
                |> function
                | Some m ->
                    m
                    |> Maximum.toBigRational
                    |> ValueUnit.valueToUnit vu.Unit
                    |> Some,
                    m |> Maximum.isIncl
                | None -> None, false

            dto.Name <-
                vu |> getName |> Name.toString
            dto.Unit <-
                vu |> getUnit |> ValueUnit.unitToString
            dto.Vals <-
                vr
                |> ValueRange.getValSet
                |> Option.map (ValueSet.toSet >> Set.toList)
                |> Option.map (List.map (ValueUnit.valueToUnit vu.Unit))
                |> Option.defaultValue []
            dto.Incr <-
                vr
                |> ValueRange.getIncr
                |> Option.map Increment.toList
                |> Option.map (List.map (ValueUnit.valueToUnit vu.Unit))
                |> Option.defaultValue []

            dto.Min <- min
            dto.MinIncl <- inclMin
            dto.Max <- max
            dto.MaxIncl <- inclMax

            dto


    /// Type and functions that represent a frequency
    module Frequency =


        /// String representation of the type
        let [<Literal>] name = "Freq"


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
        let [<Literal>] name = "Time"


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
        let [<Literal>] name = "Count"


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
        let [<Literal>] name = "Qty"


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
        let [<Literal>] name = "Total"


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
        let [<Literal>] name = "Rate"


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
        let [<Literal>] name = "Conc"


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


        /// Create a `Concentration` with name **n**
        /// and `Unit` **un** per shape unit **su**
        let concentration n un su =
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
        let [<Literal>] name = "QtyAdjust"


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
        let [<Literal>] name = "TotalAdjust"


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
        let [<Literal>] name = "RateAdjust"


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
            |> Name.toStringList
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
            |> Name.toStringList
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



/// Helper functions to
/// facilitate the use of the
/// `Informedica.GenSolver.Lib`
module Solver =

    open Informedica.Utils.Lib
    open Informedica.GenUnits.Lib
    open Informedica.GenSolver.Lib.Types

    module Variable = Informedica.GenSolver.Lib.Variable
    module Name = Variable.Name
    module ValueRange = Variable.ValueRange
    module Property = ValueRange.Property
    module Equation = Informedica.GenSolver.Lib.Equation
    module Solver = Informedica.GenSolver.Lib.Solver
    module Api = Informedica.GenSolver.Lib.Api


    let orderProdEq = function
    | h::tail -> (h, tail) |> OrderProductEquation
    | _ -> "not a valid product equation" |> failwith


    let orderSumEq = function
    | h::tail -> (h, tail) |> OrderSumEquation
    | _ -> "not a valid sum equation" |> failwith


    /// Create an `Equation` using a constructor **cr**
    /// a result `VariableUnit` **y** and a list of
    /// `VariableUnit` list **xs**
    let toEq cr y xs =
        (y |> VariableUnit.getVar, xs |> List.map VariableUnit.getVar)
        |> cr


    /// Create a `ProdEquation` from `VariableUnit`s
    let toSolverProdEq succ fail y xs =
        toEq (Equation.createProductEq succ fail) y xs


    /// Create a `SumEquation` from `VariableUnit`s
    let toSolverSumEq succ fail y xs =
        toEq (Equation.createSumEq succ fail) y xs


    let mapToSolverEqs =
        List.fold (fun acc eq ->
            match eq with
            | OrderProductEquation (y, xs) -> toSolverProdEq id (string >> exn >> raise) y xs
            | OrderSumEquation     (y, xs) -> toSolverSumEq id  (string >> exn >> raise) y xs
            |> List.singleton
            |> List.append acc
        ) []


    let replaceUnit log n u eqs =
        (n, u)
        |> Events.SolverReplaceUnit
        |> Logging.logInfo log

        let repl c vru vrus =
            if vru |> VariableUnit.getName = n then
                (vru |> VariableUnit.setUnit u, vrus)
            else
                vru,
                vrus
                |> List.map (fun vru ->
                    if vru |> VariableUnit.getName = n then
                        vru
                        |> VariableUnit.setUnit u
                    else vru
                )
            |> c

        eqs
        |> List.map (fun e ->
            match e with
            | OrderSumEquation (vru, vrus) ->
                repl OrderSumEquation vru vrus
            | OrderProductEquation (vru, vrus) ->
                repl OrderProductEquation vru vrus
        )


    /// calculate the units for all variable units in
    /// all eqs
    let solveUnits log eqs =
        let hasUnit = VariableUnit.hasUnit
        let noUnit = hasUnit >> not

        let rec solve acc eqs =
            match eqs with
            | []      -> acc
            | h::tail ->
                match h with
                | OrderProductEquation (y, xs) ->
                    if y::xs  |> List.hasExactlyOne noUnit then
                        if y |> noUnit then
                            let y =
                                { y with
                                    Unit =
                                        xs
                                        |> List.map VariableUnit.getUnit
                                        |> List.reduce (ValueUnit.calcUnit (*))
                                }
                            let h = (y, xs) |> OrderProductEquation
                            // start from scratch
                            h::tail
                            |> List.append acc
                            |> replaceUnit log y.Variable.Name y.Unit
                            |> solve []

                        else
                            let xs, n, u =
                                // actually y = x
                                if xs |> List.length = 1 then
                                    let x = xs.Head
                                    [ x |> VariableUnit.setUnit y.Unit ],
                                    Some x.Variable.Name, Some y.Unit
                                // y = x1 * x2 ... so
                                // the x without a unit = y / multiple of all xs with units, i.e. (x1 * x2 .. )
                                else
                                    xs
                                    |> List.fold (fun acc x ->
                                        let xs', n, u = acc
                                        if x |> noUnit then // found the x without a unit
                                            let x =
                                                { x with
                                                    Unit =
                                                        xs
                                                        |> List.filter hasUnit
                                                        |> List.map VariableUnit.getUnit
                                                        |> List.reduce (ValueUnit.calcUnit (*))
                                                        |> ValueUnit.calcUnit (/)
                                                            (y |> VariableUnit.getUnit)
                                                }
                                            (x::xs', (Some x.Variable.Name), (Some x.Unit))
                                        else
                                            (x::xs', n, u)
                                    ) ([], None, None)

                            let h = (y, xs) |> OrderProductEquation
                            // start from scratch
                            h::tail
                            |> List.append acc
                            |> replaceUnit log (n |> Option.get) (u |> Option.get)
                            |> solve []

                    else
                        solve (h::acc) tail

                | OrderSumEquation (y, xs) ->
                    if y::xs |> List.forall hasUnit ||
                       y::xs |> List.forall noUnit then
                        solve (h::acc) tail

                    else
                        // get the names of vrus with no unit
                        let ns =
                            y::xs
                            |> List.filter noUnit
                            |> List.map VariableUnit.getName
                        // find the vru with a unit
                        let x =
                            y::xs
                            |> List.find hasUnit
                        // start from scratch
                        ({ y with Unit = x.Unit },
                         xs |> List.map (VariableUnit.setUnit x.Unit))
                        |> OrderSumEquation
                        |> List.singleton
                        |> List.append tail
                        |> List.append acc
                        // make sure that all vrus in all eqs get the unit
                        |> (fun eqs ->
                            ns
                            |> List.fold (fun acc n ->
                                acc |> replaceUnit log n x.Unit
                            ) eqs
                        )
                        |> solve []

        solve [] eqs


    let toVariableUnits =
        List.map (fun eq ->
            match eq with
            | OrderProductEquation (y, xs)
            | OrderSumEquation     (y, xs) -> y::xs
        )


    /// Turn a set of values `vs` to base values
    let toBase n eqs v =

        eqs
        |> toVariableUnits
        |> List.tryFindInList (VariableUnit.getName >> ((=) n))
        |> function
        | Some vru ->
            vru
            |> VariableUnit.getUnit
            |> fun u ->
                v
                |> ValueUnit.create u
                |> ValueUnit.toBase

        | None ->
            $"could not find %A{n} in toBase n eqs vs"
            |> failwith


    let mapFromSolverEqs orig eqs =
        let vrusl = orig |> toVariableUnits
        let vars =
            eqs
            |> List.collect Equation.toVars
            |> List.distinct

        vrusl
        |> List.map (fun vrus ->
            vrus
            |> List.map (fun vru ->
                { vru with
                    Variable =
                        vars
                        |> List.tryFind (fun v -> v.Name = vru.Variable.Name)
                        |> function
                        | Some v -> v
                        | None ->
                            $"could not find %A{vru.Variable.Name}"
                            |> failwith
                }
            )
        )



    let setVals n p eqs =
        eqs
        |> Api.setVariableValues n p


    let filterEqsWithUnits =
        List.filter (fun eq ->
            match eq with
            | OrderProductEquation (y, xs)
            | OrderSumEquation     (y, xs) ->
                y::xs |> List.forall VariableUnit.hasUnit
        )


    let propToBase n eqs p = p |> Property.mapValue (toBase n eqs)


    // Solve a set of equations setting a property `p` with
    // name `n`, to a valueset `vs`.
    let solve log n p eqs =
        let sortQue = Solver.sortQue
        let toBase = propToBase n eqs

        eqs
        // use only eqs with all vrus have units
        |> filterEqsWithUnits
        |> mapToSolverEqs
        |> Api.solve false sortQue log n (p |> toBase)
        |> mapFromSolverEqs eqs


    let solveAll log eqs =
        eqs
        // use only eqs with all vrus have units
        |> filterEqsWithUnits
        |> mapToSolverEqs
        |> Api.solveAll false log
        |> mapFromSolverEqs eqs


    let applyConstraints log (cs : Constraint list) eqs =
        let cs =
            cs
            |> List.map (fun c ->
                { c with
                    Property =
                        c.Property
                        |> propToBase c.Name eqs
                }
            )

        eqs
        // use only eqs with all vrus have units
        |> filterEqsWithUnits
        |> mapToSolverEqs
        |> fun eqs -> Api.applyConstraints true log eqs cs
        |> mapFromSolverEqs eqs


    let solveConstraints log (cs : Constraint list) eqs =
        let cs =
            cs
            |> List.map (fun c ->
                { c with
                    Property =
                        c.Property
                        |> propToBase c.Name eqs
                }
            )

        eqs
        // use only eqs with all vrus have units
        |> filterEqsWithUnits
        |> mapToSolverEqs
        |> Api.solveConstraints true log cs
        |> mapFromSolverEqs eqs