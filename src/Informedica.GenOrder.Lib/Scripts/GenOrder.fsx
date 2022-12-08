

#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: FParsec"


#r "../../Informedica.Utils.Lib/bin/Debug/net6.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.GenUnits.Lib/bin/Debug/net6.0/Informedica.GenUnits.Lib.dll"
#r "../../Informedica.GenSolver.Lib/bin/Debug/net6.0/Informedica.GenSolver.Lib.dll"


fsi.AddPrinter<System.DateTime> (fun dt -> dt.ToShortDateString())


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
    type OrderVariable =
        {
            Min : Minimum option
            Max : Maximum option
            Incr : Increment option
            /// Stores the values/range
            Variable:  Variable
            /// Stores the unit
            Unit: Unit
        }


    /// An order equation is either a product equation or a
    /// sum equation
    type OrderEquation =
        | OrderProductEquation of OrderVariable * OrderVariable list
        | OrderSumEquation of OrderVariable * OrderVariable list


    /// Time "tme"
    /// Type that represents a time
    type Time = Time of OrderVariable


    /// Count "cnt"
    /// Type that represents a count
    type Count = Count of OrderVariable


    /// Count / Time "frq"
    /// Type that represents a frequency
    type Frequency = Frequency of OrderVariable


    /// Quantity "qty"
    /// Type that represents a quantity
    type Quantity = Quantity of OrderVariable


    //// Quantity / Time "ptm"
    /// Type that represents a quantity per time
    type QuantityPerTime = QuantityPerTime of OrderVariable

    /// Quantity / Time "rte"
    /// Type that represents a rate
    type Rate = Rate of OrderVariable


    /// Quantity "tot"
    /// Type that represents a total
    type Total = Total of OrderVariable


    /// Quantity / Quantity "cnc"
    /// Type that represents a concentration
    type Concentration = Concentration of OrderVariable


    /// Quantity / Adjust "qty_adj"
    /// Type that represents a adjusted quantity
    type QuantityAdjust = QuantityAdjust of OrderVariable


    /// Quantity / Adjust / Time "ptm_adj"
    /// Type that represents a adjusted quantity per time
    type QuantityPerTimeAdjust = QuantityPerTimeAdjust of OrderVariable


    /// Quantity / Adjust / Time "rte_adj"
    /// Type that represents a adjusted quantity per time
    type RateAdjust = RateAdjust of OrderVariable


    /// Quantity / Adjust "tot_adj"
    /// Type that represents a adjusted total
    type TotalAdjust = TotalAdjust of OrderVariable



    /// An Id is represented by a string
    type Id = Id of string


    type Dose =
        {
            Quantity : Quantity
            PerTime : QuantityPerTime
            Rate : Rate
            Total : Total
            QuantityAdjust : QuantityAdjust
            PerTimeAdjust : QuantityPerTimeAdjust
            RateAdjust : RateAdjust
            TotalAdjust : TotalAdjust
        }


    /// Models an `Item` in a `Component`
    type Item =
        {
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
        }


    /// Models in a `Component` in and `Orderable`
    type Component =
        {
            Id : Id
            /// The name of a `Component`
            Name: Name
            // The shape of an component
            Shape : string
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
            /// The `Item`s in a `Component`
            Items: Item list
        }


    /// Models an `Orderable`
    type Orderable =
        {
            /// The name of the orderable
            Name: Name
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
            Route: string // Route
            /// The duration of an order
            Duration: Time
            /// The start stop date of the order
            StartStop: StartStop
        }


    /// Type that represents a prescription
    and Prescription =
        | Continuous
        /// A discontinuous prescription with a frequency
        | Discontinuous of Frequency
        /// A discontinuous prescription with both frequency and time
        | Timed of Frequency * Time


    type EquationMapping =
        | ProductMapping of string list
        | SumMapping of string list


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
        // Quantity of an Item in an Order
        | ItemOrderQuantity
        // Dose Quantity of an Item
        | ItemDoseQuantity
        // Dose Quantity of an Item per time
        | ItemDosePerTime
        // Dose Rate of an Item
        | ItemDoseRate
        // Dose Total of an Item in an Order
        | ItemDoseTotal
        // Adjusted Dose Quantity of an Item
        | ItemDoseQuantityAdjust
        // Adjusted Dose Total of an Item
        | ItemDosePerTimeAdjust
        // Adjusted Dose Rate of an Item
        | ItemDoseRateAdjust
        // Adjusted Total Dose of an Item in an Order
        | ItemDoseTotalAdjust
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
        | ComponentDosePerTime
        // Dose Rate of a Component
        | ComponentDoseRate
        // Dose Total of a Component in Order
        | ComponentDoseTotal
        // Adjusted Dose Quantity of a Component
        | ComponentDoseQuantityAdjust
        // Adjusted Dose Total of a Component
        | ComponentDosePerTimeAdjust
        // Adjusted Dose Rate of a Component
        | ComponentDoseRateAdjust
        // Adjusted Dose Total of a Component in an Order
        | ComponentDoseTotalAdjust
        // Quantity of an Orderable
        | OrderableOrderableQuantity
        // Amount of Dose Quantity in an Orderable Quantity
        | OrderableDoseCount
        // Dose Quantity of an Orderable
        | OrderableDoseQuantity
        // Dose Total of an Orderable
        | OrderableDosePerTime
        // Dose Rate of an Orderable
        | OrderableDoseRate
        // Dose Total of an Orderable in an Order
        | OrderableDoseTotal
        // Adjusted Dose Quantity of an Orderable
        | OrderableDoseQuantityAdjust
        // Adjusted Dose Total of an Orderable
        | OrderableDosePerTimeAdjust
        // Adjusted Dose Rate of an Orderable
        | OrderableDoseRateAdjust
        // Adjusted Dose Total of an Orderable in an Order
        | OrderableDoseTotalAdjust
        // Prescription Frequency
        | OrderPrescriptionFrequency
        // Prescription Time
        | OrderPrescriptionTime
        // Quantity of an Orderable in an Order
        | OrderableOrderQuantity
        // Amount of Orderables in an Order
        | OrderableOrderCount
        // Order Adjust Quantity
        | OrderAdjustQuantity
        // The duration of the order
        | OrderOrderTime


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
            /// maps to ItemDosePerTime
            MinDosePerTime : BigRational option
            /// maps to ItemDosePerTime
            MaxDosePerTime : BigRational option
            /// maps to ItemDoseAdjustTotalAdjust
            MinDosePerTimeAdjust : BigRational option
            /// maps to ItemDoseAdjustTotalAdjust
            MaxDosePerTimeAdjust : BigRational option
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
            $"https://docs.google.com/spreadsheets/d/%s{id}/gviz/tq?tqx=out:csv&sheet=%s{sheet}"

        //https://docs.google.com/spreadsheets/d/1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g/edit?usp=sharing
        let [<Literal>] constraints = "1nny8rn9zWtP8TMawB3WeNWhl5d4ofbWKbGzGqKTd49g"


        //https://docs.google.com/spreadsheets/d/1ccW2b6vYVZQsyb8TGpZtJoqtllCbmwrZiIFZ7BN1sB4/edit?usp=sharing
        let [<Literal>] genpres = "1ccW2b6vYVZQsyb8TGpZtJoqtllCbmwrZiIFZ7BN1sB4"


        let client = new HttpClient()


        let download url =
            async {
                use! resp = client.GetAsync(Uri(url)) |> Async.AwaitTask
                use! stream = resp.Content.ReadAsStreamAsync() |> Async.AwaitTask
                use reader = new StreamReader(stream)
                return reader.ReadToEnd()
            }


        let getDataFromExcelSheet excel sheet =
            createUrl sheet excel
            |> download
            |> Async.RunSynchronously
            |> Csv.parseCSV



        let getDataFromConstraints = getDataFromExcelSheet constraints


        let getDataFromGenPres = getDataFromExcelSheet genpres



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


        let [<Literal>] concatWith = "."
        let [<Literal>] addWith = "_"


        /// Create a `Name` from a list of strings that
        let create ns =
            try
                $"[{ns |> String.concat concatWith}]" |> Name.createExc
            with
            | e ->
                printfn $"cannot create name with {ns}"
                raise e

        let toString  = Name.toString


        let fromString = Name.createExc


        let toStringList =
            Name.toString
            >> (String.replace "[" "")
            >> (String.replace "]" "")
            >> (String.replace addWith concatWith)
            >> (String.split concatWith)


        let add s n =
            try
                $"{n |> toString}{addWith}{s}" |> Name.createExc
            with
            | e ->
                printfn $"cannot add name with {s} and {n}"
                raise e



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



module Variable =

    open Informedica.GenSolver.Lib.Types

    module ValueRange =

        open Informedica.GenSolver.Lib.Variable.ValueRange


        let scale n vr =
            let calc = (*) n
            let mapMin = Minimum.map calc calc
            let mapMax = Maximum.map calc calc

            let fVs vs =
                vs
                |> ValueSet.map calc

            let fMinMax (min, max) =
                min |> mapMin,
                max |> mapMax

            let fIncr = Increment.map calc

            let fMinIncr (min, incr) =
                min |> mapMin,
                incr |> Increment.map calc

            let fIncrMax (incr, max) =
                incr |> Increment.map calc,
                max |> mapMax

            let fMinIncrMax (min, incr, max) =
                min |> mapMin,
                incr |> Increment.map calc,
                max |> mapMax

            vr |> map mapMin mapMax fMinMax fIncr fMinIncr fIncrMax fMinIncrMax fVs


    let scale n (var : Variable) =
        { var with Values = var.Values |> ValueRange.scale n }


/// Functions that deal with the `VariableUnit` type
module OrderVariable =

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
    module Multipliers = ValueUnit.Multipliers

    type Unit = ValueUnit.Unit


    /// Create a `VariableUnit` with preset values
    let create n min incr max vs un =
        ValueRange.create true min incr max vs
        |> fun vlr ->
            let var = Variable.create id n vlr
            {
                Min = min
                Max = max
                Incr = incr
                Variable = var
                Unit = un
            }


    /// Create a new `VariableUnit` with
    /// `Name` **nm** and `Unit` **un**
    let createNew n un = create n None None None None un


    /// Apply **f** to `VariableUnit` **vru**
    let apply f (ovar: OrderVariable) = ovar |> f


    /// Utility function to facilitate type inference
    let get = apply id


    /// Get all record fields from a `VariableUnit`
    let getMembers { Variable = var; Unit = un } =
        var, un


    /// Get the `Variable` from a `VariableUnit`
    let getVar = getMembers >> fst


    /// Get the `Variable.Name` from a `VariableUnit` **vru**
    let getName ovar = (ovar |> getVar).Name


    let eqsName ovar1 ovar2 = (ovar1 |> getName) = (ovar2 |> getName)



    /// Get the `Unit` from a `VariableUnit`
    let getUnit = getMembers >> snd


    let hasUnit = getUnit >> ((<>) ValueUnit.NoUnit)


    let scale n ovar =
        let calc = (*) n
        { ovar with
            Min = ovar.Min |> Option.map (Minimum.map calc calc)
            Max = ovar.Max |> Option.map (Maximum.map calc calc)
            Incr = ovar.Incr |> Option.map (Increment.map calc)
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


    (*
    let valueToBase v ovar =
        v |> ValueUnit.valueToBase (ovar |> getUnit)


    let valueToUnit v ovar =
        v |> ValueUnit.valueToUnit (ovar |> getUnit)


    let getBaseValues =
        getVar
        >> Variable.getValueRange
        >> Variable.ValueRange.getValSet
        >> Option.map Variable.ValueRange.ValueSet.toSet
        >> Option.defaultValue Set.empty


    let getUnitValues ovar =
        ovar
        |> getBaseValues
        |> Seq.map (fun vs ->
            vs, ovar |> getUnit
        )
        |> Seq.map (fun (v, u) ->
            v
            |> ValueUnit.valueToUnit u
        )


    let containsBaseValue ovar =
        getVar
        >> Variable.getValueRange
        >> ValueRange.contains ovar


    let containsUnitValue v ovar =
        let u = ovar |> getUnit

        ovar
        |> getVar
        |> Variable.getValueRange
        |> ValueRange.contains (v |> ValueUnit.valueToBase u)
        *)


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

            let n    = dto.Name |> Name.fromString
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

        let toDto (vu : OrderVariable) =
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



    /// Type and functions that represent a count
    module Count =


        let [<Literal>] name = "cnt"


        /// Turn `Count` in a `VariableUnit`
        let toOrdVar (Count cnt) = cnt


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString


        let getUnits = toOrdVar >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Count


        /// Set a `Count` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar Count


        /// Create a `Count` with name **n**
        let create n =
            Units.Count.times
            |> createNew (n |> Name.add name)
            |> Count


        /// Turn a `Count` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `Count` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toBase = toOrdVar >> toBase >> Count


        let toUnit = toOrdVar >> toUnit >> Count



    /// Type and functions that represent a time
    module Time =


        let [<Literal>] name = "tme"


        /// Turn `Time` in a `VariableUnit`
        let toOrdVar (Time tme) = tme


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString


        let getUnits = toOrdVar >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> Time


        /// Set a `Time` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar Time


        /// Create a `Time` with name **n**
        /// with `Unit` **un**
        let create n un =
            un
            |> createNew (n |> Name.add name)
            |> Time


        /// Turn a `Time` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `Time` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toBase = toOrdVar >> toBase >> Time


        let toUnit = toOrdVar >> toUnit >> Time



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
            | ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                Units.Count.times
                |> ValueUnit.per tu
            |> createNew (n |> Name.add name)
            |> Frequency


        /// Turn a `Frequency` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `Frequency` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toBase = toOrdVar >> toBase >> Frequency


        let toUnit = toOrdVar >> toUnit >> Frequency



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
            | ValueUnit.NoUnit, _
            | _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per su
            |> createNew (n |> Name.add name)
            |> Concentration


        /// Turn a `Concentration` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `Concentration` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toBase = toOrdVar >> toBase >> Concentration


        let toUnit = toOrdVar >> toUnit >> Concentration



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


        let toBase = toOrdVar >> toBase >> Quantity


        let toUnit = toOrdVar >> toUnit >> Quantity



    /// Type and functions that represent a quantity per time
    module QuantityPerTime =


        let [<Literal>] name = "ptm"


        /// Turn `PerTime` in a `VariableUnit`
        let toOrdVar (QuantityPerTime ptm) = ptm


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toOrdVar
            >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> QuantityPerTime


        /// Set a `PerTime` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar QuantityPerTime


        /// Create a `PerTime` with name **n**
        /// and `Unit` **un** and time unit **tu**
        let create n un tu =
            match un with
            | ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per tu
            |> createNew (n |> Name.add name)
            |> QuantityPerTime


        /// Turn a `PerTime` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `PerTime` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toBase = toOrdVar >> toBase >> QuantityPerTime


        let toUnit = toOrdVar >> toUnit >> QuantityPerTime



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
            | ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per tu
            |> createNew (n |> Name.add name)
            |> Rate


        /// Turn a `PerTime` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `PerTime` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toBase = toOrdVar >> toBase >> Rate


        let toUnit = toOrdVar >> toUnit >> Rate



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


        let toBase = toOrdVar >> toBase >> Total


        let toUnit = toOrdVar >> toUnit >> Total



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
            | ValueUnit.NoUnit, _
            | _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
            |> createNew (n |> Name.add name)
            |> QuantityAdjust


        /// Turn a `QuantityAdjust` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `QuantityAdjust` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toBase = toOrdVar >> toBase >> QuantityAdjust


        let toUnit = toOrdVar >> toUnit >> QuantityAdjust



    /// Type and functions that represent a adjusted total,
    /// and a adjusted total is a quantity per time
    module QuantityPerTimeAdjust =


        let [<Literal>] name = "ptm_adj"


        /// Turn `TotalAdjust` in a `VariableUnit`
        let toOrdVar (QuantityPerTimeAdjust ptm_adj) = ptm_adj


        let unitToString =
            toOrdVar
            >> getUnit
            >> ValueUnit.unitToString


        let getUnits =
            toOrdVar
            >> getUnits


        let toDto = toOrdVar >> Dto.toDto


        let fromDto dto = dto |> Dto.fromDto |> QuantityPerTimeAdjust


        /// Set a `TotalAdjust` with a `Variable`
        /// in a list fromVariable` lists
        let fromOrdVar = fromOrdVar toOrdVar QuantityPerTimeAdjust


        /// Create a `TotalAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj** per time unit **tu**
        let create n un adj tu =
            match un, adj, tu with
            | ValueUnit.NoUnit, _, _
            | _, ValueUnit.NoUnit, _
            | _, _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
                |> ValueUnit.per tu
            |> createNew (n |> Name.add name)
            |> QuantityPerTimeAdjust


        /// Turn a `TotalAdjust` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `TotalAdjust` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toBase = toOrdVar >> toBase >> QuantityPerTimeAdjust


        let toUnit = toOrdVar >> toUnit >> QuantityPerTimeAdjust



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
            | ValueUnit.NoUnit, _, _
            | _, ValueUnit.NoUnit, _
            | _, _, ValueUnit.NoUnit -> ValueUnit.NoUnit
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


        let toBase = toOrdVar >> toBase >> RateAdjust


        let toUnit = toOrdVar >> toUnit >> RateAdjust



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
            | ValueUnit.NoUnit, _
            | _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
            |> createNew (n |> Name.add name)
            |> TotalAdjust


        /// Turn a `QuantityAdjust` to a string
        let toString = toOrdVar >> (toString false)


        /// Print a `QuantityAdjust` as a value unit string list
        let toValueUnitStringList = toValueUnitStringList toOrdVar


        let toBase = toOrdVar >> toBase >> TotalAdjust


        let toUnit = toOrdVar >> toUnit >> TotalAdjust



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



    let filterEqsWithUnits =
        List.filter (fun eq ->
            match eq with
            | OrderProductEquation (y, xs)
            | OrderSumEquation     (y, xs) ->
                y::xs |> List.forall OrderVariable.hasUnit
        )


    let scaleOrderEqs scalar eqs =
        let eqs = eqs |> filterEqsWithUnits
        let toBase y xs =
            (y |> scalar, xs |> List.map scalar)

        eqs
        |> List.map (fun eq ->
            match eq with
            | OrderProductEquation (y, xs) -> toBase y xs |> OrderProductEquation
            | OrderSumEquation     (y, xs) -> toBase y xs |> OrderSumEquation
        )


    let orderEqsToBase = scaleOrderEqs OrderVariable.toBase


    let orderEqsToUnit = scaleOrderEqs OrderVariable.toUnit


    let mapToSolverEqs =
        List.map (fun eq ->
            match eq with
            | OrderProductEquation (y, xs) -> (y.Variable, xs |> List.map (fun v -> v.Variable)) |> ProductEquation
            | OrderSumEquation     (y, xs) -> (y.Variable, xs |> List.map (fun v -> v.Variable)) |> SumEquation
        )


    let mapToOrderEqs ordEqs eqs =
        let vars =
            eqs
            |> List.collect Equation.toVars
        let repl v =
            { v with
                Variable =
                    vars
                    |> List.find (Variable.getName >> ((=) v.Variable.Name))
            }
        ordEqs
        |> List.map (fun eq ->
            match eq with
            | OrderProductEquation (y, xs) ->
                (y |> repl, xs |> List.map repl)
                |> OrderProductEquation
            | OrderSumEquation (y, xs) ->
                (y |> repl, xs |> List.map repl)
                |> OrderSumEquation
        )


    let replaceUnit log n u eqs =
        (n, u)
        |> Events.SolverReplaceUnit
        |> Logging.logInfo log

        let repl c ovar ovars =
            if ovar |> OrderVariable.getName = n then
                (ovar |> OrderVariable.setUnit u, ovars)
            else
                ovar,
                ovars
                |> List.map (fun vru ->
                    if vru |> OrderVariable.getName = n then
                        vru
                        |> OrderVariable.setUnit u
                    else vru
                )
            |> c

        eqs
        |> List.map (fun e ->
            match e with
            | OrderSumEquation (ovar, ovars) -> repl OrderSumEquation ovar ovars
            | OrderProductEquation (ovar, ovars) -> repl OrderProductEquation ovar ovars
        )


    /// calculate the units for all variable units in
    /// all eqs
    let solveUnits log eqs =
        let hasUnit = OrderVariable.hasUnit
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
                                        |> List.map OrderVariable.getUnit
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
                                    [ x |> OrderVariable.setUnit y.Unit ],
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
                                                        |> List.map OrderVariable.getUnit
                                                        |> List.reduce (ValueUnit.calcUnit (*))
                                                        |> ValueUnit.calcUnit (/)
                                                            (y |> OrderVariable.getUnit)
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
                        // get the names of order variables with no unit
                        let ns =
                            y::xs
                            |> List.filter noUnit
                            |> List.map OrderVariable.getName
                        // find the vru with a unit
                        let x =
                            y::xs
                            |> List.find hasUnit
                        // start from scratch
                        ({ y with Unit = x.Unit },
                         xs |> List.map (OrderVariable.setUnit x.Unit))
                        |> OrderSumEquation
                        |> List.singleton
                        |> List.append tail
                        |> List.append acc
                        // make sure that all order variables in all eqs get the unit
                        |> (fun eqs ->
                            ns
                            |> List.fold (fun acc n ->
                                acc |> replaceUnit log n x.Unit
                            ) eqs
                        )
                        |> solve []

        solve [] eqs


    let solveMinMax = Api.solveAll true


    let solve = Api.solveAll false



/// Types and functions that deal with an order.
/// An `Order` models the `Prescription` of an
/// `Orderable` with a `StartStop` start date and
/// stop date.
module Order =

    open System
    open Informedica.Utils.Lib.BCL
    open Informedica.GenUnits.Lib
    open WrappedString


    /// Utility functions to
    /// enable mapping of a `Variable`s
    /// to an `Order`
    module Mapping =


        let [<Literal>] qty = OrderVariable.Quantity.name
        let [<Literal>] cnc = OrderVariable.Concentration.name
        let [<Literal>] ptm = OrderVariable.QuantityPerTime.name
        let [<Literal>] rte = OrderVariable.Rate.name
        let [<Literal>] tot = OrderVariable.Total.name
        let [<Literal>] qtyAdj = OrderVariable.QuantityAdjust.name
        let [<Literal>] ptmAdj = OrderVariable.QuantityPerTimeAdjust.name
        let [<Literal>] rteAdj = OrderVariable.RateAdjust.name
        let [<Literal>] totAdj = OrderVariable.TotalAdjust.name
        let [<Literal>] cnt = OrderVariable.Count.name
        let [<Literal>] frq = OrderVariable.Frequency.name
        let [<Literal>] tme = OrderVariable.Time.name
        let [<Literal>] itm = "itm" //Orderable.Literals.item
        let [<Literal>] cmp = "cmp" //Orderable.Literals.comp
        let [<Literal>] orb = "orb" //Orderable.Literals.orderable
        let [<Literal>] dos = "dos" //Orderable.Literals.dose
        let [<Literal>] prs = "prs" //"Prescription"
        let [<Literal>] ord = "ord" // "Order"
        let [<Literal>] adj = "adj" // "Adjust"

        let [<Literal>] discontinuous = 3
        let [<Literal>] continuous = 4
        let [<Literal>] timed = 5


        let getEquations indx =
            Web.getDataFromGenPres "Equations"
            |> Array.skip 1
            |> Array.filter (fun xs -> xs[indx] = "x")
            |> Array.map (Array.item 1)
            |> Array.toList


        let getEqsMapping (ord: Order) (eqs : string list) =
            let sumEqs =
                eqs
                |> List.filter (String.contains "sum")

            let prodEqs =
                eqs
                |> List.filter (String.contains "sum" >> not)

            let itmEqs =
                prodEqs
                |> List.filter (String.contains "[itm]")

            let cmpEqs =
                prodEqs
                |> List.filter (fun e ->
                    itmEqs
                    |> List.exists ((=) e)
                    |> not &&
                    e.Contains("[cmp]")
                )

            let orbEqs =
                prodEqs
                |> List.filter (fun e ->
                    itmEqs
                    |> List.exists ((=) e)
                    |> not &&
                    cmpEqs
                    |> List.exists((=) e)
                    |> not
                )

            let idN = [ord.Id |> Id.toString] |> Name.create
            let orbN = [ord.Id |> Id.toString; ord.Orderable.Name |> Name.toString] |> Name.create

            ord.Orderable.Components
            |> List.fold (fun acc c ->
                let cmpN =
                    [
                        yield! orbN |> Name.toStringList
                        c.Name |> Name.toString
                    ]
                    |> Name.create

                let itms =
                    c.Items
                    |> List.collect (fun i ->
                        itmEqs
                        |> List.map (fun s ->
                            let itmN =
                                [
                                    yield! cmpN |> Name.toStringList
                                    i.Name |> Name.toString
                                ]
                                |> Name.create
                            s
                            |> String.replace "[cmp]" $"{cmpN |> Name.toString}"
                            |> String.replace "[itm]" $"{itmN |> Name.toString}"
                        )
                    )

                let cmps =
                    cmpEqs
                    |> List.map (String.replace "[cmp]" $"{cmpN |> Name.toString}")

                acc
                |> List.append cmps
                |> List.append itms
            ) []
            |> fun es ->
                let sumEqs =
                    sumEqs
                    |> List.map (fun e ->
                        match e
                              |> String.replace "sum(" ""
                              |> String.replace ")" ""
                              |> String.split " = " with
                        | [lv; rv] ->
                            ord.Orderable.Components
                            |> List.map(fun c ->
                                let cmpN =
                                    [
                                        yield! orbN |> Name.toStringList
                                        c.Name |> Name.toString
                                    ]
                                    |> Name.create

                                rv
                                |> String.replace "[cmp]" $"{cmpN |> Name.toString}"
                            )
                            |> String.concat " + "
                            |> fun s -> $"{lv} = {s}"
                        | _ ->
                            printfn $"could not match {e}"
                            ""
                    )
                    |> List.filter (String.isNullOrWhiteSpace >> not)
                    |> List.map (String.replace "[orb]" $"{orbN |> Name.toString}")
                    |> SumMapping
                let prodEqs =
                    es
                    |> List.append orbEqs
                    |> List.append es
                    |> List.map (String.replace "[orb]" $"{orbN |> Name.toString}")
                    |> List.map (String.replace "[ord]" $"{idN |> Name.toString}")
                    |> ProductMapping

                sumEqs, prodEqs



    /// Types and functions to deal
    /// with an `Orderable`, i.e. something
    /// that can be ordered.
    module Orderable =

        open Informedica.GenSolver.Lib
        open Types

        type Name = Types.Name

        /// Contains string constants
        /// to create `Variable` names
        module Literals =

            [<Literal>]
            let item = Mapping.itm
            [<Literal>]
            let comp = Mapping.cmp
            [<Literal>]
            let orderable = Mapping.orb
            [<Literal>]
            let order = Mapping.ord
            [<Literal>]
            let dose = Mapping.dos



        module Dose =

            module Quantity = OrderVariable.Quantity
            module QuantityPerTime = OrderVariable.QuantityPerTime
            module Rate = OrderVariable.Rate
            module Total = OrderVariable.Total
            module QuantityAdjust = OrderVariable.QuantityAdjust
            module QuantityPerTimeAdjust = OrderVariable.QuantityPerTimeAdjust
            module RateAdjust = OrderVariable.RateAdjust
            module TotalAdjust = OrderVariable.TotalAdjust


            let create qty ptm rte tot qty_adj ptm_adj rte_adj tot_adj =
                {
                    Quantity = qty
                    PerTime = ptm
                    Rate = rte
                    Total = tot
                    QuantityAdjust = qty_adj
                    PerTimeAdjust = ptm_adj
                    RateAdjust = rte_adj
                    TotalAdjust = tot_adj
                }

            let createNew n =
                let un = ValueUnit.NoUnit
                let n = n |> Name.add Literals.dose

                let qty = Quantity.create n un
                let ptm = QuantityPerTime.create n un un
                let rte = Rate.create n un un
                let tot = Total.create n un
                let qty_adj = QuantityAdjust.create n un un
                let rte_adj = RateAdjust.create n un un un
                let ptm_adj = QuantityPerTimeAdjust.create n un un un
                let tot_adj = TotalAdjust.create n un un

                create qty ptm rte tot qty_adj ptm_adj rte_adj tot_adj


            /// Turn an `Item` to `VariableUnit`s
            let toOrdVars (dos : Dose) =
                let qty = dos.Quantity |> Quantity.toOrdVar
                let ptm = dos.PerTime |> QuantityPerTime.toOrdVar
                let rte = dos.Rate |> Rate.toOrdVar
                let tot = dos.Total |> Total.toOrdVar
                let qty_adj = dos.QuantityAdjust |> QuantityAdjust.toOrdVar
                let ptm_adj = dos.PerTimeAdjust |> QuantityPerTimeAdjust.toOrdVar
                let rte_adj = dos.RateAdjust |> RateAdjust.toOrdVar
                let tot_adj = dos.TotalAdjust |> TotalAdjust.toOrdVar

                [
                    qty
                    ptm
                    rte
                    tot
                    qty_adj
                    ptm_adj
                    rte_adj
                    tot_adj
                ]

            let fromOrdVars ovars (dos: Dose) =
                let qty = dos.Quantity |> Quantity.fromOrdVar ovars
                let ptm = dos.PerTime |> QuantityPerTime.fromOrdVar ovars
                let rte = dos.Rate |> Rate.fromOrdVar ovars
                let tot = dos.Total |> Total.fromOrdVar ovars
                let qty_adj = dos.QuantityAdjust |> QuantityAdjust.fromOrdVar ovars
                let ptm_adj = dos.PerTimeAdjust |> QuantityPerTimeAdjust.fromOrdVar ovars
                let rte_adj = dos.RateAdjust |> RateAdjust.fromOrdVar ovars
                let tot_adj = dos.TotalAdjust |> TotalAdjust.fromOrdVar ovars

                create qty ptm rte tot qty_adj ptm_adj rte_adj tot_adj



            /// Turn an `Item` to a list of `string`s,
            /// each string containing the variable
            /// `Name`, `ValueRange` and `Unit`
            let toString = toOrdVars >> List.map (OrderVariable.toString false)


            module Dto =


                module Units = ValueUnit.Units
                module Quantity = OrderVariable.Quantity
                module QuantityPerTime = OrderVariable.QuantityPerTime
                module Rate = OrderVariable.Rate
                module Total = OrderVariable.Total
                module QuantityAdjust = OrderVariable.QuantityAdjust
                module QuantityPerTimeAdjust = OrderVariable.QuantityPerTimeAdjust
                module RateAdjust = OrderVariable.RateAdjust
                module TotalAdjust = OrderVariable.TotalAdjust


                type Dto () =
                    member val Quantity = OrderVariable.Dto.dto () with get, set
                    member val PerTime = OrderVariable.Dto.dto () with get, set
                    member val Rate = OrderVariable.Dto.dto () with get, set
                    member val Total = OrderVariable.Dto.dto () with get, set
                    member val QuantityAdjust = OrderVariable.Dto.dto () with get, set
                    member val PerTimeAdjust = OrderVariable.Dto.dto () with get, set
                    member val RateAdjust = OrderVariable.Dto.dto () with get, set
                    member val TotalAdjust = OrderVariable.Dto.dto () with get, set


                let fromDto (dto: Dto) =

                    let qty = dto.Quantity |> Quantity.fromDto
                    let ptm = dto.PerTime |> QuantityPerTime.fromDto
                    let rte = dto.Rate |> Rate.fromDto
                    let tot = dto.Total |> Total.fromDto
                    let qty_adj = dto.QuantityAdjust |> QuantityAdjust.fromDto
                    let ptm_adj = dto.PerTimeAdjust |> QuantityPerTimeAdjust.fromDto
                    let rte_adj = dto.RateAdjust |> RateAdjust.fromDto
                    let tot_adj = dto.TotalAdjust |> TotalAdjust.fromDto

                    create qty ptm rte tot qty_adj ptm_adj rte_adj tot_adj

                let toDto (dos : Dose) =
                    let dto = Dto ()

                    dto.Quantity <-
                        dos.Quantity
                        |> Quantity.toDto
                    dto.PerTime <-
                        dos.PerTime
                        |> QuantityPerTime.toDto
                    dto.Rate <-
                        dos.Rate
                        |> Rate.toDto
                    dto.Total <-
                        dos.Total
                        |> Total.toDto
                    dto.QuantityAdjust <-
                        dos.QuantityAdjust
                        |> QuantityAdjust.toDto
                    dto.PerTimeAdjust <-
                        dos.PerTimeAdjust
                        |> QuantityPerTimeAdjust.toDto
                    dto.RateAdjust <-
                        dos.RateAdjust
                        |> RateAdjust.toDto
                    dto.TotalAdjust <-
                        dos.TotalAdjust
                        |> TotalAdjust.toDto

                    dto


                let dto () = Dto ()



        /// Type and functions that models an
        /// `Order` `Item` that is contained in
        /// a `Component`
        module Item =

            module Quantity = OrderVariable.Quantity
            module Concentration = OrderVariable.Concentration
            module Total = OrderVariable.Total
            module Rate = OrderVariable.Rate


            /// Create an item with
            ///
            /// * **id**: the order id
            /// * **n**: the name of the item
            /// * **cmp_qty**: the quantity of the item in a component
            /// * **orb_qty**: the quantity of the item in an orderable
            /// * **cmp_cnc**: the item concentration in a component
            /// * **orb_cnc**: the item concentration in an orderable
            /// * **dos**: the item dose
            let create n cmp_qty orb_qty cmp_cnc orb_cnc dos =
                {
                    Name = n
                    ComponentQuantity = cmp_qty
                    OrderableQuantity = orb_qty
                    ComponentConcentration = cmp_cnc
                    OrderableConcentration = orb_cnc
                    Dose = dos
                }


            /// Create a new item with
            ///
            /// **id**: the order id
            /// **n**: the string name of the item
            let createNew id orbN cmpN itmN =
                let un = ValueUnit.NoUnit
                let n =
                    [ id; orbN; cmpN; itmN ]
                    |> Name.create

                let cmp_qty = let n = n |> Name.add Literals.comp in Quantity.create n un
                let orb_qty = let n = n |> Name.add Literals.orderable in Quantity.create n un
                let cmp_cnc = let n = n |> Name.add Literals.comp in Concentration.create n un un
                let orb_cnc = let n = n |> Name.add Literals.orderable in Concentration.create n un un
                let dos     = Dose.createNew n

                create (itmN |> Name.fromString) cmp_qty orb_qty cmp_cnc orb_cnc dos


            /// Apply **f** to an `item`
            let apply f (itm: Item) = itm |> f


            /// Utility method to facitilitate type inference
            let get = apply id


            /// Get the `Name` of an `Item`
            let getName itm = (itm |> get).Name


            /// Get the `Item` dose
            let getDose itm = (itm |> get).Dose


            /// Turn an `Item` to `VariableUnit`s
            let toOrdVars itm =
                let itm_cmp_qty = (itm |> get).ComponentQuantity |> Quantity.toOrdVar
                let itm_orb_qty = itm.OrderableQuantity          |> Quantity.toOrdVar
                let itm_cmp_cnc = itm.ComponentConcentration     |> Concentration.toOrdVar
                let itm_orb_cnc = itm.OrderableConcentration     |> Concentration.toOrdVar

                [
                    itm_cmp_qty
                    itm_orb_qty
                    itm_cmp_cnc
                    itm_orb_cnc
                    yield! itm.Dose |> Dose.toOrdVars
                ]


            let fromOrdVars ovars itm =
                let cmp_qty = (itm |> get).ComponentQuantity |> Quantity.fromOrdVar ovars
                let orb_qty = itm.OrderableQuantity          |> Quantity.fromOrdVar ovars
                let cmp_cnc = itm.ComponentConcentration     |> Concentration.fromOrdVar ovars
                let orb_cnc = itm.OrderableConcentration     |> Concentration.fromOrdVar ovars
                let dos = itm.Dose |> Dose.fromOrdVars ovars

                create itm.Name cmp_qty orb_qty cmp_cnc orb_cnc dos



            /// Turn an `Item` to a list of `string`s,
            /// each string containing the variable
            /// `Name`, `ValueRange` and `Unit`
            let toString = toOrdVars >> List.map (OrderVariable.toString false)



            module Dto =

                module Units = ValueUnit.Units
                module Id = WrappedString.Id
                module Name = WrappedString.Name
                module Quantity = OrderVariable.Quantity
                module Concentration = OrderVariable.Concentration


                type Dto () =
                    member val Name = "" with get, set
                    member val ComponentQuantity = OrderVariable.Dto.dto () with get, set
                    member val OrderableQuantity = OrderVariable.Dto.dto () with get, set
                    member val ComponentConcentration = OrderVariable.Dto.dto () with get, set
                    member val OrderableConcentration = OrderVariable.Dto.dto () with get, set
                    member val Dose = Dose.Dto.dto () with get, set


                let fromDto (dto: Dto) =
                    let n = dto.Name |> Name.fromString
                    let cmp_qty = dto.ComponentQuantity |> Quantity.fromDto
                    let orb_qty = dto.OrderableQuantity |> Quantity.fromDto
                    let cmp_cnc = dto.ComponentConcentration |> Concentration.fromDto
                    let orb_cnc = dto.OrderableConcentration |> Concentration.fromDto
                    let dos = dto.Dose |> Dose.Dto.fromDto

                    create n cmp_qty orb_qty cmp_cnc orb_cnc dos


                let toDto (itm : Item) =
                    let dto = Dto ()

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
                    dto.Dose <-
                        itm.Dose |> Dose.Dto.toDto

                    dto


                let dto id orbN cmpN itmN =
                    createNew id orbN cmpN itmN
                    |> toDto



        /// Types and functions to model a
        /// `Component` in an `Orderable`.
        /// A `Component` contains a list
        /// of `Item`s
        module Component =

            module Name = Name
            module Quantity = OrderVariable.Quantity
            module Concentration = OrderVariable.Concentration
            module Count = OrderVariable.Count


            /// Create a component with
            ///
            /// * `id`: the order id
            /// * `n`: the name of the component
            /// * `cmp_qty`: quantity of component
            /// * `orb_qty`: quantity of component in orderable
            /// * `orb_cnt`: count of component in orderable
            /// * `ord_qty`: quantity of component in order
            /// * `ord_cnt`: count of component in order
            /// * `orb_cnc`: concentration of component in orderable
            /// * `dos`: component dose
            /// * `dos_adj`: adjusted dose of component
            /// * `ii`: list of `Item`s in a component
            let create id nm sh cmp_qty orb_qty orb_cnt ord_qty ord_cnt orb_cnc dos ii =
                {
                    Id = id
                    Name = nm
                    Shape = sh
                    ComponentQuantity = cmp_qty
                    OrderableQuantity = orb_qty
                    OrderableCount = orb_cnt
                    OrderQuantity = ord_qty
                    OrderCount = ord_cnt
                    OrderableConcentration = orb_cnc
                    Dose = dos
                    Items = ii
                }

            /// Create a new component with
            /// * `id`: the id of the component
            /// * `n`: the name of the component
            let createNew id orbN cmpN sh =
                let un = ValueUnit.NoUnit
                let nm = [ id; orbN; cmpN ] |> Name.create
                let id = Id.create id

                let cmp_qty = let n = nm |> Name.add Literals.comp in Quantity.create n un
                let orb_qty = let n = nm |> Name.add Literals.orderable in Quantity.create n un
                let orb_cnt = let n = nm |> Name.add Literals.orderable in Count.create n
                let ord_qty = let n = nm |> Name.add Literals.order in Quantity.create n un
                let ord_cnt = let n = nm |> Name.add Literals.order in Count.create n
                let orb_cnc = let n = nm |> Name.add Literals.orderable in Concentration.create n un un
                let dos     = Dose.createNew nm

                create id (cmpN |> Name.fromString) sh cmp_qty orb_qty orb_cnt ord_qty ord_cnt orb_cnc dos []


            /// Apply **f** to a `Component` **comp**
            let apply f (comp: Component) = comp |> f


            /// Utility to facilitate type inference
            let get = apply id


            /// Get the name of a `Component`
            let getName cmp = (cmp |> get).Name


            /// Get the `Item`s in an `Component`
            let getItems cmp = (cmp |> get).Items


            /// Map a `Component` **cmp**
            /// to `VariableUnit`s
            let toOrdVars cmp =
                let cmp_qty = (cmp |> get).ComponentQuantity |> Quantity.toOrdVar
                let orb_qty = cmp.OrderableQuantity          |> Quantity.toOrdVar
                let orb_cnt = cmp.OrderableCount             |> Count.toOrdVar
                let orb_cnc = cmp.OrderableConcentration     |> Concentration.toOrdVar
                let ord_qty = cmp.OrderQuantity              |> Quantity.toOrdVar
                let ord_cnt = cmp.OrderCount                 |> Count.toOrdVar

                [
                    cmp_qty
                    orb_qty
                    orb_cnt
                    orb_cnc
                    ord_qty
                    ord_cnt
                    yield! cmp.Dose |> Dose.toOrdVars
                    yield! cmp.Items |> List.collect Item.toOrdVars
                ]


            /// Map a `Component` **cmp**
            /// to `VariableUnit`s
            let fromOrdVars ovars cmp =
                let cmp_qty = (cmp |> get).ComponentQuantity |> Quantity.fromOrdVar ovars
                let orb_qty = cmp.OrderableQuantity          |> Quantity.fromOrdVar ovars
                let orb_cnt = cmp.OrderableCount             |> Count.fromOrdVar ovars
                let orb_cnc = cmp.OrderableConcentration     |> Concentration.fromOrdVar ovars
                let ord_qty = cmp.OrderQuantity              |> Quantity.fromOrdVar ovars
                let ord_cnt = cmp.OrderCount                 |> Count.fromOrdVar ovars
                let dos = cmp.Dose |> Dose.fromOrdVars ovars

                cmp.Items
                |> List.map (Item.fromOrdVars ovars)
                |> create cmp.Id cmp.Name cmp.Shape cmp_qty orb_qty orb_cnt ord_qty ord_cnt orb_cnc dos


            /// Create a string list from a
            /// component where each string is
            /// a variable name with the value range
            /// and the Unit
            let toString cmp =
                let ii = cmp.Items

                cmp
                |> toOrdVars
                |> List.map (OrderVariable.toString false)
                |> List.append (ii |> List.collect Item.toString )



            module Dto =

                module Units = ValueUnit.Units
                module Id = WrappedString.Id
                module Name = WrappedString.Name
                module Quantity = OrderVariable.Quantity
                module Concentration = OrderVariable.Concentration
                module CT = OrderVariable.Count


                type Dto () =
                    member val Id = "" with get, set
                    member val Name = "" with get, set
                    member val Shape = "" with get, set
                    member val ComponentQuantity = OrderVariable.Dto.dto () with get, set
                    member val OrderableQuantity = OrderVariable.Dto.dto () with get, set
                    member val OrderableCount = OrderVariable.Dto.dto () with get, set
                    member val OrderQuantity = OrderVariable.Dto.dto () with get, set
                    member val OrderCount = OrderVariable.Dto.dto () with get, set
                    member val OrderableConcentration = OrderVariable.Dto.dto () with get, set
                    member val Dose = Dose.Dto.dto () with get, set
                    member val Items : Item.Dto.Dto list = [] with get, set


                let fromDto (dto: Dto) =

                    let id = dto.Id |> Id.create
                    let n = dto.Name |> Name.fromString
                    let s = dto.Shape
                    let cmp_qty = dto.ComponentQuantity |> Quantity.fromDto
                    let orb_qty = dto.OrderableQuantity |> Quantity.fromDto
                    let orb_cnt = dto.OrderableCount    |> Count.fromDto
                    let orb_cnc = dto.OrderableConcentration |> Concentration.fromDto
                    let ord_qty = dto.OrderQuantity |> Quantity.fromDto
                    let ord_cnt = dto.OrderCount    |> Count.fromDto
                    let ii =
                        dto.Items
                        |> List.map Item.Dto.fromDto

                    let dos = dto.Dose |> Dose.Dto.fromDto

                    create id n s cmp_qty orb_qty orb_cnt ord_qty ord_cnt orb_cnc dos ii


                let toDto (cmp : Component) =
                    let dto = Dto ()

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
                    dto.Dose <-
                        cmp.Dose
                        |> Dose.Dto.toDto
                    dto.Items <-
                        cmp.Items
                        |> List.map Item.Dto.toDto

                    dto

                let dto id orbN cmpN shape =
                    createNew id orbN cmpN shape
                    |> toDto



        module Quantity = OrderVariable.Quantity
        module Concentration = OrderVariable.Concentration
        module Count = OrderVariable.Count


        /// Create an `Orderable` with
        ///
        /// * nm: the name of the orderable
        /// * orb\_qty: quantity of the orderable
        /// * ord\_qty: quantity of orderable in the order
        /// * orb\_cnt: the count of orderable in the order
        /// * dos: the orderable dose
        /// * dos\_adj: the adjusted orderable dose
        let create n orb_qty ord_qty ord_cnt dos_cnt dos cc =
            {
                Name = n
                OrderableQuantity = orb_qty
                OrderQuantity = ord_qty
                OrderCount = ord_cnt
                DoseCount = dos_cnt
                Dose = dos
                Components = cc
            }

        /// Create a new `Orderable` with a `Component` list
        /// `cl`, and
        /// * `Orderable`unit `un` and
        /// * component unit `cu`
        /// * time unit `tu`
        /// * adjust unit `adj`
        let createNew id orbN =
            let un = ValueUnit.NoUnit
            let n = [id; orbN] |> Name.create

            let orb_qty = let n = n |> Name.add Literals.orderable in Quantity.create n un
            let ord_qty = let n = n |> Name.add Literals.order in Quantity.create n un
            let ord_cnt = let n = n |> Name.add Literals.order in Count.create n
            let dos_cnt = let n = n |> Name.add Literals.dose in Count.create n
            let dos     = Dose.createNew n

            create (orbN |> Name.fromString) orb_qty ord_qty ord_cnt dos_cnt dos []


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


        // Get the base `Unit` of an `Orderable`
        let getUnit orb =
            (orb |> get).OrderableQuantity
            |> Quantity.toOrdVar
            |> OrderVariable.getUnit


        /// Map an `Orderable` **orb** to
        /// `VariableUnit`s
        let toOrdVars orb =
            let ord_qty = (orb |> get).OrderQuantity |> Quantity.toOrdVar
            let orb_qty = orb.OrderableQuantity      |> Quantity.toOrdVar
            let ord_cnt = orb.OrderCount             |> Count.toOrdVar
            let dos_cnt = orb.DoseCount              |> Count.toOrdVar

            [
                ord_qty
                orb_qty
                ord_cnt
                dos_cnt
                yield! orb.Dose |> Dose.toOrdVars
                yield! orb.Components |> List.collect Component.toOrdVars
            ]


        /// Map an `Orderable` **orb** to
        /// `VariableUnit`s
        let fromOrdVars ovars orb =
            let ord_qty = (orb |> get).OrderQuantity |> Quantity.fromOrdVar ovars
            let orb_qty = orb.OrderableQuantity      |> Quantity.fromOrdVar ovars
            let ord_cnt = orb.OrderCount             |> Count.fromOrdVar ovars
            let dos_cnt = orb.DoseCount              |> Count.fromOrdVar ovars
            let dos = orb.Dose |> Dose.fromOrdVars ovars

            orb.Components
            |> List.map (Component.fromOrdVars ovars)
            |> create orb.Name orb_qty ord_qty ord_cnt dos_cnt dos


        /// Turn an `Orderable` `ord` into
        /// a list of strings.
        let toString orb =
            let cc = orb.Components

            orb
            |> toOrdVars
            |> List.map (OrderVariable.toString false)
            |> List.append (cc |> List.collect Component.toString )
            |> List.sort



        module Dto =

            module Units = ValueUnit.Units
            module Id = WrappedString.Id
            module Name = WrappedString.Name
            module Quantity = OrderVariable.Quantity
            module Concentration = OrderVariable.Concentration
            module CT = OrderVariable.Count

            type Dto () =
                member val Name = "" with get, set
                member val OrderableQuantity = OrderVariable.Dto.dto () with get, set
                member val OrderQuantity = OrderVariable.Dto.dto () with get, set
                member val OrderCount = OrderVariable.Dto.dto () with get, set
                member val DoseCount = OrderVariable.Dto.dto () with get, set
                member val Dose = Dose.Dto.dto () with get, set
                member val Components : Component.Dto.Dto list = [] with get, set


            let fromDto (dto: Dto) =
                let n = dto.Name |> Name.fromString

                let orb_qty = dto.OrderableQuantity |> Quantity.fromDto
                let ord_qty = dto.OrderQuantity     |> Quantity.fromDto
                let ord_cnt = dto.OrderCount        |> Count.fromDto
                let dos_cnt = dto.DoseCount         |> Count.fromDto

                let cc =
                    dto.Components
                    |> List.map Component.Dto.fromDto

                let dos = dto.Dose |> Dose.Dto.fromDto

                create n orb_qty ord_qty ord_cnt dos_cnt dos cc

            let toDto (orb : Orderable) =
                let dto = Dto ()

                dto.Name <- orb.Name |> Name.toString
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
                dto.Dose <-
                    orb.Dose
                    |> Dose.Dto.toDto
                dto.Components <-
                    orb.Components
                    |> List.map Component.Dto.toDto

                dto


            let dto id orbN =
                createNew id orbN
                |> toDto



    module Prescription =

        open Types

        module Frequency = OrderVariable.Frequency
        module Time = OrderVariable.Time


        /// Create `Frequency` and `Time` with name generated by string list **n**
        let freqTime tu1 tu2 n =  (Frequency.create n tu1, Time.create n tu2)


        /// Create a continuous `Prescription` with name generated by string list **n**
        let continuous tu1 tu2 n =
            let _, _ = n |> freqTime tu1 tu2 in Continuous


        /// Create a discontinuous `Prescription` with name generated by string list **n**
        let discontinuous tu1 tu2 n =
            let frq, _ = n |> freqTime tu1 tu2 in frq |> Discontinuous


        /// Create a timed `Prescription` with name generated by string list **n**
        let timed tu1 tu2 n =
            let frq, tme = n |> freqTime tu1 tu2 in (frq, tme) |> Timed


        /// Check whether a `Prescription` is continuous
        let isContinuous = function | Continuous -> true | _ -> false


        /// Check whether a `Prescription` is discontinuous with a time
        let isTimed = function | Timed _ -> true | _ -> false


        /// Turn `Prescription` **prs** into `VariableUnit`s to
        /// be used in equations
        let toOrdVars prs =
            match prs with
            | Continuous -> None, None
            | Discontinuous frq ->
                frq |> Frequency.toOrdVar |> Some, None
            | Timed(frq, tme)     ->
                frq |> Frequency.toOrdVar |> Some, tme |> Time.toOrdVar |> Some


        let fromOrdVars ovars prs =
            match prs with
            | Continuous -> prs
            | Discontinuous frq ->
                frq |> Frequency.fromOrdVar ovars |> Discontinuous
            | Timed(frq, tme)     ->
                (frq |> Frequency.fromOrdVar ovars,
                tme |> Time.fromOrdVar ovars)
                |> Timed



        /// Turn a `Prescription` **prs** into
        /// a string list
        let toString (prs: Prescription) =
                match prs with
                | Continuous -> ["Continuous"]
                | Discontinuous frq -> [frq |> Frequency.toString]
                | Timed(frq, tme)     -> [frq |> Frequency.toString; tme |> Time.toString]


        module Dto =

            module Units = ValueUnit.Units
            module Id = WrappedString.Id
            module NM = Name

            type Dto () =
                member val IsContinuous = false with get, set
                member val IsDiscontinuous = false with get, set
                member val IsTimed = false with get, set
                member val Frequency = OrderVariable.Dto.dto () with get, set
                member val Time = OrderVariable.Dto.dto () with get, set

            let fromDto (dto : Dto) =
                match dto.IsContinuous,
                      dto.IsDiscontinuous,
                      dto.IsTimed with
                | true,  false, false -> Continuous
                | false, true,  false ->
                    dto.Frequency
                    |> Frequency.fromDto
                    |> Discontinuous
                | false, false, true  ->
                    (dto.Frequency |> Frequency.fromDto, dto.Time |> Time.fromDto)
                    |> Timed
                | _ -> exn "dto is neither or both process, continuous, discontinuous or timed"
                       |> raise

            let toDto pres =
                let dto = Dto ()

                match pres with
                | Continuous -> dto.IsContinuous <- true
                | Discontinuous freq ->
                    dto.IsDiscontinuous <- true
                    dto.Frequency <- freq |> Frequency.toDto
                | Timed (freq, time) ->
                    dto.IsTimed <- true
                    dto.Frequency <- freq |> Frequency.toDto
                    dto.Time      <- time |> Time.toDto

                dto

            let dto n =
                let dto  = Dto ()
                let f, t =
                    n
                    |> Name.fromString
                    |> freqTime ValueUnit.NoUnit ValueUnit.NoUnit

                dto.Frequency <- f |> Frequency.toDto
                dto.Time <- t |> Time.toDto
                dto.IsDiscontinuous <- true

                dto

            let setToContinuous (dto : Dto) =
                dto.IsContinuous <- true
                dto.IsDiscontinuous <- false
                dto.IsTimed <- false
                dto

            let setToDiscontinuous (dto : Dto) =
                dto.IsContinuous <- false
                dto.IsDiscontinuous <- true
                dto.IsTimed <- false
                dto

            let setToTimed (dto : Dto) =
                dto.IsContinuous <- false
                dto.IsDiscontinuous <- false
                dto.IsTimed <- true
                dto



    /// Types and functions that
    /// model a start and stop date time
    /// of an `Order`
    module StartStop =

        let toString startStop =
            match startStop with
            | Start dt ->
                dt
                |> DateTime.formattedString "dd-MM-yy"
                |> sprintf "%s"
            | StartStop (start, stop) ->
                stop
                |> DateTime.formattedString "dd-MM-yy"
                |> sprintf "%s - %s" (start |> DateTime.formattedString "dd-MM-yy")



    module ValueRange = Informedica.GenSolver.Lib.Variable.ValueRange
    module Equation = Informedica.GenSolver.Lib.Equation
    module Property = ValueRange.Property
    module Quantity = OrderVariable.Quantity
    module Frequency = OrderVariable.Frequency
    module QuantityPerTimeAdjust = OrderVariable.QuantityPerTimeAdjust
    module Concentration = OrderVariable.Concentration
    module Rate = OrderVariable.Rate
    module RateAdjust = OrderVariable.RateAdjust
    module Time = OrderVariable.Time
    module Units = ValueUnit.Units

    type Equation = Informedica.GenSolver.Lib.Types.Equation


    /// Apply `f` to `Order` `ord`
    let apply f (ord: Order) = ord |> f


    /// Utility function to facilitate type inference
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
    let create id adj_qty orb prs rte tme sts =
        {
            Id = id
            Adjust = adj_qty
            Orderable = orb
            Prescription = prs
            Route = rte
            Duration = tme
            StartStop = sts
        }


    let createNew id orbN str_prs route =
        let orb = Orderable.createNew id orbN
        let n = [id] |> Name.create

        let adj =
            Quantity.create (n |> Name.add Mapping.adj) ValueUnit.NoUnit

        let tme =
            Time.create (n |> Name.add Mapping.ord) ValueUnit.NoUnit

        let prs =
            n
            |> Name.add Mapping.prs
            |> str_prs
        let sts = DateTime.Now  |> StartStop.Start

        create (id |> Id.create) adj orb prs route tme sts


    let getAdjust ord = (ord |> get).Adjust


    let getOrderable ord = (ord |> get).Orderable


    /// Turn an order into a list of string
    /// representing variable name, valuerange
    /// and unit group
    let toString (ord: Order) =
        [ ord.Adjust |> Quantity.toString ]
        |> List.append (Orderable.Literals.orderable::(ord.Orderable |> Orderable.toString))
        |> List.append ("Prescription"::(ord.Prescription |> Prescription.toString))
        |> List.append ("Route"::[ord.Route])
        |> List.filter (String.isNullOrWhiteSpace >> not)


    /// Map an `Orderable` **orb** to
    /// `VariableUnit`s
    let toOrdVars (ord : Order) =
        let adj_qty = ord.Adjust |> Quantity.toOrdVar
        let ord_tme = ord.Duration |> Time.toOrdVar

        let prs_vars =
            ord.Prescription
            |> Prescription.toOrdVars
            |> fun  (f, t) ->
                [f; t]
                |> List.choose id
        [
            adj_qty
            ord_tme
            yield! prs_vars
            yield! ord.Orderable |> Orderable.toOrdVars
        ]


    let fromOrdVars ovars (ord : Order) =
        { ord with
            Adjust = ord.Adjust |> Quantity.fromOrdVar ovars
            Duration = ord.Duration |> Time.fromOrdVar ovars
            Prescription = ord.Prescription |> Prescription.fromOrdVars ovars
            Orderable = ord.Orderable |> Orderable.fromOrdVars ovars
        }


    let mapToEquations eqs (ord: Order)  =
        let ovars = ord |> toOrdVars

        let map repl eqs =
            let eqs, c =
                match eqs with
                | SumMapping eqs -> eqs, OrderSumEquation
                | ProductMapping eqs -> eqs, OrderProductEquation
            eqs
            |> List.map (String.replace "=" repl)
            |> List.map (String.split repl >> List.map String.trim)
            |> List.map (fun xs ->
                match xs with
                | h::rest ->
                    let h =
                        try
                            ovars |> List.find (fun v -> v.Variable.Name |> Name.toString = h)
                        with
                        | _ -> failwith $"cannot find {h} in {ovars}"
                    let rest =
                        rest
                        |> List.map (fun s ->
                            try
                                ovars |> List.find (fun v -> v.Variable.Name |> Name.toString = s)
                            with
                            | _ -> failwith $"cannot find {s} in {ovars}"
                        )
                    (h, rest) |> c
                | _ -> failwith $"cannot map {eqs}"
            )

        let sumEqs, prodEqs = eqs

        sumEqs |> map "+"
        |> List.append (prodEqs |> map "*")


    let mapFromEquations (ord: Order) eqs =
        let ovars =
            eqs
            |> List.collect (fun e ->
                match e with
                | OrderProductEquation (y, xs)
                | OrderSumEquation (y, xs) -> y::xs
            )
            |> List.distinct

        ord |> fromOrdVars ovars


    let solveMinMax logger (ord: Order) =
        let mapping =
            Mapping.getEquations 5
            |> Mapping.getEqsMapping ord

        let oEqs =
            ord
            |> mapToEquations mapping
            |> Solver.solveUnits logger
            |> Solver.orderEqsToBase

        oEqs
        |> Solver.mapToSolverEqs
        |> Solver.solveMinMax logger
        |> Solver.mapToOrderEqs oEqs
        |> Solver.orderEqsToUnit
        |> mapFromEquations ord


    module Print =

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
            o.Orderable.Dose.Quantity
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
                        (fun i -> i.Dose.Quantity)
                        (Quantity.toValueUnitStringList (Some 3))

                let dt =
                    o
                    |> printItem
                        (fun i -> i.Dose.PerTimeAdjust)
                        (QuantityPerTimeAdjust.toValueUnitStringList (Some 2))

                let pres = $"{o.Orderable.Name |> Name.toString} {fr} {dq} ({dt})"
                let prep = $"{o |> printComponentQuantity}"
                let adm = $"{fr} {o |> printOrderableDoseQuantity}"

                pres, prep, adm

            | Prescription.Continuous ->
                // infusion rate
                let rt =
                    o.Orderable.Dose.Rate
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
                        (fun i -> i.Dose.RateAdjust)
                        (RateAdjust.toValueUnitStringList (Some 2))

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
                    o.Orderable.Dose.Rate
                    |> Rate.toValueUnitStringList (Some 1)
                    |> Seq.map snd
                    |> String.concat ""

                let dq =
                    o
                    |> printItem
                        (fun i -> i.Dose.Quantity)
                        (Quantity.toValueUnitStringList (Some 3))

                let dt =
                    o
                    |> printItem
                        (fun i -> i.Dose.PerTimeAdjust)
                        (QuantityPerTimeAdjust.toValueUnitStringList (Some 1))

                let pres = $"{o.Orderable.Name |> Name.toString} {fr} {dq} = ({dt}) {rt}"
                let prep = o |> printComponentQuantity
                let adm = $"{fr} {o |> printOrderableDoseQuantity} in {tme}, {rt}"

                pres, prep, adm



    module Dto =

        type Dto (id , n) =
            member val Id = id with get, set
            member val Adjust = OrderVariable.Dto.dto () with get, set
            member val Orderable = Orderable.Dto.dto id n with get, set
            member val Prescription = Prescription.Dto.dto n with get, set
            member val Route = "" with get, set
            member val Duration = OrderVariable.Dto.dto () with get, set
            member val Start = DateTime.now () with get, set
            member val Stop : DateTime option = None with get, set


        let fromDto (dto : Dto) =
            let id = dto.Id |> Id.create
            let adj_qty = dto.Adjust |> Quantity.fromDto
            let ord_tme = dto.Duration |> Time.fromDto
            let orb = dto.Orderable |> Orderable.Dto.fromDto
            let prs = dto.Prescription |> Prescription.Dto.fromDto
            let sts =
                match dto.Stop with
                | Some dt -> (dto.Start, dt) |> StartStop.StartStop
                | None -> dto.Start |> StartStop.Start

            create id adj_qty orb prs dto.Route ord_tme sts


        let toDto (ord : Order) =
            let id = ord.Id |> Id.toString
            let n = ord.Orderable.Name |> Name.toString
            let dto = Dto (id, n)

            dto.Adjust <- ord.Adjust |> Quantity.toDto
            dto.Duration <- ord.Duration |> Time.toDto
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


        let dto id orbN rte cmps str_prs =
            let dto =
                createNew id orbN str_prs rte
                |> toDto

            dto.Orderable.Components <-
                [
                    for cmpN, shape, itms in cmps do
                        let c = Orderable.Component.Dto.dto id orbN cmpN shape
                        c.Items <-
                            itms
                            |> List.map (Orderable.Item.Dto.dto id orbN cmpN)
                        c
                ]

            dto


        let continuous id orbN rte cmps  =
            Prescription.continuous ValueUnit.NoUnit ValueUnit.NoUnit
            |> dto id orbN rte cmps


        let discontinuous id orbN rte cmps =
            Prescription.discontinuous ValueUnit.NoUnit ValueUnit.NoUnit
            |> dto  id orbN rte cmps


        let timed  id orbN rte cmps=
            Prescription.timed ValueUnit.NoUnit ValueUnit.NoUnit
            |> dto id orbN rte cmps


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



[ "paracetamol", "tablet", ["paracetamol"] ]
|> Order.Dto.discontinuous "2" "paracetamol" "or"
|> Order.Dto.fromDto
|> Order.toOrdVars
|> List.map (fun ovar -> ovar.Variable.Name |> Name.toString)
|> List.iteri (printfn "%i. %s")


let ord =
    [
        "gentamicin", "infusion fluid", ["gentamicin"]
    ]
    |> Order.Dto.timed "1" "gentamicin" "iv"
    |> Order.Dto.fromDto


ord
|> Order.solveMinMax { Log = ignore }
|> Order.toString




