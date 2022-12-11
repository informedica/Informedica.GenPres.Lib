namespace Informedica.GenOrder.Lib


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
            Constraints : Constraints
            /// Stores the values/range
            Variable:  Variable
            /// Stores the unit
            Unit: Unit
        }
    and Constraints =
        {
                Min : Minimum option
                Max : Maximum option
                Incr : Increment option
                Values : ValueSet option
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
    type PerTime = PerTime of OrderVariable

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
    type PerTimeAdjust = PerTimeAdjust of OrderVariable


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
            PerTime : PerTime
            Rate : Rate
            Total : Total
            QuantityAdjust : QuantityAdjust
            PerTimeAdjust : PerTimeAdjust
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


    type Gender = Male | Female | AnyGender


    type MinMax = { Minimum : BigRational option; Maximum : BigRational option }



    type Patient =
        {
            Diagnosis : string
            Gender : Gender
            Age : MinMax
            Weight : MinMax
            BSA : MinMax
            GestAge : MinMax
            PMAge : MinMax
        }



    /// The dose limits that can be applied
    type DoseLimit =
        {
            Substance : string
            NormQuantity : BigRational option
            Quantity : MinMax
            NormQuantityAdjust : BigRational option
            QuantityAdjust : MinMax
            NormPerTime : BigRational option
            PerTime : MinMax
            NormPerTimeAdjust : BigRational option
            PerTimeAdjust : MinMax
            NormRate : BigRational option
            Rate : MinMax
            NormRateAdjust : BigRational option
            RateAdjust : MinMax
        }


    type DoseRule =
        {
            // selector properties
            Indication : string
            Generic : string
            Shape : string
            Route : string
            Department : string
            Patient : Patient
            /// maps to OrderTyp
            OrderType : string
            /// maps to PresFreq
            Frequencies : BigRational list
            /// maps to OrderableDoseRate
            Rates : BigRational list
            /// maps to OrderableQuantity
            Quantities : BigRational list
            Time : MinMax
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
            Concentration : MinMax
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
            /// The route by which the order is applied
            Route : string
            // The type of order
            OrderType : OrderType
            Frequencies : BigRational list
            Rates : BigRational list
            DoseQuantities : BigRational list
            DoseCount : BigRational option
        }
    /// The product components that are used by the drug order
    and ProductComponent =
        {
            /// The name of the product
            Name : string
            /// The shape of the product
            Shape : string
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
            Dose : DoseLimit
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
            Id : string
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


