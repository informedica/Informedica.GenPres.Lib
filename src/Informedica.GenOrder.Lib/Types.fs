namespace Informedica.GenOrder.Lib

open System
open MathNet.Numerics


module Types =

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
            /// Stores the unit group
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
            /// The `Item` `Dose`, i.e. quanity, total and rate of `Item` administered
            Dose: Dose
            // The `Item` `DoseAdjust`,  i.e. adjusted quanity, total and rate of `Item` administered
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
            /// i.e. quanity, total and rate of `Component` administered
            Dose: Dose
            // The `Component` `DoseAdjust`,  
            /// i.e. adjusted quanity, total and rate of `Component` administered
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


    /// Type that represents a prescription
    type Prescription = 
        /// A process
        | Process
        /// A continuous infusion
        | Continuous
        /// A discontinuous presciption with a frequency
        | Discontinuous of Frequency
        /// A discontinuous prescription with both frequency and time
        | Timed of Frequency * Time



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

    /// Mapping of an order to variables
    type OrderMapping =
        | PresFreq
        | PresTime
        | ItemComponentQty
        | ItemOrderableQty
        | ItemComponentConc
        | ItemOrderableConc
        | ItemDoseQty
        | ItemDoseTotal
        | ItemDoseRate
        | ItemDoseAdjustQtyAdjust
        | ItemDoseAdjustTotalAdjust
        | ItemDoseAdjustRateAdjust
        | ComponentComponentQty
        | ComponentOrderableQty
        | ComponentOrderableCount
        | ComponentOrderCount
        | ComponentOrderableConc
        | ComponentDoseQty
        | ComponentDoseTotal
        | ComponentDoseRate
        | ComponentDoseAdjustQtyAdjust
        | ComponentDoseAdjustTotalAdjust
        | ComponentDoseAdjustRateAdjust
        | OrderableOrderableQty
        | OrderableOrderQty
        | OrderableOrderCount
        | OrderableDoseCount
        | OrderableDoseQty
        | OrderableDoseTotal
        | OrderableDoseRate
        | OrderableDoseAdjustQtyAdjust
        | OrderableDoseAdjustTotalAdjust
        | OrderableDoseAdjustRateAdjust
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
            Limit : Limit
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
            /// The "divisibility" of the products
            /// Note: should be a product attribute
            Divisible : BigRational
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
            /// The time unit used for frequency
            TimeUnit : string
            /// The rate unit used for rate
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
    type ConstrainedOrder = (DrugConstraint list * Order)

    /// The dose limits that can be applied
    type DoseLimits =
        {
            /// The name of the order variable to
            /// which the 
            Name : string
            Frequencies : BigRational list
            Rates : BigRational list
            /// The substance name to which the dose limits
            /// are applied
            SubstanceName : string
            MaxDoseQuantity : BigRational option
            MinDoseQuantity : BigRational option
            MaxDoseQuantityAdjust : BigRational option
            MinDoseQuantityAdjust : BigRational option
            MaxDoseTotal : BigRational option
            MinDoseTotal : BigRational option
            MaxDoseTotalAdjust : BigRational option
            MinDoseTotalAdjust : BigRational option
            MaxDoseRate : BigRational option
            MinDoseRate : BigRational option
            MaxDoseRateAdjust : BigRational option
            MinDoseRateAdjust : BigRational option
        }


    type SolutionLimits =
        {
            Name : string
            Component : string
            MinConcentration : BigRational option
            MaxConcentration : BigRational option
            DoseCount : BigRational option
            MinTime : BigRational option
            MaxTime : BigRational option
        }


    type Scenario =
        {
            No : int
            Name : string
            Shape : string
            Route : string
            Prescription : string
            Preparation : string
            Administration : string
        }


    module Events =

        type Event =
            | SolverReplaceUnit of (Name * Unit)
            | OrderSolved of Order
            | OrderConstraintsSolved of Order * Constraint list
            | OrderScenario of string
            | OrderScenerioWithNameValue of Order * Name * BigRational



    module Logging =

        open Informedica.GenSolver.Lib.Types.Logging

        type Message =
            | OrderException of string
            | OrderMessage of Events.Event
            interface IMessage
