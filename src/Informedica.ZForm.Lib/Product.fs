namespace Informedica.ZForm.Lib

module Product =

    open System
    open MathNet.Numerics

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL

    open Informedica.GenUnits.Lib
    open Informedica.GenUnits.Lib.Api

    open Informedica.ZIndex.Lib


    /// A product is a medical substance or
    /// substances with a specific shape
    type Product =
        {
            // The name of the product which is the generic
            // substance of the product or a concatenation of
            // generic substance names or a 'name'.
            Name : string
            /// The pharmacological shape of a product.
            DisplayName : string
            Synonyms : string list
            Shape : string
            /// The route of a product
            Unit : string
            Routes : Route.Route list
            Pharmacologic : string list
            /// The display name of the generic
            DivisibleBy : Divisibility
            GenericProducts : GenericProduct List
        }
    and Divisibility = NoDiv | Div of bigint
    and GenericProduct =
        {
            Id : int
            Label : string
            /// The substances on which the concentration and dosing is based.
            Substances : Substance list
            TradeProducts : TradeProduct list
        }
    and Substance =
        {
            Name : string
            Concentration : ValueUnit.ValueUnit
        }
    and TradeProduct =
        {
            Id : int
            Names : string
            Label : string
            Quantity : ValueUnit.ValueUnit
        }


    let empty =
        {
            Name = ""
            DisplayName = ""
            Synonyms = []
            Shape = ""
            Unit = ""
            Routes = []
            Pharmacologic = []
            DivisibleBy = NoDiv
            GenericProducts = []
        }


