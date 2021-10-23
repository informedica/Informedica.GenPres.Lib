namespace Informedica.GenOrder.Lib

/// Types and functions to deal with
/// value primitives
module WrappedString =

    open Types

    /// Type and functions that 
    /// deal with an identifier
    module Id = 

        let create s = s |> Id

        let lift f = fun (Id s) -> s |> f |> create

        let toString (Id s) = s

    /// Helper functions for `Informedica.GenSolver.Variable.Name` type
    module Name =
        
        open Informedica.GenSolver.Lib
        open Informedica.GenSolver.Lib.Types
        
        module Name = Variable.Name

        /// Create a `Name` from a list of strings that 
        let create ns = ns |> String.concat "." |> Name.createExc

        let lift f = fun n -> n |> Name.toString |> f |> Name.createExc

        let toString  = Name.toString


