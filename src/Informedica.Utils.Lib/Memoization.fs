namespace Informedica.Utils.Lib

/// Utility functions to apply memoization
module Memoization =

    open System.Collections.Generic
    
    /// Memoize a function `f` according
    /// to its parameter
    let memoize f =
        let cache = ref Map.empty
        fun x ->
            match (!cache).TryFind(x) with
            | Some r -> r
            | None ->
                let r = f x
                cache := (!cache).Add(x, r)
                r


