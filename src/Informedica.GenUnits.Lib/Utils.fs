namespace Informedica.GenUnits.Lib


module Utils =  
    
    module Nullable =
    
        let isNull x = x = Unchecked.defaultof<_>
        let isNotNull x = x |> isNull |> not

        let (|NotNull|IsNull|) x = 
            if x |> isNull then IsNull else NotNull


