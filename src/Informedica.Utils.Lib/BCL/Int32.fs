namespace Informedica.Utils.Lib.BCL
    
module Int32 =

    open System

    let parse s = Int32.Parse(s, Globalization.CultureInfo.InvariantCulture)

    let tryParse (s : string) =
        let (b, n) = Int32.TryParse(s)
        if b then n |> Some else None
