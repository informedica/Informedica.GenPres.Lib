namespace Informedica.ZIndex.Lib


module ConsumerProduct =

    open Informedica.Utils.Lib

    type ConsumerProduct =
        {
            Id : int
            Name : string
            Label : string
            Quantity : float
            Container : string
        }

    let create id nm lb qt ct =
        {
            Id = id
            Name = nm
            Label = lb
            Quantity = qt
            Container = ct
        }

    let _get id =
        Zindex.BST004T.records ()
        |> Array.filter (fun r ->
            r.MUTKOD <> 1 &&
            r.HPKODE = id
        )
        |> Array.map (fun r ->
            let nm = Names.getName r.ATNMNR Names.Full
            let lb = Names.getName r.ATNMNR Names.Label
            let ct = Names.getThes r.VPDLOM Names.ConsumerContainer Names.Fifty

            create r.ATKODE nm lb r.VPDLHV ct
        )

    let get : int -> ConsumerProduct [] = Memoization.memoize _get
