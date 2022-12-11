namespace Informedica.GenForm.Lib


module Product =

    open Informedica.Utils.Lib.BCL


    let toBrs s =
        s
        |> String.splitAt ';'
        |> Array.choose Double.tryParse
        |> Array.choose BigRational.fromFloat


    let toBrOpt brs = brs |> Array.tryHead


    let tupleBrOpt brs1 brs2 =
        brs1 |> Array.tryHead,
        brs2 |> Array.tryHead


    let filter generic shape (prods : Product array) =
        let eqs s1 s2 =
            let s1 = s1 |> String.trim |> String.toLower
            let s2 = s2 |> String.trim |> String.toLower
            s1 = s2
        prods
        |> Array.filter (fun p -> p.Generic |> eqs generic && p.Shape |> eqs shape)


    let products () =
        Web.getDataFromSheet Web.dataUrlId2 "Products"
        |> fun data ->
            let getColumn =
                data
                |> Array.head
                |> Csv.getStringColumn

            data
            |> Array.tail
            |> Array.map (fun r ->
                let get = getColumn r
                let toBrOpt = toBrs >> toBrOpt

                {|
                    GPK =  get "GPK"
                    ATC = get "ATC"
                    MainGroup = get "MainGroup"
                    SubGroup = get "SubGroup"
                    Generic = get "Generic"
                    TallMan = get "TallMan"
                    Synonyms = get "Synonyms" |> String.split "||" |> List.toArray
                    Product = get "Product"
                    Label = get "Label"
                    Shape = get "Shape"
                    ShapeQuantity = get "ShapeQuantity" |> toBrOpt
                    ShapeUnit = get "ShapeUnit"
                    ShapeVolume = get "ShapeVol" |> toBrOpt
                    Substance = get "Substance"
                    SubstanceQuantity = get "SubstanceQuantity" |> toBrOpt
                    SubstanceUnit = get "SubstanceUnit"
                    MultipleQuantity = get "MultipleQuantity" |> toBrOpt
                    MultipleUnit = get "MultipleUnit"
                    Divisible = get "Divisible" |> toBrOpt
                |}
            )
            |> Array.groupBy (fun r ->
                {
                    GPK =  r.GPK
                    ATC = r.ATC
                    MainGroup = r.MainGroup
                    SubGroup = r.SubGroup
                    Generic = r.Generic
                    TallMan = r.TallMan
                    Synonyms = r.Synonyms
                    Product = r.Product
                    Label = r.Label
                    Shape = r.Shape
                    ShapeQuantity = r.ShapeQuantity
                    ShapeUnit = r.ShapeUnit
                    ShapeVolume = r.ShapeVolume
                    Divisible = r.Divisible
                    Substances = [||]
                }
            )
            |> Array.map (fun (prod, rs) ->
                { prod with
                    Substances =
                        rs
                        |> Array.map (fun r ->
                            {
                                Name = r.Substance
                                Quantity = r.SubstanceQuantity
                                Unit = r.SubstanceUnit
                                MultipleQuantity = r.MultipleQuantity
                                MultipleUnit = r.MultipleUnit
                            }
                        )
                }
            )

    let generics (products : Product array) =
        products
        |> Array.map (fun p ->
            p.Generic
        )
        |> Array.distinct


    let synonyms (products : Product array) =
        products
        |> Array.collect (fun p ->
            p.Synonyms
        )
        |> Array.append (generics products)
        |> Array.distinct


    let shapes  (products : Product array) =
        products
        |> Array.map (fun p -> p.Shape)
        |> Array.distinct

