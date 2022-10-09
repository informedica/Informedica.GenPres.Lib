namespace Informedica.ZIndex.Lib


module Names =

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL


    type Name = Full | Short | Memo | Label


    type Length = TwentyFive | Fifty


    type Item =
        | Shape
        | Route
        | GenericUnit
        | ShapeUnit
        | PrescriptionContainer
        | ConsumerContainer


    let mapItem = function
        | Shape -> 6
        | Route -> 7
        | GenericUnit -> 2
        | ShapeUnit -> 2
        | PrescriptionContainer -> 73
        | ConsumerContainer -> 4


    /// Look in BST020T Namen bestand
    let getName id nm =
        match
            Zindex.BST020T.records ()
            |> Array.tryFind (fun r ->
                r.MUTKOD <> 1 &&
                r.NMNR = id
            ) with
        | Some r ->
            match nm with
            | Full  -> r.NMNAAM
            | Short  -> r.NMNM40
            | Memo  -> r.NMMEMO
            | Label -> r.NMETIK
        | None -> ""


    /// Look in BST902T Therauri totaal
    let getThes id it ln =
        match
            Zindex.BST902T.records ()
            |> Array.tryFind (fun r ->
                r.MUTKOD <> 1 &&
                r.TSITNR = id &&
                it |> mapItem = r.TSNR
            ) with
        | Some r -> match ln with | TwentyFive -> r.THNM25 | Fifty -> r.THNM50
        | None -> ""


    let getItems itm ln =
            Zindex.BST902T.records()
            |> Array.filter (fun r ->
                itm |> mapItem = r.TSNR
            )
            |> Array.map (fun r ->
                match ln with | TwentyFive -> r.THNM25 | Fifty -> r.THNM50
            )
            |> Array.distinct
            |> Array.sort


    let getRoutes =
        fun () ->
            getItems Route TwentyFive
            |> Array.collect (fun rt ->
                rt |> String.splitAt '/'
            )
            |> Array.distinct
            |> Array.sort
        |> Memoization.memoize


    let getShapes =
        fun () ->
            getItems Shape Fifty
        |> Memoization.memoize


    let getGenericUnits =
        fun () ->
            getItems GenericUnit Fifty
        |> Memoization.memoize


    let getShapeUnits =
        fun () ->
            getItems ShapeUnit TwentyFive
        |> Memoization.memoize


    let getPrescriptionContainers =
        fun () ->
            getItems PrescriptionContainer TwentyFive
        |> Memoization.memoize


    let getConsumerContainers =
        fun () ->
            getItems ConsumerContainer TwentyFive
        |> Memoization.memoize


