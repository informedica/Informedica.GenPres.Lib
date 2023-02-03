
#load "load.fsx"

#time

open System.Collections.Generic


open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.ZIndex.Lib


// File
File.exists <| FilePath.GStandPath + "BST000T"


Json.clearCache ()
// Load all
printfn "Loading GenPresProduct ..."
GenPresProduct.load true
printfn "Loading ATCGroup ..."
ATCGroup.load ()
printfn "Loading DoseRule ..."
DoseRule.load ()
printfn "Loading Substance"
Substance.load ()


// Print log genericproducts
let logGenericProducts () =
    printfn "Start ..."
    let _log = List<string>()
    let logf s =
        _log.Add(s)
    GenericProduct.getWithLog logf [] |> ignore
    _log.ToArray()
    |> Array.distinct
    |> Array.iter (printfn "%s")

logGenericProducts ()

