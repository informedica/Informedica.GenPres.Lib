
#load "loadGenCode.fsx"

#time

open System

open Informedica.ZIndex.Lib
open Informedica.Utils.Lib

// File
File.exists <| FilePath.GStandPath + "BST000T"

CodeGen.generateZIndex (CodeGen.tabelList)
|> File.writeTextToFile "Zindex.fs"


Parser.getData BST000T.name BST000T.posl BST000T.pickList

Environment.CurrentDirectory

BST001T.records ()
|> Array.filter (fun r -> r.MDBST = "BST000T")
|> Array.iter (printfn "%A")

BST000T.records ()

