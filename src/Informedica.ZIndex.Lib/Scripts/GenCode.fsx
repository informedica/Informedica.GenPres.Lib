
#load "loadGenCode.fsx"

#time

open System

open Informedica.GenProduct.Lib
open Informedica.Utils.Lib

// File
File.exists <| FilePath.GStandPath + "BST000T"

CodeGen.generateZIndex (CodeGen.tabelList)
|> File.writeTextToFile "Zindex.fs"

Parser.getData BST000T.name BST000T.posl BST000T.pickList

Environment.CurrentDirectory

BST001T.records ()
BST000T.records ()

