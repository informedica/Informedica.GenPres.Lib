
#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "nuget: Unquote"

#r "../../Informedica.Utils.Lib/bin/Debug/net6.0/Informedica.Utils.Lib.dll"

open System
open Informedica.Utils.Lib


let zindexPath = __SOURCE_DIRECTORY__ |> Path.combineWith "../../../"
Environment.CurrentDirectory <- zindexPath

