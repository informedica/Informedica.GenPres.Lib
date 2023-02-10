
#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: FParsec"
#r "nuget: Newtonsoft.Json"
#r "nuget: Aether"


#r "../../Informedica.Utils.Lib/bin/Debug/net6.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.GenUnits.Lib/bin/Debug/net6.0/Informedica.GenUnits.Lib.dll"
#r "../../Informedica.GenCore.Lib/bin/Debug/net6.0/Informedica.GenCore.Lib.dll"

#load "../Types.fs"
#load "../FilePath.fs"
#load "../Json.fs"
#load "../Parser.fs"
#load "../BST001T.fs"
#load "../Zindex.fs"
#load "../Names.fs"
#load "../Route.fs"


open Informedica.Utils.Lib
open Informedica.ZIndex.Lib


Route.routeMapping ()
|> Array.map (fun r ->
    {| r with 
        mapping = 
            r.name 
            |> Reflection.fromString<Route.Route>
            |> Option.defaultValue Route.NoRoute
    |}    
)
