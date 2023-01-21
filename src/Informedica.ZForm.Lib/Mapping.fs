namespace Informedica.ZForm.Lib


/// Provide mappings for using units and
/// routes for the G-Stand database, the
/// internal unit structure and an application.
module Mapping =

    open System

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL
    open Informedica.ZIndex.Lib


    let combineWith = Path.combineWith


    [<Literal>]
    let unitPath = "./mapping/UnitMapping.csv"


    [<Literal>]
    let freqPath = "./mapping/FrequencyMapping.csv"


    [<Literal>]
    let routePath = "./mapping/RouteMapping.csv"

    /// The different types of mapping:
    /// * AppMap: mapping for the application
    /// * GStandMap: mapping for the GStand database
    /// * PedFormMap: mapping for the Dutch Pedicatric formulary: www.kinderformularium.nl
    /// * ValueUnitMap: mapping for the internal structure
    type Mapping = AppMap | GStandMap | PedFormMap  | ValueUnitMap


    let getMappingValue = function
    | AppMap -> 0
    | GStandMap -> 1
    | PedFormMap -> 2
    | ValueUnitMap -> 3


    /// Read the mapping from a file
    /// with path `path`. Expecting a csv
    /// file with a ";" delimiter
    let readMappingFile path =
        Environment.CurrentDirectory
        |> combineWith FilePath.data
        |> combineWith path
        |> File.readAllLines
        |> Array.skip 1
        |> Array.map (String.splitAt ';')


    /// Get all values for a mapping file
    /// at path `path` using mapping `m`.
    let getAll path m =
        let n = m |> getMappingValue
        path
        |> readMappingFile
        |> Array.map (fun row ->
            if row |> Array.length < n then ""
            else row[n]
        )

    let allAppUnits () = getAll unitPath AppMap

    let allGStandUnits () = getAll unitPath GStandMap

    let allPedFormUnits () = getAll unitPath PedFormMap

    let allValueUnitUnits () = getAll unitPath ValueUnitMap


    let allAppRoutes () = getAll routePath AppMap

    let allGStandRoutes () = getAll routePath GStandMap

    let allPedFormRoutes () = getAll routePath PedFormMap


    let allAppFreqs () = getAll freqPath AppMap

    let allGStandFreqs () = getAll freqPath GStandMap

    let allPedFormFreqs () = getAll freqPath PedFormMap

    let allValueUnitFreqs () = getAll freqPath ValueUnitMap


    /// Map a string to another string
    /// using the mapping described in the file
    /// in the `path` variable. Map from `m1` to `m2`
    let map path m1 m2 s =
        let eqs s1 s2 =
            let norm s =
                s
                |> String.trim
                |> String.toLower
                |> String.replace "**" " "
                |> String.replace "||" " "

            s1 |> norm = (s2 |> norm)

        let i1, i2 =
            match m1, m2 with
            | AppMap,       GStandMap
            | GStandMap,    AppMap
            | AppMap,       ValueUnitMap
            | GStandMap,    ValueUnitMap
            | ValueUnitMap, AppMap
            | ValueUnitMap, GStandMap    ->
                (m1 |> getMappingValue), (m2 |> getMappingValue)
            | _ -> 0, 0

        let mapping = readMappingFile path
        if i1 = 0 && i2 = 0 || (i1 > mapping.Length || i2 > mapping.Length) then ""
        else
            mapping
            |> Array.filter (fun x -> x[i1] |> eqs s)
            |> Array.fold (fun acc xs ->
                if acc = "" then xs[i2]
                elif xs[i2] = "" then acc
                else acc + "||" + xs[i2]
            ) ""


    /// Map a unit string
    let mapUnit = map unitPath

    /// Map a unit string from the appication to the internal structure
    let mapUnitFromAppToValueUnit = mapUnit AppMap ValueUnitMap

    /// Map a unit from the GStand database to the internal structure
    let mapUnitFromGStandToValueUnit = mapUnit GStandMap ValueUnitMap

    /// Map a frequency string
    let mapFreq = map freqPath

    /// Map a route
    let mapRoute = map routePath


    module MappingTests =


        let tests () =

            // Test all unit mappings
            [ (AppMap, allAppUnits ())
              (GStandMap, allGStandUnits ())
              (PedFormMap, allPedFormUnits ())
              (ValueUnitMap, allValueUnitUnits ()) ]
            |> List.map (fun (m, units) ->
                printfn $"Printing all units for: %A{m}"
                units
                |> Array.iter (fun s -> if s <> "" then s|> printfn "%s")
                (m, units)
            )
            |> (fun units ->
                printfn "Mapping all units"
                [ for m1, us1 in units do
                    for m2, us2 in units do
                        if m1 = m2 then ()
                        else
                            for u1 in us1 do
                                for u2 in us2 do
                                    let s1 = mapUnit m1 m2 u1
                                    let s2 = mapUnit m2 m1 u2
                                    if s1 <> "" then yield s1 |> (sprintf "mapping %A to %A %s -> %s" m1 m2 u1)
                                    if s2 <> "" then yield s2 |> (sprintf "mapping %A to %A %s -> %s" m2 m1 u2) ]
            )
            |> List.distinct
            |> List.sort
            |> List.iter (printfn "%s")

            // Test all route mappings
            [ (AppMap, allAppRoutes ())
              (GStandMap, allGStandRoutes ())
              (PedFormMap, allPedFormRoutes ()) ]
            |> List.map (fun (m, routes) ->
                printfn $"Printing all routes for: %A{m}"
                routes
                |> Array.iter (fun s -> if s <> "" then s|> printfn "%s")
                (m, routes)
            )
            |> (fun routes ->
                printfn "Mapping all routes"
                [ for m1, rts1 in routes do
                    for m2, rts2 in routes do
                        if m1 = m2 then ()
                        else
                            for r1 in rts1 do
                                for r2 in rts2 do
                                    let s1 = mapRoute m1 m2 r1
                                    let s2 = mapRoute m2 m1 r2
                                    if s1 <> "" then yield s1 |> (sprintf "mapping %A to %A %s -> %s" m1 m2 r1)
                                    if s2 <> "" then yield s2 |> (sprintf "mapping %A to %A %s -> %s" m2 m1 r2) ]
            )
            |> List.distinct
            |> List.sort
            |> List.iter (printfn "%s")

            // Test all frequency mappings
            [ (AppMap, allAppFreqs ())
              (GStandMap, allGStandFreqs ())
              (PedFormMap, allPedFormFreqs ())
              (ValueUnitMap, allValueUnitFreqs ()) ]
            |> List.map (fun (m, freqs) ->
                printfn "Printing all freqs for: %A" m
                freqs
                |> Array.iter (fun s -> if s <> "" then s|> printfn "%s")
                (m, freqs)
            )
            |> (fun freqs ->
                printfn "Mapping all freqs"
                [ for m1, fs1 in freqs do
                    for m2, fs2 in freqs do
                        if m1 = m2 then ()
                        else
                            for f1 in fs1 do
                                for f2 in fs2 do
                                    let s1 = mapFreq m1 m2 f1
                                    let s2 = mapFreq m2 m1 f2
                                    if s1 <> "" then yield s1 |> (sprintf "mapping %A to %A %s -> %s" m1 m2 f1)
                                    if s2 <> "" then yield s2 |> (sprintf "mapping %A to %A %s -> %s" m2 m1 f2) ]
            )
            |> List.distinct
            |> List.sort
            |> List.iter (printfn "%s")
