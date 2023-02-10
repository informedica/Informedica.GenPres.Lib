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
    

    open MathNet.Numerics
    open Informedica.GenUnits.Lib

    let unitMapping =
        [
            ("day",  Units.Time.day)
            ("day2",  Units.Time.nDay 2N)
            ("day3",  Units.Time.nDay 3N)
            ("dropl",  Units.General.general "druppel")
            ("FIP",  Units.General.general "FIP")
            ("g",  Units.Mass.gram)
            ("hour",  Units.Time.hour)
            ("hour36",  Units.Time.nHour 36N)
            ("IE",  Units.InterNatUnit.iu)
            ("kg",  Units.Weight.kiloGram)
            ("m2",  Units.BSA.m2)
            ("mcg",  Units.Mass.microGram)
            ("mg",  Units.Mass.milliGram)
            ("min",  Units.Time.minute)
            ("ml",  Units.Volume.milliLiter)
            ("mmol",  Units.Molar.milliMol)
            ("month",  Units.Time.month)
            ("ng",  Units.Mass.nanoGram)
            ("piece",  Units.General.general "stuk")
            ("puff",  Units.General.general "puff")
            ("times",  Units.Count.times)
            ("week",  Units.Time.week)
            ("week13",  Units.Time.nWeek 13N)
            ("week2",  Units.Time.nWeek 2N)
            ("week4",  Units.Time.nWeek 4N)
        ]
