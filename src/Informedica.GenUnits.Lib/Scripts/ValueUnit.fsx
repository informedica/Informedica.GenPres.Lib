
//#I __SOURCE_DIRECTORY__

#load "load-utils.fsx"
#load "load.fsx"

open MathNet.Numerics

open Informedica.GenUnits.Lib
open Informedica.Utils.Lib.BCL
open ValueUnit

let toString = toString Units.English Units.Short

let (>==>) vu f =
    vu 
    |> toString
    |> printfn "%s"
    f vu

let x = create Units.Count.times
let mg = create Units.Mass.milliGram
let mcg = create Units.Mass.microGram
let ml = create Units.Volume.milliLiter
let kg = create Units.Weight.kiloGram
let hr = create Units.Time.hour
let day = create Units.Time.day
let min = create Units.Time.minute

let mcgkgmin = Units.Mass.microGram |> per Units.Weight.kiloGram |> per Units.Time.minute
let mgkgday = Units.Mass.milliGram |> per Units.Weight.kiloGram |> per Units.Time.day
let microgkgday = Units.Mass.microGram |> per Units.Weight.kiloGram |> per Units.Time.day

(200N |> mg)
>==> (fun vu -> vu / (5N |> ml)) // Concentration
>==> (fun vu -> vu * (2N |> ml)) // take 2 ml
>==> (fun vu -> vu / (50N |> ml)) // dissolve in 50 ml
>==> (fun vu -> vu * ((1N |> ml) / (1N |> hr))) // infusion speed
>==> (fun vu -> vu / (5N |> kg)) // for 5 kg patient
>==> convertTo mcgkgmin // Convert to dose unit
>==> toString

(8N / 25N) |> create (Units.Mass.milliGram |> per Units.Time.hour |> per Units.Weight.kiloGram)
|> convertTo mcgkgmin
|> convertTo (Units.Mass.microGram |> per Units.Weight.kiloGram |> per Units.Time.minute)

((400N |> mg) /  (20N |> ml)) * (((10N |> ml) + (10N |> ml)) / (1N |> hr))


createCombiUnit (Count(Times(2N)), OpPer, Time(Hour(1N)))
|> (fun u -> createCombiUnit ((Mass(MilliGram(1N)), OpTimes, u)))


// Calculate (20 mg + 30 ml) * 3 / (1 day) / (8 kg) and express this
// as mg/kg/day
((20N |> mg) + (30N |> mg)) / ((3N |> x) / (1N |> day)) / (8N |> kg)
==> mgkgday

(40N |> mg) / (1N |> ml)


let dripMgPerHour n = (n |> mg) / (1N |> hr)



let test () =
    // Creeer een waarde met de eenheid mg/kg/dag
    let mgkgday = create  (Units.Mass.milliGram |> per Units.Weight.kiloGram |> per Units.Time.day)
    // Creeer een waarde met de eenheid mcg/kg/dag
    let microgkgday = create (Units.Mass.microGram |> per Units.Weight.kiloGram |> per Units.Time.day)

    // Kijk of 1 mcg/kg/dag kleiner is dan 2/1000 = 0.002 mg/kg/dag
    // zonder rekening te houden met de eenheid
    (1N |> microgkgday) < ((2N / 1000N) |> mgkgday)
    // Print false = onwaar
    |> printfn "%A"
    // Vergelijk de waarde volgens de bijbehorende eenheid
    (1N |> microgkgday) <? ((2N / 1000N) |> mgkgday)
    // Print true = waar
    |> printfn "%A"

let mg1 = 1N |> create Units.Mass.milliGram
let piece = 1N |> create (Units.General.general "piece")
[mg1; mg1; piece] |> List.reduce (/) |> ValueUnit.get |> snd

module Demo =

    let create u v =
        v
        |> BigRational.FromInt
        |> create u

    let x = create Units.Count.times
    let mg = create Units.Mass.milliGram
    let mcg = create Units.Mass.microGram
    let ml = create Units.Volume.milliLiter
    let kg = create Units.Weight.kiloGram
    let hr = create Units.Time.hour
    let day = create Units.Time.day
    let min = create Units.Time.minute

    let mcgkgmin = Units.Mass.microGram |> per Units.Weight.kiloGram |> per Units.Time.minute

    let print vu =
        let v, u = vu |> ValueUnit.get

        v 
        |> BigRational.toFloat
        |> Double.fixPrecision 2
        |> string
        |> (fun s -> 
            s + " "  + 
            (u |> ValueUnit.Units.toString ValueUnit.Units.Dutch ValueUnit.Units.Short)
            |> String.removeTextBetweenBrackets
        )
        |> printfn "%s"

    // Calculation for a dopamin infusion for a patient with weight 10 kg
    // - Take 5 ml of dopamine 40 mg/ml
    // - Add 45 ml of saline for a 50 ml pump
    // - Set the pump rate to 1 ml/hour
    //
    // Calculate ((dopamine 40 mg/ml * 5 ml + saline 45 ml) * (1 ml / hour)) / 10 kg = ... mcg/kg/min
    (((40 |> mg) / (1 |> ml)) * (5 |> ml) / ((5 |> ml) + (45 |> ml))) * ((1 |> ml) / (1 |> hr)) / (10 |> kg)
    // Convert the result to mcg/kg/min
    ==> mcgkgmin
    // Print the result
    |> print
    // Prints : 6.7 mcg/kg/min

    Api.eval "100 mg[Mass] * 1 gram[Mass]"

    "10 mg[Mass]/kg[Weight]/day[Time]"
    |> ValueUnit.fromString
    |> ValueUnit.toString ValueUnit.Units.English ValueUnit.Units.Short
    |> ValueUnit.fromString
    |> ValueUnit.toString ValueUnit.Units.English ValueUnit.Units.Short
