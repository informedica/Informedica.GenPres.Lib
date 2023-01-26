namespace Informedica.GenCore.Lib


[<AutoOpen>]
module Measures =


    [<Measure>] type m2

    [<Measure>] type cm

    [<Measure>] type m

    [<Measure>] type kg

    [<Measure>] type gram

    [<Measure>] type day

    [<Measure>] type week

    [<Measure>] type month

    [<Measure>] type year




module Constants =

    

    let [<Literal>] fullTerm = 40<week>


    let [<Literal>] premature = 37<week>


    let [<Literal>] daysInWeek = 7


    let [<Literal>] weeksInYear = 52


    let [<Literal>] monthsInYear = 12


    let [<Literal>] daysInYear = 365



module Conversions =


    let fromInt (one: int<_>) x = x * one


    let fromDec (one: decimal<_>) x = x * one


    let dayFromInt x : int<day> = x |> fromInt 1<day>


    let weekFromInt x : int<week> = x |> fromInt 1<week>


    let monthFromInt x : int<month> = x |> fromInt 1<month>


    let yearFromInt x : int<year> = x |> fromInt 1<year>


    let gramFromInt x : int<gram> = x |> fromInt 1<gram>


    let gramFromDecimal x : decimal<gram> = x |> fromDec 1m<gram>


    let kgFromDecimal x : decimal<kg> = x |> fromDec 1m<kg>


    let cmFromInt x : int<cm> = x |> fromInt 1<cm>


    let cmFromDecimal x : decimal<cm> = x |> fromDec 1m<cm>


    let meterFromInt x : int<m> = x |> fromInt 1<m>


    let meterFromDecimal x : decimal<m> = x |> fromDec 1m<m>


    let weeksToDays (weeks: int<week>) =
        weeks * Constants.daysInWeek
        |> int
        |> dayFromInt


    let daysToWeeks (days: int<day>) =
        (int days) / 7 |> weekFromInt,
        (int days) % 7 |> weekFromInt


    let intYearsToDays (years : int<year>) =
        years * Constants.daysInYear
        |> int
        |> dayFromInt


    let decYearsToDays (years : decimal<year>) =
        years * (decimal Constants.daysInYear)
        |> decimal |> int
        |> dayFromInt


    let kgToGram (kg : decimal<kg>) =
        (decimal kg / 1000m)
        |> gramFromDecimal


    let gramToKg (gram : int<gram>) =
        (decimal gram) / 1000m
        |> kgFromDecimal



