namespace Informedica.GenCore.Lib



module Calculations =


    open Informedica.Utils.Lib.BCL



    module Age =

        let yearsMonthsWeeksDaysToDays y (m : int<month>) w d =
            let dy, dm, dw =
                y |> Conversions.intYearsToDays,
                m |> int |> (*) 30 |> Conversions.dayFromInt,
                w |> Conversions.weeksToDays

            d + dy + dm + dw


        let yearsMonthsWeeksToDaysOpt y m w d =
            let y, m, w, d =
                y |> Option.defaultValue 0<year>,
                m |> Option.defaultValue 0<month>,
                w |> Option.defaultValue 0<week>,
                d |> Option.defaultValue 0<day>

            yearsMonthsWeeksDaysToDays y m w d


        let fromBirthDate bd dt =
            let y, m, w, d = DateTime.age bd dt
            y |> Conversions.yearFromInt,
            m |> Conversions.monthFromInt,
            w |> Conversions.weekFromInt,
            d |> Conversions.dayFromInt


        let toBirthDate dt (ys : int<year>) (ms : int<month>) (ws : int<week>) (ds : int<day>) =
            let ys = int ys * -1
            let ms = int ms * -1
            let ws = int ws * -1
            let ds = int ds * -1

            dt 
            |> DateTime.addYears ys
            |> DateTime.addMonths ms
            |> DateTime.addWeeks ws
            |> DateTime.addDays ds


        let adjustedAge (gestDays: int<day>) (gestWeeks: int<week>) dtBirth dtNow =
            let fullTerm = Constants.fullTerm |> Conversions.weeksToDays
            let age = DateTime.dateDiffDays dtNow dtBirth |> Conversions.dayFromInt
            age - (fullTerm - (gestDays + (gestWeeks |> Conversions.weeksToDays)))


        let postMenstrualAge (actAge: int<day>) (gestWeeks: int<week>) (gestDays: int<day>) =
            (gestWeeks |> Conversions.weeksToDays) + gestDays + actAge
            |> Conversions.daysToWeeks


        let ageToString yrs mos wks dys =
            let yToStr = Conversions.yearToString "year" "years"
            let mToStr = Conversions.monthToString "month" "months"
            let wToStr = Conversions.weekToString "week" "weeks"
            let dToStr = Conversions.dayToString "day" "days"

            [
                yrs |> Option.map yToStr 
                mos |> Option.map mToStr 
                wks |> Option.map wToStr 
                dys |> Option.map dToStr 
            ]
            |> List.choose id
            |> List.filter String.notEmpty
            |> String.concat ", "


        let ageToStringNL yrs mos wks dys =
            let yToStr = Conversions.yearToString "jaar" "jaar"
            let mToStr = Conversions.monthToString "maand" "maanden"
            let wToStr = Conversions.weekToString "week" "weeken"
            let dToStr = Conversions.dayToString "dag" "dagen"

            [
                yrs |> Option.map yToStr 
                mos |> Option.map mToStr 
                wks |> Option.map wToStr 
                dys |> Option.map dToStr 
            ]
            |> List.choose id
            |> List.filter String.notEmpty
            |> String.concat ", "



    module BSA =


        let mosteller = fun w h -> sqrt (w * h / 3600.)

        let duBois = fun w h -> 0.007184 * (w ** 0.425) * (h ** 0.725)

        let haycock = fun w h -> 0.024265 * (w ** 0.5378) * (h ** 0.3964)

        let gehanAndGeorge = fun w h -> 0.0235 * (w ** 0.51456) * (h ** 0.42246)

        let fujimoto = fun w h -> 0.008883 * (w ** 0.444) * (h ** 0.663)


        let calcBSA formula fixPrec (weight : decimal<kg>) (height: decimal<cm>)  =
            let w = decimal weight |> float
            let h = decimal height |> float
            formula w h
            |> decimal
            |> fun x ->
                let x =
                    match fixPrec with
                    | Some p ->
                        x
                        |> Decimal.fixPrecision p
                    | None -> x

                x * 1m<m2>


        let calcDuBois = calcBSA duBois

        let calcMosteller = calcBSA mosteller

        let calcHaycock = calcBSA haycock

        let calcGehanAndGeorge = calcBSA gehanAndGeorge

        let calcFujimoto = calcBSA fujimoto





