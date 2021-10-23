namespace Informedica.Utils.Lib.BCL

module DateTime =

    open System

    let apply f (dt: DateTime) = f dt

    let get = apply id

    let create yr mo ds = DateTime(yr, mo, ds)

    /// Returns age in years, months, weeks and days
    /// Given a DateTime 'date1' and a DateTime 'date2'
    /// where 'date1' should be >= 'date2' to get a positive
    /// age, otherwise age will be negative
    let now () = DateTime.Now

    let daysInYear () = ((now ()).AddYears(1) - now ()).Days

    let daysInMonth () = daysInYear () / 12

    let daysInWeek = 7

    let addYears yr dt =
        (dt |> get).AddYears(yr)

    let addMonths mo dt =
        (dt |> get).AddMonths(mo)

    let addWeeks wk dt =
        (dt |> get).AddDays((float wk) * (float daysInWeek))

    let addDays (ds: int) dt =
        (dt |> get).AddDays(ds |> float)


    let age (date1:DateTime) (date2: DateTime) = 

        let inv, date1, date2 = 
            if date1 > date2 then false, date1, date2 else true, date2, date1

        let y, date2 = date1.Year - date2.Year, date2.AddYears(date1.Year - date2.Year)
        let y, date2 = 
            if (date1 - date2).Days < 0 then y - 1, date2.AddYears(-1) else y, date2

        let m, date2 = 
            if date1.Year = date2.Year then 
                date1.Month - date2.Month, date2.AddMonths(date1.Month - date2.Month) 
            else 
                (12 - date2.Month) + date1.Month, date2.AddMonths((12 - date2.Month) + date1.Month)
        let m, date2 = 
            if (date1 - date2).Days < 0 then m - 1, date2.AddMonths(-1) else m, date2

        let d = (date1 - date2).Days
        let d, w = d % 7, d / 7
        
        if inv then -y, -m, -w, -d
        else y, m, w, d


    let ageToDate yr mo wk ds dt =
        (dt |> get)
        |> addYears (-1 * yr)
        |> addMonths (-1 * mo)
        |> addWeeks (-1 * wk)
        |> addDays (-1 * ds)

    let ageNow = age DateTime.Now

    let ageToString  years months weeks days age =
        let pluralize n s = 
            match n with 
            | 0 -> ""
            | 1 -> n.ToString() + " " + (s |> fst)
            | _ -> n.ToString() + " " + (s |> snd)
        let yr, mo, wk, d = age
        let s =
            match yr, mo with 
            | _ when yr > 10 -> pluralize yr years
            | _ when yr > 0  -> pluralize yr years + " " + pluralize mo months
            | _ when mo > 0  -> pluralize mo months + " " + pluralize wk weeks
            | _              -> pluralize wk weeks + " " + pluralize d days
        s.Trim() 

    let ageToStringDutch = ageToString ("jaar", "jaar") 
                                       ("maand", "maanden") 
                                       ("week", "weken") 
                                       ("dag", "dagen")

    let getAgeFromDate = ageNow >> ageToStringDutch



