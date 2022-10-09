namespace Informedica.ZForm.Lib


module Route =

    open Informedica.Utils.Lib.BCL

    // * ARTI/LESION
    // * AURICULAIR
    // * CUTAAN
    // DENTAAL
    // ENDOCERVIC
    // * ENDOTR.PULM
    // * ENDOTRACHEOPULMONAIR
    // * EPIDURAAL
    // EPIDURAAL, INTRATHECAAL, PERINEURAAL
    // EXTRACORPORAAL
    // * GASTR-ENTER
    // * IM
    // * INHALATIE
    // INTRA-ART.
    // INTRA-ARTERIEEL
    // INTRA-ARTICULAIR
    // INTRA-OCUL.
    // INTRA-UTERIEN
    // INTRABURSAAL
    // INTRACARDIAAL
    // INTRACAVERNEUS
    // INTRACORONAIR
    // * INTRADERMAAL
    // INTRALAESIONAAL
    // INTRALYMFATISCH
    // * INTRAMUSCULAIR
    // INTRAMUSCULAIR, INTRAVENEUS
    // INTRAMUSCULAIR, SUBCUTAAN
    // INTRAOSSAAL
    // INTRAPERITONEAAL
    // INTRAPLEURAAL
    // INTRATHECAAL
    // * INTRAVENEUS
    // INTRAVENEUS, SUBCUTAAN
    // * INTRAVESIC.
    // * INTRAVESICAAL
    // INTRAVITR.
    // INTRAVITREAAL
    // * IV
    // * LOKAAL
    // * NASAAL
    // * NEUS
    // NIET GESPEC
    // NVT
    // OOG
    // * OOR
    // * ORAAL
    // ORAAL/RECT
    // * OROMUCOSAAL
    // PAR./ORAAL
    // PAR/UTERIEN
    // PAR/VESICAL
    // PARABULBAIR
    // PARENT/RECT
    // PARENTERAAL
    // PERI-ARTICULAIR
    // PERIBULBAIR
    // PERINEURAAL
    // PERITONEAAL
    // * RECTAAL
    // RETROBULBAIR
    // SUBCONJUNCTIVAAL
    // * SUBCUTAAN
    // SUBLINGUAAL
    // * TRANSDERMAAL
    // TRANSDERML
    // URETHRAAL
    // UTERIEN
    // VAGINAAL

    type Route =
        | Alternative of string
        | AUR // AURICULAIR OOR
        | CUT // CUTAAN TRANSDERMAAL TRANSDERML LOKAAL
        | ENDOTR // ENDOTR.PULM ENDOTRACHEOPULMONAIR
        | EPIDUR // EPIDURAAL
        | IM // INTRAMUSCULAIR IM
        | INH // INHALATIE
        | INTRAVESIC // INTRAVESIC. INTRAVESICAAL
        | IV // INTRAVENEUS IV
        | LESION // ARTI/LESION
        | NASAL // NASAAL NEUS
        | ORAL // ORAAL GASTR-ENTER OROMUCOSAAL
        | OROMUCOSAL //OROMUCOSAAL
        | RECTAL // RECTAAL
        | SUBCUT // INTRADERMAAL SUBCUTAAN
        | NoRoute


    let routeMapping =
        [
            (AUR, ["AURICULAIR"; "OOR"])
            (CUT, [ "CUTAAN"; "TRANSDERMAAL"; "TRANSDERML"; "LOKAAL"])
            (ENDOTR, ["ENDOTR.PULM"; "ENDOTRACHEOPULMONAIR"])
            (EPIDUR, ["EPIDURAAL"])
            (IM, ["INTRAMUSCULAIR"; "IM"])
            (INH, ["INHALATIE"; "INH"])
            (INTRAVESIC, ["INTRAVESIC."; "INTRAVESICAAL"])
            (IV, ["INTRAVENEUS"; "IV"])
            (NASAL, ["NASAAL"; "NEUS"])
            (ORAL, ["ORAAL"; "GASTR-ENTER"; "OR"])
            (OROMUCOSAL, ["OROMUCOSAAL"])
            (RECTAL, ["RECTAAL"; "RECT"])
            (SUBCUT, ["INTRADERMAAL"; "SUBCUTAAN"; "SC"])
        ]


    let createRoute s =
        let m =
            routeMapping
            |> List.tryFind (fun (_, rs) ->
                rs
                |> List.exists (String.equalsCapInsens s)
            )
        match m with
        | Some (r, _) -> r
        | _ -> NoRoute


    let eqsRoute r s = s |> createRoute = r


    let toString r =
        match routeMapping |> List.tryFind (fun (r', _) -> r' = r) with
        | Some (_, rl) ->
            match rl |> List.tryHead with
            | Some h -> h
            | None   -> ""
        | None -> ""


    let fromString s =
        routeMapping
        |> List.tryFind (fun (_, rs) -> rs |> List.exists ((=) s))
        |> Option.bind (fun (r, _) -> Some r)