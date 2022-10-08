namespace Informedica.GenForm.Lib

module SolutionRule =

    open MathNet.Numerics
    open Informedica.Utils.Lib.BCL


    let toBrs = BigRational.toBrs


    let toBrOpt = BigRational.toBrOpt


    let tupleBrOpt = BigRational.tupleBrOpt



    let get getter (rules : SolutionRule[]) =
        rules
        |> Array.map getter
        |> Array.distinct
        |> Array.sort



    let generics = get (fun sr -> sr.Selector.Generic)


    let getRules () =
        Web.getDataFromSheet "SolutionRules2"
        |> fun data ->
            let prods = Product.products ()

            let getColumn =
                data
                |> Array.head
                |> Csv.getStringColumn

            data
            |> Array.tail
            |> Array.map (fun r ->
                let get = getColumn r
                let toBrOpt = toBrs >> toBrOpt

                {|
                    Generic = get "Generic"
                    Shape = get "Shape"
                    Department = get "Dep"
                    CVL = get "CVL"
                    PVL = get "PVL"
                    MinAge = get "MinAge" |> toBrOpt
                    MaxAge = get "MaxAge" |> toBrOpt
                    MinWeight = get "MinWeight" |> toBrOpt
                    MaxWeight = get "MaxWeight" |> toBrOpt
                    MinDose = get "MinDose" |> toBrOpt
                    MaxDose = get "MaxDose" |> toBrOpt
                    DoseType = get "DoseType"
                    Solutions = get "Solutions" |> String.split "|"
                    Volumes = get "Volumes" |> toBrs
                    MinVol = get "MinVol" |> toBrOpt
                    MaxVol = get "MaxVol" |> toBrOpt
                    MinPerc = get "MinPerc" |> toBrOpt
                    MaxPerc = get "MaxPerc" |> toBrOpt
                    MinTime = get "MinTime" |> toBrOpt
                    MaxTime = get "MaxTime" |> toBrOpt
                    MaxRate = get "MaxRate" |> toBrOpt
                    RateUnit = get "RateUnit"
                    Substance = get "Substance"
                    Unit = get "Unit"
                    Quantities = get "Quantities" |> toBrs
                    MinQty = get "MinQty" |> toBrOpt
                    MaxQty = get "MaxQty" |> toBrOpt
                    MinConc = get "MinConc" |> toBrOpt
                    MaxConc = get "MaxConc" |> toBrOpt
                |}
            )
            |> Array.groupBy (fun r ->
                {
                    Selector =
                        {
                            Generic = r.Generic
                            Shape = r.Shape
                            Department = r.Department
                            Location =
                                if r.CVL = "x" then CVL
                                else
                                    if r.PVL = "x" then PVL
                                    else
                                        AnyLocation
                            Age = (r.MinAge, r.MaxAge) |> MinMax.fromTuple
                            Weight = (r.MinWeight, r.MaxWeight) |> MinMax.fromTuple
                            Dose = (r.MinDose, r.MaxDose) |> MinMax.fromTuple
                            DoseType = r.DoseType |> DoseType.fromString
                        }
                    Solutions = r.Solutions |> List.toArray
                    Volumes = r.Volumes
                    Volume = (r.MinVol, r.MaxVol) |> MinMax.fromTuple
                    DosePerc = (r.MinPerc, r.MaxPerc) |> MinMax.fromTuple
                    Time = (r.MinTime, r.MaxTime) |> MinMax.fromTuple
                    MaxRate = r.MaxRate
                    RateUnit = r.RateUnit
                    Products = prods |> Product.filter r.Generic r.Shape
                    SolutionLimits = [||]
                }
            )
            |> Array.map (fun (solutionRule, rs) ->
                { solutionRule with
                    SolutionLimits =
                        rs
                        |> Array.map (fun l ->
                            {
                                Substance = l.Substance
                                Unit = l.Unit
                                Quantity = (l.MinQty, l.MaxQty) |> MinMax.fromTuple
                                Quantities = l.Quantities
                                Concentration = (l.MinConc, l.MaxConc) |> MinMax.fromTuple
                            }
                        )
                }
            )


    let printSolutionLimit (sr: SolutionRule) (limit: SolutionLimit) =
        let loc =
            match sr.Selector.Location with
            | CVL -> "###### centraal: \n* "
            | PVL -> "###### perifeer: \n* "
            | AnyLocation -> "* "

        let qs =
            if limit.Quantities |> Array.isEmpty then
                ""
            else
                limit.Quantities
                |> Array.map BigRational.toStringNl
                |> String.concat ", "
                |> fun s -> $" {s} {limit.Unit}"

        let q =
            if limit.Quantity
               |> MinMax.toString
               |> String.isNullOrWhiteSpace then
                ""
            else
                limit.Quantity
                |> MinMax.toString
                |> fun s ->
                    if qs |> String.isNullOrWhiteSpace then
                        $" {s} {limit.Unit}"
                    else
                        $" ({s} {limit.Unit})"

        let vol =
            if sr.Volume
               |> MinMax.toString
               |> String.isNullOrWhiteSpace then
                ""
            else
                sr.Volume
                |> MinMax.toString
                |> fun s -> $""" in {s} ml {sr.Solutions |> String.concat "/"}"""
            |> fun s ->
                if s |> String.isNullOrWhiteSpace |> not then s
                else
                    sr.Volumes
                    |> Array.map BigRational.toStringNl
                    |> String.concat "\n"
                    |> fun s ->
                        let sols = sr.Solutions |> String.concat "/"
                        if s |> String.isNullOrWhiteSpace then
                            if sols |> String.isNullOrWhiteSpace then " puur"
                            else $" in {sols}"
                        else
                            $" in {s} ml {sols}"

        let conc =
            if limit.Concentration |> MinMax.toString |> String.isNullOrWhiteSpace then ""
            else
                $"* concentratie: {limit.Concentration |> MinMax.toString} {limit.Unit}/ml"

        let dosePerc =
            let p =
                sr.DosePerc
                |> MinMax.map (fun br -> br * 100N) (fun br -> br * 100N)
                |> MinMax.toString

            if p |> String.isNullOrWhiteSpace then ""
            else
                $"* geef {p}%% van de bereiding"

        let time =
            if sr.Time |> MinMax.toString |> String.isNullOrWhiteSpace then ""
            else
                $"* inlooptijd: {sr.Time |> MinMax.toString} min"

        let rate =
            match sr.MaxRate with
            | Some r when sr.RateUnit |> String.isNullOrWhiteSpace |> not &&
                          limit.Unit |> String.isNullOrWhiteSpace |> not ->
                $"* max inloop snelheid: {r |> BigRational.toStringNl} {limit.Unit}/{sr.RateUnit}"
            | _ -> ""

        $"""
{loc}{limit.Substance}: {q}{qs}{vol}
{conc}
{dosePerc}
{time}{rate}
"""


    let toMarkdown (rules: SolutionRule []) =
        let generic_md generic products =
            $"""
# %s{generic}
---

#### Producten

%s{products}

"""

        let department_md dep =
            let dep =
                match dep with
                | _ when dep = "AICU" -> "ICC"
                | _ -> dep

            $"""

### Afdeling: {dep}
"""

        let pat_md pat =
            $"""
##### %s{pat}
"""

        let product_md product =
            $"""
* %s{product}
"""


        ({| md = ""; rules = [||] |}, rules |> Array.groupBy (fun d -> d.Selector.Generic))
        ||> Array.fold (fun acc (generic, rs) ->
            let prods =
                rs
                |> Array.collect (fun d -> d.Products)
                |> Array.sortBy (fun p ->
                    p.Substances
                    |> Array.sumBy (fun s -> s.Quantity |> Option.defaultValue 0N)
                )
                |> Array.map (fun p ->
                    let sol =
                        p.ShapeVolume
                        |> Option.map BigRational.toStringNl
                        |> Option.defaultValue ""
                    if sol |> String.isNullOrWhiteSpace then product_md p.Label
                    else
                        $"{p.Label} oplossen in {sol} ml"
                        |> product_md
                )
                |> Array.distinct
                |> String.concat "\n"

            {| acc with
                md = generic_md generic prods
                rules = rs
            |}
            |> fun r ->
                if r.rules = Array.empty then r
                else
                    (r, r.rules |> Array.groupBy (fun d -> d.Selector.Department))
                    ||> Array.fold (fun acc (dep, rs) ->
                        {| acc with
                            md = acc.md + (department_md dep)
                            rules = rs
                        |}
                        |> fun r ->
                            if r.rules |> Array.isEmpty then r
                            else
                                (r,
                                 r.rules
                                 |> Array.groupBy (fun r ->
                                    {|
                                        Age = r.Selector.Age
                                        Weight = r.Selector.Weight
                                        Dose = r.Selector.Dose
                                        DoseType = r.Selector.DoseType
                                    |}
                                 )
                                )
                                ||> Array.fold (fun acc (sel, rs) ->
                                    let sol =
                                        rs
                                        |> Array.groupBy (fun r -> r.Selector.Location)
                                        |> Array.collect (fun (_, rs) ->
                                            rs
                                            |> Array.tryHead
                                            |> function
                                                | None -> [||]
                                                | Some r ->
                                                    r.SolutionLimits
                                                    |> Array.map (printSolutionLimit r)
                                        )
                                        |> String.concat "\n"

                                    let pat =
                                        let a = sel.Age |> Patient.printAgeMinMax

                                        let w =
                                            let s = sel.Weight |> MinMax.toString

                                            if s |> String.isNullOrWhiteSpace then
                                                ""
                                            else
                                                $"gewicht %s{s} kg"

                                        if a |> String.isNullOrWhiteSpace
                                           && w |> String.isNullOrWhiteSpace then
                                            ""
                                        else
                                            $"patient: %s{a} %s{w}" |> String.trim

                                    let dose =
                                        let d = sel.Dose |> MinMax.toString
                                        let u =
                                            match rs |> Array.collect (fun r -> r.SolutionLimits) with
                                            | [| sl |] -> sl.Unit
                                            | _ -> ""

                                        if d |> String.isNullOrWhiteSpace ||
                                           u |> String.isNullOrWhiteSpace then ""
                                        else
                                            $"{d} {u}" 

                                    let dt =
                                        let s = sel.DoseType |> DoseType.toString
                                        if s |> String.isNullOrWhiteSpace then ""
                                        else
                                            $"{s}"
                                        

                                    {| acc with
                                        rules = rs
                                        md =
                                            if pat |> String.isNullOrWhiteSpace &&
                                               dose |> String.isNullOrWhiteSpace then
                                                acc.md + $"##### {dt}"
                                            else
                                                acc.md + pat_md ($"{dt}, {pat}{dose}")
                                            |> fun s -> $"{s}\n{sol}"
                                    |}
                                )
                    )


        )
        |> fun md -> md.md


    let printGenerics (rules: SolutionRule []) =
        rules
        |> generics
        |> Array.sort
        |> Array.map (fun g ->
            rules
            |> Array.filter (fun sr -> sr.Selector.Generic = g)
            |> toMarkdown
        )

