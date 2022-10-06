
#time

#load "load.fsx"


#load "../Types.fs"
#load "../Utils.fs"
#load "../MinMax.fs"
#load "../Patient.fs"
#load "../Product.fs"


open System
open System.IO


open MathNet.Numerics

open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.GenForm.Lib

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


type Location =
    | PVL
    | CVL
    | AnyLocation


type SolutionLimit =
    {
        Substance : string
        Unit : string
        Quantity : MinMax
        Quantities : BigRational []
        Concentration : MinMax
    }


type SolutionRule =
    {
        Generic : string
        Shape : string
        Department : string
        Location : Location
        Age : MinMax
        Weight : MinMax
        Solutions : string []
        Volumes : BigRational []
        Volume : MinMax
        DoseCount : MinMax
        Time : MinMax
        Products : Product []
        SolutionLimits : SolutionLimit []
    }



let toBrs = BigRational.toBrs


let toBrOpt = BigRational.toBrOpt


let tupleBrOpt = BigRational.tupleBrOpt



let get getter (rules : SolutionRule[]) =
    rules
    |> Array.map getter
    |> Array.distinct
    |> Array.sort



let generics = get (fun sr -> sr.Generic)


let getSolutionRules () =
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
                Solutions = get "Solutions" |> String.split "|"
                Volumes = get "Volumes" |> toBrs
                MinVol = get "MinVol" |> toBrOpt
                MaxVol = get "MaxVol" |> toBrOpt
                MinCount = get "MinCount" |> toBrOpt
                MaxCount = get "MaxCount" |> toBrOpt
                MinTime = get "MinTime" |> toBrOpt
                MaxTime = get "MaxTime" |> toBrOpt
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
                Solutions = r.Solutions |> List.toArray
                Volumes = r.Volumes
                Volume = (r.MinVol, r.MaxVol) |> MinMax.fromTuple
                DoseCount = (r.MinCount, r.MaxCount) |> MinMax.fromTuple
                Time = (r.MinTime, r.MaxTime) |> MinMax.fromTuple
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
        match sr.Location with
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
                    if s |> String.isNullOrWhiteSpace then
                        $""" in {sr.Solutions |> String.concat "/"}"""
                    else
                        $""" in {s} ml {sr.Solutions |> String.concat "/"}"""

    let conc =
        if limit.Concentration |> MinMax.toString |> String.isNullOrWhiteSpace then ""
        else
            $"* concentratie: {limit.Concentration |> MinMax.toString} {limit.Unit}/ml"

    let time =
        if sr.Time |> MinMax.toString |> String.isNullOrWhiteSpace then ""
        else
            $"* inlooptijd: {sr.Time |> MinMax.toString} min"

    $"""
{loc}{limit.Substance}: {q}{qs}{vol}
{conc}
{time}
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


    ({| md = ""; rules = [||] |}, rules |> Array.groupBy (fun d -> d.Generic))
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
                (r, r.rules |> Array.groupBy (fun d -> d.Department))
                ||> Array.fold (fun acc (dep, rs) ->
                    {| acc with
                        md = acc.md + (department_md dep)
                        rules = rs
                    |}
                    |> fun r ->
                        if r.rules |> Array.isEmpty then r
                        else
                            (r, r.rules |> Array.groupBy (fun r -> r.Age, r.Weight))
                            ||> Array.fold (fun acc ((a, w), rs) ->
                                let sol =
                                    rs
                                    |> Array.groupBy (fun r -> r.Location)
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
                                    let a = a |> Patient.printAgeMinMax

                                    let w =
                                        let s = w |> MinMax.toString

                                        if s |> String.isNullOrWhiteSpace then
                                            ""
                                        else
                                            $"gewicht %s{s} kg"

                                    if a |> String.isNullOrWhiteSpace
                                       && w |> String.isNullOrWhiteSpace then
                                        ""
                                    else
                                        $"patient: %s{a} %s{w}" |> String.trim

                                {| acc with
                                    rules = rs
                                    md =
                                        if pat |> String.isNullOrWhiteSpace then acc.md
                                        else
                                            acc.md + pat_md pat
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
        |> Array.filter (fun sr -> sr.Generic = g)
        |> toMarkdown
    )


getSolutionRules ()
|> printGenerics
|> fun s -> File.WriteAllText("solutions.md", s |> String.concat "\n")


getSolutionRules ()
|> Array.take 2
|> toMarkdown

