namespace Informedica.GenForm.Lib


module PrescriptionRule =

    open MathNet.Numerics
    open Informedica.GenForm.Lib


    let get (pat : Patient) =
        let filter =
            Filter.filter
            |> Filter.setPatient pat

        DoseRule.get ()
        |> DoseRule.filter filter
        |> Array.collect (fun dr ->
            // ugly hack to get the right solution rule based on the dose quantity
            let dose =
                dr.DoseLimits[0]
                |> fun d ->
                    let dq = d.Quantity.Maximum
                    let dp =
                        match dr.Frequencies |> Array.tryHead with
                        | Some f -> d.PerTime.Maximum |> Option.map (fun v -> v / f)
                        | None   -> None
                    let dqa =
                        match pat.Weight with
                        | None   -> None
                        | Some w ->
                            let w = w / 1000N
                            d.QuantityAdjust.Maximum
                            |> Option.map ((*) w)
                    let dpa =
                        match dr.Frequencies |> Array.tryHead with
                        | Some f ->
                            match pat.Weight with
                            | None   -> None
                            | Some w ->
                                let w = w / 1000N
                                d.PerTimeAdjust.Maximum |> Option.map (fun v -> w * v / f)
                        | None   -> None

                    match dq, dp, dqa, dpa with
                    | Some q, _, _, _
                    | _, Some q, _, _
                    | _, _, Some q, _
                    | _, _, _, Some q -> q |> Some
                    | _ -> None

            SolutionRule.get ()
            |> SolutionRule.filter
                { filter with
                    Generic = dr.Generic |> Some
                    Shape = dr.Shape |> Some
                    Route = dr.Route |> Some
                    DoseType = dr.DoseType
                    Dose = dose
                }
            //|> fun xs ->
            //    if xs |> Array.length > 1 then
            //        printfn $"multiple solution rules for {dr.Generic}, {dr.Shape}"
            //    xs
            |> function
            | srs when srs |> Array.isEmpty ->
                [| { Patient = pat; DoseRule = dr; SolutionRule = None }  |]
            | srs ->
                srs
                |> Array.map (fun sr ->
                    {
                        Patient = pat
                        DoseRule = dr
                        SolutionRule = sr |> Some
                    }
                )
        )

    let filterProducts shapeQuantities (substs : (string * BigRational option) array)  (pr : PrescriptionRule) =
        { pr with
            DoseRule =
                { pr.DoseRule with
                    Products =
                        pr.DoseRule.Products
                        |> Array.filter (fun p ->
                            p.ShapeQuantities
                            |> Array.exists (fun sq ->
                                shapeQuantities
                                |> Array.exists ((=) sq)
                            ) &&
                            p.Substances
                            |> Array.map (fun s -> s.Name.ToLower(), s.Quantity)
                            |> Array.exists (fun sq ->
                                substs
                                |> Array.exists((=) sq)
                            )
                        )
                }
        }


    let toMarkdown (prs : PrescriptionRule []) =
            [
                yield!
                    prs
                    |> Array.collect (fun x ->
                        [|
                            [| x.DoseRule |] |> DoseRule.Print.toMarkdown
                            if x.SolutionRule.IsSome then
                                [| x.SolutionRule.Value |] |> SolutionRule.Print.toMarkdown "verdunnen"
                          |]
                  )
            ]
            |> List.append [ prs[0].Patient |> Patient.toString ]
            |> String.concat "\n"


    let getDoseRule (pr : PrescriptionRule) = pr.DoseRule


    let getDoseRules = Array.map getDoseRule


    let indications = getDoseRules >> DoseRule.indications


    let generics = getDoseRules >> DoseRule.generics


    let shapes = getDoseRules >> DoseRule.shapes


    let routes = getDoseRules >> DoseRule.routes


    let departments = getDoseRules >> DoseRule.departments


    let diagnoses= getDoseRules >> DoseRule.diagnoses


    let genders = getDoseRules >> DoseRule.genders


    let patients = getDoseRules >> DoseRule.patients


    let frequencies = getDoseRules >> DoseRule.frequencies



