
#load "load-utils.fsx"
#load "load.fsx"

#time

open Informedica.Utils.Lib.BCL
open Informedica.GenSolver.Utils
open MathNet.Numerics

module API = Informedica.GenSolver.Lib.Api

let procss s = "> " + s + " </br> "|> String.replace "*" "\*" |> printfn "%s"

// ToDo Need to rewrite

// let printEqs = API.printEqs procss
// let solve    = API.solve procss
// let init     = API.init
// let nonZeroNegative = API.nonZeroNegative

// let eqs = " = "
// let tms = " * "
// let add = " + "

// let time   = "time"         // Time
// let freq   = "freq"         // Frequency
// let total  = "prescr.total" // Total prescribed
// let qty    = "prescr.qty"   // Quantity prescribed
// let rate   = "prescr.rate"  // Rate prescribed

// let drug_total = "drug.total" // Total of drug
// let drug_qty   = "drug.qty"   // Quantity of drug

// let wght = "weight" // Weight adjustment
// let bsa  = "bsa"    // Body surface area adjustment

// let compSub c comp_total comp_drug_qty s = 
//     let sub_comp_qty  = s + "." + c + ".comp.qty"  // Quantity of substance in component
//     let sub_comp_conc = s + "." + c + ".comp.conc" // Concentration of substance in component

//     let sub_drug_qty  = s + ".drug.qty"  // Quantity of substance in drug
//     let sub_drug_conc = s + ".drug.conc" // Concentration of substance in drug

//     let sub_dose_qty   = s + ".dose.qty"   // Quantity dose of substance
//     let sub_dose_total = s + ".dose.total" // Total dose of substance
//     let sub_dose_rate  = s + ".dose.rate"  // Rate dose of substance

//     let sub_dose_qty_wght   = s + ".dose.qty.wght"   // Weight adjusted quantity dose of substance
//     let sub_dose_total_wght = s + ".dose.total.wght" // Weight adjusted total dose of substance
//     let sub_dose_rate_wght  = s + ".dose.rate.wght"  // Weight adjusted rate dose of substance

//     let sub_dose_qty_bsa   = s + ".dose.qty.bsa"   // Body surface area adjusted quantity dose of substance
//     let sub_dose_total_bsa = s + ".dose.total.bsa" // Body surface area adjusted total dose of substance
//     let sub_dose_rate_bsa  = s + ".dose.rate.bsa"  // Body surface area adjusted rate dose of substance


//     [
//         sub_comp_qty   + eqs + sub_comp_conc + tms + comp_total
//         sub_drug_qty   + eqs + sub_drug_conc + tms + drug_total
//         sub_drug_qty   + eqs + sub_comp_conc + tms + comp_drug_qty
//         sub_dose_total + eqs + sub_dose_qty  + tms + freq 
//         sub_dose_qty   + eqs + sub_dose_rate + tms + time
//         sub_dose_qty   + eqs + sub_drug_conc + tms + qty
//         sub_dose_total + eqs + sub_drug_conc + tms + total
//         sub_dose_rate  + eqs + sub_drug_conc + tms + rate

//         sub_dose_qty   + eqs + sub_dose_qty_wght   + tms + wght
//         sub_dose_total + eqs + sub_dose_total_wght + tms + wght
//         sub_dose_rate  + eqs + sub_dose_rate_wght  + tms + wght

//         sub_dose_qty   + eqs + sub_dose_qty_bsa   + tms + bsa
//         sub_dose_total + eqs + sub_dose_total_bsa + tms + bsa
//         sub_dose_rate  + eqs + sub_dose_rate_bsa  + tms + bsa
//     ]

// let comp cs =
//     let c, sl = cs
//     let comp_qty   = c + ".comp.qty"   // Quantity of component
//     let comp_total = c + ".comp.total" // Total of component

//     let comp_drug_qty  = c + ".drug.qty"  // Quantity of component in drug
//     let comp_drug_conc = c + ".drug.conc" // Concentration of component in drug

//     let comp_dose_qty   = c + ".dose.qty"   // Quantity dose of component
//     let comp_dose_total = c + ".dose.total" // Total dose of component
//     let comp_dose_rate  = c + ".dose.rate"  // Rate dose of component

//     let comp_dose_qty_wght   = c + ".dose.qty.wght"   // Weight adjusted quantity dose of component
//     let comp_dose_total_wght = c + ".dose.total.wght" // Weight adjusted total dose of component
//     let comp_dose_rate_wght  = c + ".dose.rate.wght"  // Weight adjusted rate dose of component

//     let comp_dose_qty_bsa   = c + ".dose.qty.bsa"   // Body surface area adjusted quantity dose of component
//     let comp_dose_total_bsa = c + ".dose.total.bsa" // Body surface area adjusted total dose of component
//     let comp_dose_rate_bsa  = c + ".dose.rate.bsa"  // Body surface area adjusted rate dose of component

//     [
//         drug_total      + eqs + comp_qty       + tms + comp_total
//         comp_drug_qty   + eqs + comp_drug_conc + tms + drug_total
//         comp_dose_total + eqs + comp_dose_qty  + tms + freq 
//         comp_dose_qty   + eqs + comp_dose_rate + tms + time
//         comp_dose_qty   + eqs + comp_drug_conc + tms + qty
//         comp_dose_total + eqs + comp_drug_conc + tms + total
//         comp_dose_rate  + eqs + comp_drug_conc + tms + rate

//         comp_dose_qty   + eqs + comp_dose_qty_wght   + tms + wght
//         comp_dose_total + eqs + comp_dose_total_wght + tms + wght
//         comp_dose_rate  + eqs + comp_dose_rate_wght  + tms + wght

//         comp_dose_qty   + eqs + comp_dose_qty_bsa   + tms + bsa
//         comp_dose_total + eqs + comp_dose_total_bsa + tms + bsa
//         comp_dose_rate  + eqs + comp_dose_rate_bsa  + tms + bsa

//     ] |> List.append (sl |> List.collect (compSub c comp_total comp_drug_qty))

// let drug cs =   
//     [ 
//         total + eqs + qty        + tms + freq
//         qty   + eqs + rate       + tms + time
//         total + eqs + drug_total + tms + drug_qty
//     ] 
//     |> List.append (cs |> List.collect comp)
//     |> List.append [ 
//         drug_total + eqs + (cs |> List.fold (fun acc (c, _) -> 
//         if acc = "" then c + ".drug.qty" else acc + add + c + ".drug.qty") "")
//         ]

//     |> init              // Initialize the calculation model
//     |> nonZeroNegative   // Set all variables to only contain non zero positive values


// let items = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m"]
// let test () = 
//     let drg =
//         [
//             ("supp", items)
//         ] |> drug

//     items
//     |> List.fold (fun t i ->
//         t
//         |> solve (i + ".drug.qty") "vals" [1N]
//         |> solve (i + ".drug.conc") "vals" [1N]
//         |> solve (i + ".supp.comp.qty") "vals" [1N]) drg
//     |> ignore
