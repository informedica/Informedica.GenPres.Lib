
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../WrappedString.fs"
#load "../Solver.fs"
#load "../ValueUnit.fs"
#load "../VariableUnit.fs"
#load "../Orderable.fs"
#load "../Prescription.fs"
#load "../Order.fs"

#time
   
open MathNet.Numerics

open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

module Units = ValueUnit.Units

// Creating a paracetamol orderable from dtos
module Paracetamol =

        module Item = Orderable.Item
        module IDto = Item.Dto

        let idto = IDto.dto "1" "paracetamol"

        idto.ComponentConcentration.Unit <- "mg[Mass]/piece[General]"
        idto.ComponentQuantity.Unit <- "mg[Mass]"
        idto.DoseTotalAdjust.Unit <- "mg[Mass]/kg[Weight]/day[Time]"

        idto
        |> IDto.fromDto
        |> Item.toString
        |> List.iter (printfn "%s")

        module Component = Orderable.Component
        module CDto = Component.Dto

        let cdto = CDto.dto "1" "paracetamol"

        cdto.Items <- [ idto ]

        cdto.OrderableQuantity.Unit <- "piece[General]"
        cdto.OrderableConcentration.Unit <- "piece[General]/piece[General]"
        cdto.OrderQuantity.Unit <- "piece[General]"

        cdto
        |> CDto.fromDto
        |> Component.toString
        |> List.iter (printfn "%s")

        module ODto = Orderable.Dto

        let odto = ODto.dto "1" "paracetamol"

        odto.OrderableQuantity.Unit <- "piece[General]"
        odto.OrderQuantity.Unit <- "piece[General]"

        odto.Components <- [ cdto ]

        let dto = Order.Dto.discontinuous "1" "paracetamol" "rect"

        dto.Orderable <- odto

        dto.Prescription.Frequency.Unit <- "x[Count]/day[Time]"
        dto.Adjust.Unit <- "kg[Weight]"

        let print () =
            dto
            |> Order.Dto.fromDto
            |> Order.toString
            |> List.iteri (fun i s ->  printfn "%i\t%s" i s)


// sulfamethoxazol + trimethoprim orderable
module Cotrimoxazol =

    module Item = Orderable.Item
    module IDto = Item.Dto

    let sulfa_idto = IDto.dto "1" "sulfamethoxazol"


    sulfa_idto
    |> IDto.fromDto
    |> Item.toString
    |> List.iter (printfn "%s")

    let trim_idto = IDto.dto "1" "trimethoprim"


    trim_idto
    |> IDto.fromDto
    |> Item.toString
    |> List.iter (printfn "%s")


    module Component = Orderable.Component
    module CDto = Component.Dto

    let cdto = CDto.dto "1" "cotrimoxazol"

    cdto.Items <- [ sulfa_idto; trim_idto ]

    cdto
    |> CDto.fromDto
    |> Component.toString
    |> List.iter (printfn "%s")

    module ODto = Orderable.Dto

    let odto = ODto.dto "1" "cotrimoxazol"

    odto.Components <- [ cdto ]

    let print () =
        odto
        |> ODto.fromDto
        |> Orderable.toString
        |> List.iteri (fun i s ->  printfn "%i\t%s" i s)


// dopamin infusion orderable
module Dopamin =

    module Item = Orderable.Item
    module IDto = Item.Dto

    let dopa_idto = IDto.dto "1" "dopamin"

    dopa_idto
    |> IDto.fromDto
    |> Item.toString
    |> List.iter (printfn "%s")

    let sod_idto = IDto.dto "1" "sodium"


    sod_idto
    |> IDto.fromDto
    |> Item.toString
    |> List.iter (printfn "%s")

    let chlor_idto = IDto.dto "1" "chloride"

    chlor_idto
    |> IDto.fromDto
    |> Item.toString
    |> List.iter (printfn "%s")


    module Component = Orderable.Component
    module CDto = Component.Dto

    let dopa_cdto = CDto.dto "1" "dopamin"

    dopa_cdto.Items <- [ dopa_idto ]

    dopa_cdto
    |> CDto.fromDto
    |> Component.toString
    |> List.iter (printfn "%s")

    let saline_cdto = CDto.dto "1" "saline"

    saline_cdto.Items <- [ sod_idto; chlor_idto ]

    saline_cdto
    |> CDto.fromDto
    |> Component.toString
    |> List.iter (printfn "%s")

    module ODto = Orderable.Dto

    let odto = ODto.dto "1" "dopamine infusion"

    odto.Components <- [ dopa_cdto; saline_cdto ]

    let print () =
        odto
        |> ODto.fromDto
        |> Orderable.toString
        |> List.iteri (fun i s ->  printfn "%i\t%s" i s)


Paracetamol.print ()

module Mapping = Order.Mapping

// General information for paracetamol
Paracetamol.dto
|> Order.Dto.fromDto
// the largest weight for a human being
|> Order.solve "paracetamol" Mapping.AdjustQty Solver.MaxIncl [635N]
// the smallest weight for a human being
|> Order.solve "paracetamol" Mapping.AdjustQty Solver.MinIncl [245N / 1000N]
// a supp cannot be divided
|> Order.solve "paracetamol" Mapping.ComponentOrderableCount Solver.Vals  [1N]
|> Order.solve "paracetamol" Mapping.ComponentComponentQty Solver.Vals  [1N]
|> Order.solve "paracetamol" Mapping.ComponentDoseQty Solver.Incr [1N]
// you want to give only one supp a time
|> Order.solve "paracetamol" Mapping.ComponentDoseQty Solver.Vals [1N]
// available products
|> Order.solve "paracetamol" Mapping.ItemComponentConc Solver.Vals  [60N; 120N; 240N; 500N; 1000N]
// general dose rules
|> Order.solve "paracetamol" Mapping.ItemDoseAdjustTotalAdjust Solver.MaxIncl  [90N]
|> Order.solve "paracetamol" Mapping.ItemDoseTotal Solver.MaxIncl  [4000N]
|> Order.solve "paracetamol" Mapping.ItemDoseQty Solver.MaxIncl  [1000N]
|> Order.solve "paracetamol" Mapping.Freq Solver.Vals [2N .. 4N]
// the patient is 10 kg
//|> Order.solve "paracetamol" Mapping.AdjustQty Solver.Vals [10N]
|> Order.toString 
|> List.iteri (fun i s -> printfn "%i\t%s" i s)


// Find out the weight range for a 240 mg paracetamol supp
// The lower limit is 5.3 kg
Paracetamol.dto
|> Order.Dto.fromDto
// the largest weight for a human being
|> Order.solve "paracetamol" Mapping.AdjustQty Solver.MaxIncl [635N]
// the smallest weight for a human being
|> Order.solve "paracetamol" Mapping.AdjustQty Solver.MinIncl [245N / 1000N]
// a supp cannot be divided
|> Order.solve "paracetamol" Mapping.ComponentOrderableCount Solver.Vals  [1N]
|> Order.solve "paracetamol" Mapping.ComponentComponentQty Solver.Vals  [1N]
|> Order.solve "paracetamol" Mapping.ComponentDoseQty Solver.Incr [1N]
// you want to give only one supp a time
|> Order.solve "paracetamol" Mapping.ComponentDoseQty Solver.Vals [1N]
// available products
|> Order.solve "paracetamol" Mapping.ItemComponentConc Solver.Vals  [240N]
// general dose rules
|> Order.solve "paracetamol" Mapping.ItemDoseAdjustTotalAdjust Solver.MaxIncl  [90N]
|> Order.solve "paracetamol" Mapping.ItemDoseTotal Solver.MaxIncl  [4000N]
|> Order.solve "paracetamol" Mapping.ItemDoseQty Solver.MaxIncl  [1000N]
|> Order.solve "paracetamol" Mapping.Freq Solver.Vals [2N .. 4N]
|> Order.toString 
|> List.iteri (fun i s -> printfn "%i\t%s" i s)


// Find out the weight range for a 240 mg paracetamol supp
// with a minimal effective dose of 20 mg/kg/day
// The lower limit is 5.3 kg and the upper limit is 48 kg
Paracetamol.dto
|> Order.Dto.fromDto
// the largest weight for a human being
|> Order.solve "paracetamol" Mapping.AdjustQty Solver.MaxIncl [635N]
// the smallest weight for a human being
|> Order.solve "paracetamol" Mapping.AdjustQty Solver.MinIncl [245N / 1000N]
// a supp cannot be divided
|> Order.solve "paracetamol" Mapping.ComponentOrderableCount Solver.Vals  [1N]
|> Order.solve "paracetamol" Mapping.ComponentComponentQty Solver.Vals  [1N]
|> Order.solve "paracetamol" Mapping.ComponentDoseQty Solver.Incr [1N]
// you want to give only one supp a time
|> Order.solve "paracetamol" Mapping.ComponentDoseQty Solver.Vals [1N]
// available products
|> Order.solve "paracetamol" Mapping.ItemComponentConc Solver.Vals  [240N]
// general dose rules
|> Order.solve "paracetamol" Mapping.ItemDoseAdjustTotalAdjust Solver.MaxIncl  [90N]
|> Order.solve "paracetamol" Mapping.ItemDoseAdjustTotalAdjust Solver.MinIncl  [20N]
|> Order.solve "paracetamol" Mapping.ItemDoseTotal Solver.MaxIncl  [4000N]
|> Order.solve "paracetamol" Mapping.ItemDoseQty Solver.MaxIncl  [1000N]
|> Order.solve "paracetamol" Mapping.Freq Solver.Vals [2N .. 4N]
|> Order.toString 
|> List.iteri (fun i s -> printfn "%i\t%s" i s)



// Calculate all possible product quantities (generic products) for a 10 kg patient
// from this all possible prescripiption scenarios can be created
Paracetamol.dto
|> Order.Dto.fromDto
// the largest weight for a human being
|> Order.solve "paracetamol" Mapping.AdjustQty Solver.MaxIncl [635N]
// the smallest weight for a human being
|> Order.solve "paracetamol" Mapping.AdjustQty Solver.MinIncl [245N / 1000N]
// a supp cannot be divided
|> Order.solve "paracetamol" Mapping.ComponentOrderableCount Solver.Vals  [1N]
|> Order.solve "paracetamol" Mapping.ComponentComponentQty Solver.Vals  [1N]
|> Order.solve "paracetamol" Mapping.ComponentDoseQty Solver.Incr [1N]
// you want to give only one supp a time
|> Order.solve "paracetamol" Mapping.ComponentDoseQty Solver.Vals [1N]
// available products
|> Order.solve "paracetamol" Mapping.ItemComponentConc Solver.Vals  [60N; 120N; 240N; 500N; 1000N]
// general dose rules
|> Order.solve "paracetamol" Mapping.ItemDoseAdjustTotalAdjust Solver.MaxIncl  [90N]
|> Order.solve "paracetamol" Mapping.ItemDoseAdjustTotalAdjust Solver.MinIncl  [20N]
|> Order.solve "paracetamol" Mapping.ItemDoseTotal Solver.MaxIncl  [4000N]
|> Order.solve "paracetamol" Mapping.ItemDoseQty Solver.MaxIncl  [1000N]
|> Order.solve "paracetamol" Mapping.Freq Solver.Vals [2N .. 4N]
// the patient is 10 kg
|> Order.solve "paracetamol" Mapping.AdjustQty Solver.Vals [10N]
|> Order.toString 
|> List.iteri (fun i s -> printfn "%i\t%s" i s)
