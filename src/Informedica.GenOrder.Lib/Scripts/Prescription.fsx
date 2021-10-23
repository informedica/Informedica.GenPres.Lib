
#I __SOURCE_DIRECTORY__

#load "load.fsx"

    
open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

let dto = Prescription.Dto.dto "1"

dto
|> Prescription.Dto.fromDto


dto
|> Prescription.Dto.setToContinuous
|> Prescription.Dto.fromDto

dto
|> Prescription.Dto.setToDiscontinuous
|> Prescription.Dto.fromDto

dto
|> Prescription.Dto.setToTimed
|> Prescription.Dto.fromDto
