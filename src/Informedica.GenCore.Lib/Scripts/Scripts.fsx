
#load "load.fsx"

#load "../Measures.fs"
#load "../Aether.fs"
#load "../Validus.fs"
#load "../Calculations.fs"
#load "../MinIncrMax.fs"
#load "../Patient.fs"



open Informedica.GenCore.Lib

open System


let fromAgeType = Patient.fromAgeType UnknownGender DateTime.Now



let ageDto = AgeValue.Dto.dto ()

ageDto.Years <- Some 157
ageDto.Days <- Some 8

ageDto
|>(AgeValue.Dto.fromDto)


let weightDto = WeightValue.Dto.dto ()

weightDto.Weight <- 300m
weightDto.WeightInKg <- false

weightDto
|> WeightValue.Dto.fromDto


WeightValue.Validation.maxWeightGram
|> decimal


VenousAccess.Validation.validate "VenousAcces" "central"


Patient.unknown
|> Patient.Dto.toDto
|> Patient.Dto.fromDto
|> function
    | Ok pat -> pat = Patient.unknown
    | Error errs ->
        printfn $"{errs}"
        false



let patDto =
    Patient.unknown
    |> Patient.Dto.toDto


patDto.Weight.Calculation <-
    let w = WeightAtDate.Dto.dto ()
    w.DateTime <- DateTime.Now
    w.WeightValue.Weight <- 4.7m
    Some w


patDto.Height.Calculation <-
    let h = HeightAtDate.Dto.dto ()
    h.DateTime <- DateTime.Now
    h.HeightValue.Height <- 57m
    h.HeightValue.HeightInMeter <- false
    Some h


patDto
|> Patient.Dto.fromDto
|> function
    | Ok p -> p |> Patient.BSA.calcMosteller
    | Error _ -> None


Department.pediatricDepartment "WKZ"
|> Department.toString


open Informedica.Utils.Lib.BCL

"UMCU"
|> Department.adultICU
|> Department.toString
|> Department.fromString


Calculations.Age.ageToStringNL None None None None

Calculations.Age.ageToStringNL (Some 2<year>) (Some 1<month>) (Some 2<week>) None


PatientAge.newBorn
PatientAge.infant
PatientAge.toddler
PatientAge.child
PatientAge.adolescent
PatientAge.adult


NewBorn |> fromAgeType 
|> Patient.BSA.calcMosteller


Adult |> fromAgeType 
|> Patient.Dto.toDto
|> Patient.Dto.fromDto


FullTerm
|> AgeWeeksDays.fromGestation
|> Option.map (AgeWeeksDays.SetGet.setDays 1<day>)
|> Option.map (AgeWeeksDays.toString)
|> Option.defaultValue ""


FullTerm
|> AgeWeeksDays.fromGestation
|> Option.map (AgeWeeksDays.SetGet.getWeeks >> GestationType.fromWeeks)

FullTerm
|> AgeWeeksDays.fromGestation
|> Option.map  AgeWeeksDays.SetGet.getWeeks

37<week> |> GestationType.fromWeeks
GestationType.max
GestationType.min



NewBorn
|> fromAgeType
//|> Patient.SetGet
//|> Patient.SetGet.getAgeValue DateTime.Now

|> Patient.calcPostMenstrualAge DateTime.Now
//|> Patient.toString DateTime.Now


NewBorn
|> fromAgeType
|> Patient.SetGet.getAge
|> PatientAge.toString


AgeValue.create (Some 0<year>) None None None
|> AgeValue.toString

Calculations.Age.ageToStringNL (Some 0<year>) None None None


let newBornDto =
    NewBorn
    |> fromAgeType
    |> Patient.Dto.toDto


newBornDto.Age.Value.Years <- 0 |> Some
newBornDto.Age.Value.Months <- 1 |> Some
newBornDto.Age.Value.Weeks <- 1 |> Some

newBornDto.GestationalAge <- 
    let dto = AgeWeeksDays.Dto.dto ()
    dto.Weeks <- 40
    dto |> Some


newBornDto
|> Patient.Dto.fromDto
|> function
| Ok p ->
    p 
    |> Patient.SetGet.getAgeValue DateTime.Now
    |> Option.map AgeValue.getAgeInDays
    |> printfn "%A" 
    p 
    |> Patient.toString DateTime.Now
| _  -> []


Calculations.Age.postMenstrualAge 97<day> 40<week> 0<day>

AgeType.normalValues

