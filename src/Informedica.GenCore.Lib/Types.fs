namespace Informedica.GenCore.Lib


module Types =


    module Patient =

        open System
        open Informedica.GenCore.Lib.Measures

        type AgeType =
            | NewBorn
            | Infant
            | Toddler
            | Child
            | Adolescent
            | Adult
            | UnknownAgeType


        type GestationType =
            | ExtremePreterm
            | VeryPreterm
            | ModeratePreterm
            | FullTerm
            | UnknownGestation


        type VenousAccess =
            | Peripheral
            | Central
            | AnyVenous
            | UnknownVenous


        type EnteralAccess =
            | Oral
            | Gastric
            | Duodenal
            | AnyEnteral
            | UnknownEnteral


        type Department =
            | AdultICU of string
            | PediatricICU of string
            | NeonatalICU of string
            | AdultDepartment of string
            | PediatricDepartment of string
            | AnyDepartment
            | UnknownDepartment


        type Gender = Male | Female | AnyGender | UnknownGender


        type AgeValue =
            {
                Years: int<year> option
                Months: int<month> option
                Weeks: int<week> option
                Days: int<day> option
            }


        type BirthDate = { Year : int<year>; Month : int<month> option; Day : int<day> option }


        type PatientAge =
            | AgeValue of AgeValue
            | BirthDate of BirthDate
            | UnknownAge


        type WeightValue = | Kilogram of decimal<kg> | Gram of int<gram>


        type HeightValue = | Meter of decimal<m> | Centimeter of decimal<cm>


        type Weight =
            {
                Actual : WeightAtDate list
                Birth : WeightValue option
                Admission : WeightAtDate list
                Calculation : WeightAtDate option
            }
        and WeightAtDate = { Date : DateTime; Weight : WeightValue }


        type EstimatedWeight = { Weight: WeightValue; SD : WeightValue }


        type Height =
            {
                Actual : HeightAtDate list
                Birth : HeightValue option
                Calculation : HeightAtDate option
            }
        and HeightAtDate = { Date : DateTime; Height : HeightValue }


        type EstimatedHeight = { Height : HeightValue; SD : HeightValue }


        type Patient =
            {
                Department : Department
                Diagnoses : string []
                Gender : Gender
                Age : PatientAge
                Weight : Weight
                Height : Height
                GestationalAge : AgeWeeksDays option
                EnteralAccess : EnteralAccess
                VenousAccess : VenousAccess
            }

        and AgeWeeksDays = { Weeks: int<week>; Days : int<day> }


