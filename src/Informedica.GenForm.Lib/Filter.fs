namespace Informedica.GenForm.Lib


module Filter =

    open Informedica.GenCore.Lib.Types.GenForm


    let filter =
        {
            Indication = None
            Generic = None
            Shape = None
            Route = None
            Department = None
            Diagnoses = [||]
            Gender = AnyGender
            Age = None
            Weight = None
            BSA = None
            GestAge = None
            PMAge = None
            DoseType = AnyDoseType
            Dose = None
            Location = AnyAccess
        }


    let setPatient (pat : Patient) (filter : Filter) =
        { filter with
            Department = pat.Department |> Some
            Diagnoses = pat.Diagnoses
            Gender = pat.Gender
            Age = pat.Age
            Weight = pat.Weight
            BSA = pat |> Patient.calcBSA
            GestAge = pat.GestAge
            PMAge = pat.PMAge
            Location = pat.Location
        }


