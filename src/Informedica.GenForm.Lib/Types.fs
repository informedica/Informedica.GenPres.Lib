namespace Informedica.GenForm.Lib


[<AutoOpen>]
module Types =

    open Informedica.Utils.Lib.BCL
    open MathNet.Numerics


    type Location =
        | PVL
        | CVL
        | AnyLocation


    type Gender = Male | Female | AnyGender


    type DoseType =
        | Start
        | Once
        | PRN
        | Maintenance
        | Continuous
        | StepDown of int
        | StepUp of int
        | Contraindicated
        | AnyDoseType


    type MinMax = { Minimum : BigRational option; Maximum : BigRational option }


    type Frequency = { Count : BigRational; TimeUnit : string }


    type ShapeRoute =
        {
            Shape : string
            Route : string
            Unit  : string
            DoseUnit : string
            Timed : bool
            Reconstitute : bool
            IsSolution : bool
        }


    type Substance =
        {
            Name : string
            Unit : string
            Quantity : BigRational option
            MultipleQuantity : BigRational option
            MultipleUnit : string
        }


    type Product =
        {
            GPK : string
            ATC : string
            MainGroup : string
            SubGroup : string
            Generic : string
            TallMan : string
            Synonyms : string array
            Product : string
            Label : string
            Shape : string
            ShapeQuantities : BigRational []
            ShapeUnit : string
            Reconstitution : Reconstitution []
            Divisible : BigRational option
            Substances : Substance array
        }
    and Reconstitution =
        {
            Route : string
            DoseType: DoseType
            Department : string
            Location : Location
            DiluentVolume : BigRational
            ExpansionVolume : BigRational option
            Diluents : string []
        }



    type DoseLimit =
        {
            Substance : string
            DoseUnit : string
            RateUnit : string
            NormQuantity : BigRational []
            Quantity : MinMax
            NormQuantityAdjust : BigRational option
            QuantityAdjust : MinMax
            NormPerTime : BigRational []
            PerTime : MinMax
            NormPerTimeAdjust : BigRational option
            PerTimeAdjust : MinMax
            NormRate : BigRational []
            Rate : MinMax
            NormRateAdjust : BigRational option
            RateAdjust : MinMax
        }


    type PatientCategory =
        {
            Department : string option
            Diagnoses : string []
            Gender : Gender
            Age : MinMax
            Weight : MinMax
            BSA : MinMax
            GestAge : MinMax
            PMAge : MinMax
            Location : Location
        }


    type Patient =
        {
            Department : string
            Diagnoses : string []
            Gender : Gender
            Age : BigRational option
            Weight : BigRational option
            BSA : BigRational option
            GestAge : BigRational option
            PMAge : BigRational option
            Location : Location
        }


    type DoseRule =
        {
            Indication : string
            Generic : string
            Shape : string
            Route : string
            Patient : PatientCategory
            AdjustUnit : string
            DoseType : DoseType
            Frequencies : BigRational array
            FreqUnit : string
            Time : MinMax
            TimeUnit : string
            Interval : MinMax
            IntervalUnit : string
            Duration : MinMax
            DurationUnit : string
            DoseLimits : DoseLimit array
            Products : Product array
        }


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
            Route : string
            DoseType : DoseType
            Department : string
            Location : Location
            Age : MinMax
            Weight : MinMax
            Dose : MinMax
            Solutions : string []
            Volumes : BigRational []
            Volume : MinMax
            DosePerc : MinMax
            Products : Product []
            SolutionLimits : SolutionLimit []
        }


    type Filter =
        {
            Indication : string option
            Generic : string option
            Shape : string option
            Route : string option
            Department : string option
            Diagnoses : string []
            Gender : Gender
            Age : BigRational option
            Weight : BigRational option
            BSA : BigRational option
            GestAge : BigRational option
            PMAge : BigRational option
            DoseType : DoseType
            Dose : BigRational option
            Location : Location
        }


    type PrescriptionRule =
        {
            Patient : Patient
            DoseRule : DoseRule
            SolutionRule : SolutionRule option
        }
