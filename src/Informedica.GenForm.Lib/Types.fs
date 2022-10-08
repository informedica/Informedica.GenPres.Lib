namespace Informedica.GenForm.Lib


[<AutoOpen>]
module Types =

    open MathNet.Numerics

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
            ShapeQuantity : BigRational option
            ShapeUnit : string
            ShapeVolume : BigRational option
            Divisible : BigRational option
            Substances : Substance array
        }


    type Gender = Male | Female | AnyGender


    type MinMax = { Minimum : BigRational option; Maximum : BigRational option }


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


    type Frequency = { Count : BigRational; TimeUnit : string }


    type DoseLimit =
        {
            Substance : string
            NormDoseQuantity : BigRational option
            DoseQuantity : MinMax
            NormDoseQuantityAdjust : BigRational option
            DoseQuantityAdjust : MinMax
            NormDoseTotal : BigRational option
            DoseTotal : MinMax
            NormDoseTotalAdjust : BigRational option
            DoseTotalAdjust : MinMax
            NormDoseRate : BigRational option
            DoseRate : MinMax
            NormDoseRateAdjust : BigRational option
            DoseRateAdjust : MinMax
        }


    type Patient =
        {
            Diagnosis : string
            Gender : Gender
            Age : MinMax
            Weight : MinMax
            BSA : MinMax
            GestAge : MinMax
            PMAge : MinMax
        }


    type DoseRule =
        {
            Indication : string
            Generic : string
            Shape : string
            Route : string
            Department : string
            Patient : Patient
            DoseType : DoseType
            Frequencies : BigRational array
            DoseUnit : string
            AdjustUnit : string
            FreqUnit : string
            RateUnit : string
            Time : MinMax
            TimeUnit : string
            Interval : MinMax
            IntervalUnit : string
            Duration : MinMax
            DurationUnit : string
            DoseLimits : DoseLimit array
            Products : Product array
        }

    type Filter =
        {
            Indication : string option
            Generic : string option
            Shape : string option
            Route : string option
            Department : string option
            Diagnosis : string option
            Gender : Gender
            Age : BigRational option
            Weight : BigRational option
            BSA : BigRational option
            GestAge : BigRational option
            PMAge : BigRational option
            DoseType : DoseType
        }



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


    type Selector =
        {
            Generic : string
            Shape : string
            Department : string
            Location : Location
            Age : MinMax
            Weight : MinMax
            Dose : MinMax
            DoseType : DoseType
        }


    type SolutionRule =
        {
            Selector : Selector
            Solutions : string []
            Volumes : BigRational []
            Volume : MinMax
            DosePerc : MinMax
            Time : MinMax
            MaxRate : BigRational option
            RateUnit : string
            Products : Product []
            SolutionLimits : SolutionLimit []
        }

