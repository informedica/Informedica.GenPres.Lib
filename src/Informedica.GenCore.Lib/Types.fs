namespace Informedica.GenCore.Lib


module Types =


    open MathNet.Numerics


    module ZIndex =


        module Names =

            type Name = Full | Short | Memo | Label


            type Length = TwentyFive | Fifty


            type Item =
                | Shape
                | Route
                | GenericUnit
                | ShapeUnit
                | PrescriptionContainer
                | ConsumerContainer


        module Route =

            // * ARTI/LESION
            // * AURICULAIR
            // * CUTAAN
            // DENTAAL
            // ENDOCERVIC
            // * ENDOTR.PULM
            // * ENDOTRACHEOPULMONAIR
            // * EPIDURAAL
            // EPIDURAAL, INTRATHECAAL, PERINEURAAL
            // EXTRACORPORAAL
            // * GASTR-ENTER
            // * IM
            // * INHALATIE
            // INTRA-ART.
            // INTRA-ARTERIEEL
            // INTRA-ARTICULAIR
            // INTRA-OCUL.
            // INTRA-UTERIEN
            // INTRABURSAAL
            // INTRACARDIAAL
            // INTRACAVERNEUS
            // INTRACORONAIR
            // * INTRADERMAAL
            // INTRALAESIONAAL
            // INTRALYMFATISCH
            // * INTRAMUSCULAIR
            // INTRAMUSCULAIR, INTRAVENEUS
            // INTRAMUSCULAIR, SUBCUTAAN
            // INTRAOSSAAL
            // INTRAPERITONEAAL
            // INTRAPLEURAAL
            // INTRATHECAAL
            // * INTRAVENEUS
            // INTRAVENEUS, SUBCUTAAN
            // * INTRAVESIC.
            // * INTRAVESICAAL
            // INTRAVITR.
            // INTRAVITREAAL
            // * IV
            // * LOKAAL
            // * NASAAL
            // * NEUS
            // NIET GESPEC
            // NVT
            // OOG
            // * OOR
            // * ORAAL
            // ORAAL/RECT
            // * OROMUCOSAAL
            // PAR./ORAAL
            // PAR/UTERIEN
            // PAR/VESICAL
            // PARABULBAIR
            // PARENT/RECT
            // PARENTERAAL
            // PERI-ARTICULAIR
            // PERIBULBAIR
            // PERINEURAAL
            // PERITONEAAL
            // * RECTAAL
            // RETROBULBAIR
            // SUBCONJUNCTIVAAL
            // * SUBCUTAAN
            // SUBLINGUAAL
            // * TRANSDERMAAL
            // TRANSDERML
            // URETHRAAL
            // UTERIEN
            // VAGINAAL

            type Route =
                | Alternative of string
                | AUR // AURICULAIR OOR
                | CUT // CUTAAN TRANSDERMAAL TRANSDERML LOKAAL
                | ENDOTR // ENDOTR.PULM ENDOTRACHEOPULMONAIR
                | EPIDUR // EPIDURAAL
                | IM // INTRAMUSCULAIR IM
                | INH // INHALATIE
                | INTRAVESIC // INTRAVESIC. INTRAVESICAAL
                | IV // INTRAVENEUS IV
                | LESION // ARTI/LESION
                | NASAL // NASAAL NEUS
                | ORAL // ORAAL GASTR-ENTER OROMUCOSAAL
                | OROMUCOSAL //OROMUCOSAAL
                | RECTAL // RECTAAL
                | SUBCUT // INTRADERMAAL SUBCUTAAN
                | NoRoute


        type Substance =
            {
                Id : int
                Name : string
                Mole : decimal
                MoleReal : decimal
            }


        type ConsumerProduct =
            {
                Id : int
                Name : string
                Label : string
                Quantity : decimal
                Container : string
                BarCodes : string []
            }


        type TradeProduct =
            {
                Id: int
                Name : string
                Label : string
                Brand : string
                Company : string
                Denominator : int
                UnitWeight : decimal
                Route : string []
                ConsumerProducts : ConsumerProduct []
            }


        type PrescriptionProduct =
            {
                Id : int
                Name : string
                Label  : string
                Quantity : decimal
                Unit : string
                Container : string
                TradeProducts : TradeProduct []
            }


        type GenericProduct =
            {
                Id : int
                Name : string
                Label : string
                ATC : string
                ATCName : string
                Shape : string
                Route : string []
                Substances : ProductSubstance []
                PrescriptionProducts : PrescriptionProduct []
            }

        and ProductSubstance =
            {
                SubstanceId : int
                SortOrder : int
                SubstanceName : string
                SubstanceQuantity : decimal
                SubstanceUnit : string
                GenericId : int
                GenericName : string
                GenericQuantity : decimal
                GenericUnit : string
                ShapeUnit : string
            }


        type GenPresProduct =
            {
                Name : string
                Shape : string
                Routes : string []
                PharmacologicalGroups : string []
                GenericProducts : GenericProduct []
                DisplayName: string
                Unit : string
                Synonyms: string []
            }



        type DoseRule =
            {
                /// The id of the doserule
                Id : int
                /// The caregroup the doserule applies to
                /// this is either 'intensieve' or 'niet-intensieve' or 'all'
                CareGroup : string
                /// This is the usage of the dose rule, can be therapeutic or
                /// profylactic
                Usage : string
                /// The dose type, 'standaard' means that the dose rule applies without
                /// a specific indication, 'verbyzondering' means the dose rule needs
                /// an indication other than 'Algemeen'.
                DoseType : string
                /// The list of generic products for which the dose rule applies
                GenericProduct : RuleGenericProduct[]
                /// The list of prescription products for which the dose rule applies
                PrescriptionProduct : RuleProduct[]
                /// The list of trade products for which the dose rule applies
                TradeProduct : RuleProduct[]
                /// The route for which the dose rule applies
                Routes : string []
                /// The indication id for which the dose rule applies.
                /// The indications are coded by ICPC/ICD-10
                IndicationId : int
                /// The indication text for which the dose rule applies.
                /// The indications are coded by ICPC/ICD-10
                Indication : string
                /// If high risk, than the dose margins are smaller
                HighRisk : bool
                /// Gender is either 'man', 'vrouw' or an empty string.
                /// When gender is empty the dose rule can apply to either
                /// gender.
                Gender : string
                /// The optional minimum or maximum age limits for the dose rule
                Age : RuleMinMax
                /// The optional minimum or maximum weight limits for which the dose
                /// rule applies
                Weight : RuleMinMax
                /// The optional BSA min/max for which the dose rule applies
                BSA : RuleMinMax
                /// The frequency of the dose rule. The total dose can be calculated
                /// by multiplying the dose by the frequency.
                Freq : RuleFrequency
                /// The normal optional min/max of the unadjusted dose
                Norm : RuleMinMax
                /// The absolute optional min/max of the unadjusted dose
                Abs : RuleMinMax
                /// The normal optional min/max of the dose adjusted by weight
                NormKg : RuleMinMax
                /// The absolute optional min/max of the dose adjusted by weight
                AbsKg : RuleMinMax
                /// The absolute optional min/max of the dose adjusted by BSA
                NormM2 : RuleMinMax
                /// The absolute optional min/max of the dose adjusted by BSA
                AbsM2 : RuleMinMax
                /// The unit in which the dose is measured
                Unit : string
            }

            static member Weight_ :
                (DoseRule -> RuleMinMax) * (RuleMinMax -> DoseRule -> DoseRule) =
                (fun dr -> dr.Weight) ,
                (fun mm dr -> { dr with Weight = mm })

            static member BSA_ :
                (DoseRule -> RuleMinMax) * (RuleMinMax -> DoseRule -> DoseRule) =
                (fun dr -> dr.BSA) ,
                (fun mm dr -> { dr with BSA = mm })

            static member Norm_ :
                (DoseRule -> RuleMinMax) * (RuleMinMax -> DoseRule -> DoseRule) =
                (fun dr -> dr.Norm) ,
                (fun mm dr -> { dr with Norm = mm })

            static member Abs_ :
                (DoseRule -> RuleMinMax) * (RuleMinMax -> DoseRule -> DoseRule) =
                (fun dr -> dr.Abs) ,
                (fun mm dr -> { dr with Abs = mm })

            static member NormKg_ :
                (DoseRule -> RuleMinMax) * (RuleMinMax -> DoseRule -> DoseRule) =
                (fun dr -> dr.NormKg) ,
                (fun mm dr -> { dr with NormKg = mm })

            static member AbsKg_ :
                (DoseRule -> RuleMinMax) * (RuleMinMax -> DoseRule -> DoseRule) =
                (fun dr -> dr.AbsKg) ,
                (fun mm dr -> { dr with AbsKg = mm })

            static member NormM2_ :
                (DoseRule -> RuleMinMax) * (RuleMinMax -> DoseRule -> DoseRule) =
                (fun dr -> dr.NormM2) ,
                (fun mm dr -> { dr with NormM2 = mm })

            static member AbsM2_ :
                (DoseRule -> RuleMinMax) * (RuleMinMax -> DoseRule -> DoseRule) =
                (fun dr -> dr.AbsM2) ,
                (fun mm dr -> { dr with AbsM2 = mm })

        and RuleProduct = { Id: int; Name: string }

        and RuleGenericProduct =
            {
                Id: int
                Name: string
                Route: string []
                Unit: string
                Substances : RuleSubstance []
            }

        and RuleSubstance = { Name: string; Quantity: decimal; Unit: string }

        and RuleFrequency = { Frequency: decimal; Time: string }

        and RuleMinMax = { Min: decimal Option; Max: decimal Option }


        type ATCGroup =
            {
                ATC1 : string
                AnatomicalGroup : string
                AnatomicalGroupEng : string
                ATC2 : string
                TherapeuticMainGroup : string
                TherapeuticMainGroupEng : string
                ATC3 : string
                TherapeuticSubGroup : string
                TherapeuticSubGroupEng : string
                ATC4 : string
                PharmacologicalGroup : string
                PharmacologicalGroupEng : string
                ATC5 : string
                Substance : string
                SubstanceEng : string
                Generic : string
                Shape : string
                Routes : string
            }


        type AgeInMo = decimal Option

        type WeightInKg = decimal Option

        type BSAInM2 = decimal Option


        type PatientFilter =
            {
                Age: AgeInMo
                Weight: WeightInKg
                BSA: BSAInM2
            }


        type ProductFilter =
            | GPKRoute of (int * string)
            | GenericShapeRoute of GenericShapeRoute

        and GenericShapeRoute =
            {
                Generic: string
                Shape: string
                Route: string
            }


        type Filter =
            {
                Patient: PatientFilter
                Product: ProductFilter
            }


        type RuleResult =
            {
                Product: GenPresProduct
                DoseRules: string []
                Doses: FreqDose []
            }

        and FreqDose =
            {
                /// The frequency of the dose rule
                Freq: RuleFrequency
                /// The optional min/max values of a 'normal dose range'
                NormDose: RuleMinMax
                /// The optional min/max values of the 'absolute dose range'
                AbsDose: RuleMinMax
                /// The optional min/max values of a 'normal dose range' per kg
                NormKg: RuleMinMax
                /// The optional min/max values of the 'absolute dose range' per kg
                AbsKg: RuleMinMax
                /// The optional min/max values of a 'normal dose range' per m2
                NormM2: RuleMinMax
                /// The optional min/max values of the 'absolute dose range' per m2
                AbsM2: RuleMinMax
                /// The unit in which the doses are measured
                Unit: string
            }



    module ZForm =

        open Informedica.GenUnits.Lib

        type Route = ZIndex.Route.Route
        type MinMax = MinMax.MinMax


        /// A product is a medical substance or
        /// substances with a specific shape
        type Product =
            {
                // The name of the product which is the generic
                // substance of the product or a concatenation of
                // generic substance names or a 'name'.
                Name : string
                /// The pharmacological shape of a product.
                DisplayName : string
                Synonyms : string list
                Shape : string
                /// The route of a product
                Unit : string
                Routes : Route list
                Pharmacologic : string list
                /// The display name of the generic
                DivisibleBy : Divisibility
                GenericProducts : GenericProduct List
            }

        and Divisibility = NoDiv | Div of bigint

        and GenericProduct =
            {
                Id : int
                Label : string
                /// The substances on which the concentration and dosing is based.
                Substances : Substance list
                TradeProducts : TradeProduct list
            }

        and Substance =
            {
                Name : string
                Concentration : ValueUnit
            }

        and TradeProduct =
            {
                Id : int
                Names : string
                Label : string
                Quantity : ValueUnit
            }


        type Patient =
            {
                GestAge : MinMax
                Age : MinMax
                Weight : MinMax
                BSA : MinMax
                Gender : Gender
            }

            static member GestAge_ :
                (Patient -> MinMax) * (MinMax -> Patient -> Patient) =
                (fun p -> p.GestAge), (fun a p -> { p with GestAge = a })

            static member Age_ :
                (Patient -> MinMax) * (MinMax -> Patient -> Patient) =
                (fun p -> p.Age), (fun a p -> { p with Age = a })

            static member Weight_ :
                (Patient -> MinMax) * (MinMax -> Patient -> Patient) =
                (fun p -> p.Weight), (fun w p -> { p with Weight = w })

            static member BSA_ :
                (Patient -> MinMax) * (MinMax -> Patient -> Patient) =
                (fun p -> p.BSA), (fun b p -> { p with BSA = b })

            static member Gender_ :
                (Patient -> Gender) * (Gender -> Patient -> Patient) =
                (fun p -> p.Gender), (fun g p -> { p with Gender = g })

        and Gender = Male | Female | Undetermined




        /// Dose limits
        type DoseRange =
            {
                // Normal limits
                Norm : MinMax
                // Normal limits adjusted by weight
                NormWeight : MinMax * Unit
                // Normal limits adjusted by BSA
                NormBSA : MinMax * Unit
                // Absolute limits
                Abs : MinMax
                // Absolute limits adjusted by weight
                AbsWeight : MinMax * Unit
                // Absolute limits adjusted by BSA
                AbsBSA : MinMax * Unit
            }
            static member Norm_ :
                (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
                (fun dr -> dr.Norm),
                (fun mm dr -> { dr with Norm = mm })

            static member NormWeight_ :
                (DoseRange -> MinMax * Unit) * (MinMax * Unit -> DoseRange -> DoseRange) =
                (fun dr -> dr.NormWeight),
                (fun mm dr -> { dr with NormWeight = mm })

            static member NormBSA_ :
                (DoseRange -> MinMax * Unit) * (MinMax * Unit -> DoseRange -> DoseRange) =
                (fun dr -> dr.NormBSA),
                (fun mm dr -> { dr with NormBSA = mm })

            static member Abs_ :
                (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
                (fun dr -> dr.Abs),
                (fun mm dr -> { dr with Abs = mm })

            static member AbsWeight_ :
                (DoseRange -> MinMax * Unit) * (MinMax * Unit -> DoseRange -> DoseRange) =
                (fun dr -> dr.AbsWeight),
                (fun mm dr -> { dr with AbsWeight = mm })

            static member AbsBSA_ :
                (DoseRange -> MinMax * Unit) * (MinMax * Unit -> DoseRange -> DoseRange) =
                (fun dr -> dr.AbsBSA),
                (fun mm dr -> { dr with AbsBSA = mm })


        /// Dosage
        type Dosage =
            {
                /// Indentifies the indication
                Name : string
                /// Dosage at the start
                StartDosage : DoseRange
                /// Dosage per administration
                SingleDosage : DoseRange
                /// Dosage rate
                RateDosage : DoseRange * RateUnit
                /// Total dosage per time period
                TotalDosage : DoseRange * Frequency
                /// List of original doserules
                Rules : Rule list
            }
            static member Name_ :
                (Dosage -> string) * (string -> Dosage -> Dosage) =
                (fun d -> d.Name),
                (fun s d -> { d with Name = s })

            static member StartDosage_ :
                (Dosage -> DoseRange) * (DoseRange -> Dosage -> Dosage) =
                (fun d -> d.StartDosage),
                (fun dr d -> { d with StartDosage = dr })

            static member SingleDosage_ :
                (Dosage -> DoseRange) * (DoseRange -> Dosage -> Dosage) =
                (fun d -> d.SingleDosage),
                (fun dr d -> { d with SingleDosage = dr })

            static member RateDosage_ :
                (Dosage -> DoseRange * RateUnit) * (DoseRange * RateUnit -> Dosage -> Dosage) =
                (fun d -> d.RateDosage),
                (fun dr d -> { d with RateDosage = dr })

            static member TotalDosage_ :
                (Dosage -> DoseRange * Frequency) * (DoseRange * Frequency -> Dosage -> Dosage) =
                (fun d -> d.TotalDosage),
                (fun dt d -> { d with TotalDosage = dt })

            static member Rules_ :
                (Dosage -> Rule list) * (Rule list -> Dosage -> Dosage) =
                (fun d -> d.Rules) ,
                (fun rs d -> { d with Rules = rs })

        and Frequency =
            {
                Frequencies : Frequencies
                TimeUnit : TimeUnit
                MinimalInterval : ValueUnit Option
            }
            static member Frequencies_ :
                (Frequency -> Frequencies) * (Frequencies -> Frequency -> Frequency) =
                (fun fr -> fr.Frequencies) ,
                (fun frs fr -> { fr with Frequencies = frs })

            static member TimeUnit_ :
                (Frequency -> Unit) * (Unit -> Frequency -> Frequency) =
                (fun fr -> fr.TimeUnit) ,
                (fun tu fr -> { fr with TimeUnit = tu })

            static member MinimalInterval_ :
                (Frequency -> ValueUnit Option) * (ValueUnit Option -> Frequency -> Frequency) =
                (fun fr -> fr.MinimalInterval) ,
                (fun mi fr -> { fr with MinimalInterval = mi })

        and Frequencies = BigRational list

        and TimeUnit = Unit

        and RateUnit = Unit

        and Rule = GStandRule of string | PedFormRule of string


        type PatientDosage =
            {
                // The patient group the doserules applies
                Patient : Patient
                // List of shapes that have a dosage
                ShapeDosage : Dosage
                // List of substances that have a dosage
                SubstanceDosages : Dosage list
            }
            static member Patient_ :
                (PatientDosage -> Patient) * (Patient -> PatientDosage -> PatientDosage) =
                (fun pd -> pd.Patient) ,
                (fun pat pd -> { pd with Patient = pat })

            static member ShapeDosage_ :
                (PatientDosage -> Dosage) * (Dosage -> PatientDosage -> PatientDosage) =
                (fun pd -> pd.ShapeDosage) ,
                (fun sd pd -> { pd with ShapeDosage = sd })

            static member SubstanceDosages_ :
                (PatientDosage -> Dosage list) * (Dosage list -> PatientDosage -> PatientDosage) =
                (fun sd -> sd.SubstanceDosages) ,
                (fun d sd -> { sd with SubstanceDosages = d })


        type TradeProductLabel =
            { HPK : int; Label : string }
            static member HPK_ :
                (TradeProductLabel -> int) * (int -> TradeProductLabel -> TradeProductLabel) =
                (fun tp -> tp.HPK) ,
                (fun hpk tp -> { tp with HPK = hpk })


            static member Label_ :
                (TradeProductLabel -> string) * (string -> TradeProductLabel -> TradeProductLabel) =
                (fun tp -> tp.Label) ,
                (fun lbl tp -> { tp with Label = lbl })


        type GenericProductLabel =
            { GPK : int; Label : string }
            static member GPK_ :
                (GenericProductLabel -> int) * (int -> GenericProductLabel -> GenericProductLabel) =
                (fun tp -> tp.GPK) ,
                (fun hpk tp -> { tp with GPK = hpk })


            static member Label_ :
                (GenericProductLabel -> string) * (string -> GenericProductLabel -> GenericProductLabel) =
                (fun tp -> tp.Label) ,
                (fun lbl tp -> { tp with Label = lbl })


        type ShapeDosage =
            {
                // Name of the shape the doserule applies to
                Shape : string list
                // TradeProducts the doserule applies to
                TradeProducts : TradeProductLabel list
                // GenericProducts the doserule applies to
                GenericProducts : GenericProductLabel list
                // Patients to wich the doserule applies to
                PatientDosages : PatientDosage list
            }

            static member Shape_ :
                (ShapeDosage -> string list) * (string list -> ShapeDosage -> ShapeDosage) =
                (fun rd -> rd.Shape) ,
                (fun s rd -> { rd with Shape = s })

            static member TradeProducts_ :
                (ShapeDosage -> TradeProductLabel list) * (TradeProductLabel list -> ShapeDosage -> ShapeDosage) =
                (fun sd -> sd.TradeProducts) ,
                (fun tps sd -> { sd with TradeProducts = tps |> List.distinct })

            static member GenericProducts_ :
                (ShapeDosage -> GenericProductLabel list) * (GenericProductLabel list -> ShapeDosage -> ShapeDosage) =
                (fun sd -> sd.GenericProducts) ,
                (fun tps sd -> { sd with GenericProducts = tps |> List.distinct })

            static member PatientDosages_ :
                (ShapeDosage -> PatientDosage list) * (PatientDosage list -> ShapeDosage -> ShapeDosage) =
                (fun rd -> rd.PatientDosages) ,
                (fun pdl rd -> { rd with PatientDosages = pdl })


        type RouteDosage =
            {
                // Administration route
                Route : string
                // The dosage rules per shape
                ShapeDosages : ShapeDosage list
            }
            static member Route_ :
                (RouteDosage -> string) * (string -> RouteDosage -> RouteDosage) =
                (fun rd -> rd.Route) ,
                (fun s rd -> { rd with Route = s })

            static member ShapeDosages_ :
                (RouteDosage -> ShapeDosage list) * (ShapeDosage list -> RouteDosage -> RouteDosage) =
                (fun rd -> rd.ShapeDosages) ,
                (fun pdl rd -> { rd with ShapeDosages = pdl })


        type IndicationDosage =
            {
                // The indication(-s) the dose rule applies to
                Indications : string list
                // The dosage rules per administration route
                RouteDosages : RouteDosage list
            }
            static member Indications_ :
                (IndicationDosage -> string list) * (string list -> IndicationDosage -> IndicationDosage) =
                (fun inds -> inds.Indications) ,
                (fun sl inds -> { inds with Indications = sl })

            static member RouteDosages_ :
                (IndicationDosage -> RouteDosage list) * (RouteDosage list -> IndicationDosage -> IndicationDosage) =
                (fun inds -> inds.RouteDosages) ,
                (fun rdl inds -> { inds with RouteDosages = rdl })


        /// Doserule
        type DoseRule =
            {
                // Generic the doserule applies to
                Generic : string
                // List of synonyms for the generic
                Synonyms : string list
                // The ATC code
                ATC : string
                // ATCTherapyGroup the doserule applies to
                ATCTherapyGroup : string
                // ATCTherapySubGroup the doserule applies to
                ATCTherapySubGroup : string
                // The generic group the doserule applies to
                GenericGroup : string
                // The generic subgroup the doserule applies to
                GenericSubGroup : string
                // The doserules per indication(-s)
                IndicationsDosages : IndicationDosage list
            }
            static member Generic_ :
                (DoseRule -> string) * (string -> DoseRule -> DoseRule) =
                (fun dr -> dr.Generic),
                (fun s dr -> { dr with Generic = s })

            static member Synonyms_ :
                (DoseRule -> string list) * (string list -> DoseRule -> DoseRule) =
                (fun dr -> dr.Synonyms) ,
                (fun sns dr -> { dr with Synonyms = sns |> List.distinct })


            static member IndicationDosages_ :
                (DoseRule -> IndicationDosage list) * (IndicationDosage list -> DoseRule -> DoseRule) =
                (fun dr -> dr.IndicationsDosages) ,
                (fun inds dr -> { dr with IndicationsDosages = inds })


        type TextConfig =
            {
                MainText: string
                IndicationText : string
                RouteText : string
                ShapeText : string
                PatientText : string
                DosageText : string
            }


    module GenForm =

        type VenousAccess =
            | PVL
            | CVL
            | AnyAccess


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
                RequiresReconstitution : bool
                Reconstitution : Reconstitution []
                Divisible : BigRational option
                Substances : Substance array
            }
        and Reconstitution =
            {
                Route : string
                DoseType: DoseType
                Department : string
                Location : VenousAccess
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
                Location : VenousAccess
            }


        type Patient =
            {
                Department : string
                Diagnoses : string []
                Gender : Gender
                Age : BigRational option
                Weight : BigRational option
                Height : BigRational option
                GestAge : BigRational option
                PMAge : BigRational option
                Location : VenousAccess
            }
            static member Gender_ =
                (fun (p : Patient) -> p.Gender), (fun g (p : Patient) -> { p with Gender = g})

            static member Age_ =
                (fun (p : Patient) -> p.Age), (fun a (p : Patient) -> { p with Age = a})

            static member Weight_ =
                (fun (p : Patient) -> p.Weight), (fun w (p : Patient) -> { p with Weight = w})

            static member Height_ =
                (fun (p : Patient) -> p.Height), (fun b (p : Patient) -> { p with Height = b})

            static member GestAge_ =
                (fun (p : Patient) -> p.GestAge), (fun a (p : Patient) -> { p with GestAge = a})

            static member PMAge_ =
                (fun (p : Patient) -> p.PMAge), (fun a (p : Patient) -> { p with PMAge = a})

            static member Department_ =
                (fun (p : Patient) -> p.Department), (fun d (p : Patient) -> { p with Department = d})


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
                Location : VenousAccess
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
                Location : VenousAccess
            }


        type PrescriptionRule =
            {
                Patient : Patient
                DoseRule : DoseRule
                SolutionRule : SolutionRule option
            }


    module GenOrder =


        open System
        open Informedica.GenUnits.Lib
        open Informedica.GenSolver.Lib.Types


        type Gender = GenForm.Gender
        type Patient = GenForm.Patient


        /// A `VariableUnit` is the combination of
        /// an `Informedica.GenSolver.Lib.Variable` with
        /// an `Informedica.GenUnits.Lib.Unit`
        /// The `Variable` stores the base values according
        /// to the `Unit`
        type OrderVariable =
            {
                Constraints : Constraints
                /// Stores the values/range
                Variable:  Variable
                /// Stores the unit
                Unit: Unit
            }
        and Constraints =
            {
                    Min : Minimum option
                    Max : Maximum option
                    Incr : Increment option
                    Values : ValueSet option
            }


        /// An order equation is either a product equation or a
        /// sum equation
        type OrderEquation =
            | OrderProductEquation of OrderVariable * OrderVariable list
            | OrderSumEquation of OrderVariable * OrderVariable list


        /// Time "tme"
        /// Type that represents a time
        type Time = Time of OrderVariable


        /// Count "cnt"
        /// Type that represents a count
        type Count = Count of OrderVariable


        /// Count / Time "frq"
        /// Type that represents a frequency
        type Frequency = Frequency of OrderVariable


        /// Quantity "qty"
        /// Type that represents a quantity
        type Quantity = Quantity of OrderVariable


        //// Quantity / Time "ptm"
        /// Type that represents a quantity per time
        type PerTime = PerTime of OrderVariable

        /// Quantity / Time "rte"
        /// Type that represents a rate
        type Rate = Rate of OrderVariable


        /// Quantity "tot"
        /// Type that represents a total
        type Total = Total of OrderVariable


        /// Quantity / Quantity "cnc"
        /// Type that represents a concentration
        type Concentration = Concentration of OrderVariable


        /// Quantity / Adjust "qty_adj"
        /// Type that represents a adjusted quantity
        type QuantityAdjust = QuantityAdjust of OrderVariable


        /// Quantity / Adjust / Time "ptm_adj"
        /// Type that represents a adjusted quantity per time
        type PerTimeAdjust = PerTimeAdjust of OrderVariable


        /// Quantity / Adjust / Time "rte_adj"
        /// Type that represents a adjusted quantity per time
        type RateAdjust = RateAdjust of OrderVariable


        /// Quantity / Adjust "tot_adj"
        /// Type that represents a adjusted total
        type TotalAdjust = TotalAdjust of OrderVariable



        /// An Id is represented by a string
        type Id = Id of string


        type Dose =
            {
                Quantity : Quantity
                PerTime : PerTime
                Rate : Rate
                Total : Total
                QuantityAdjust : QuantityAdjust
                PerTimeAdjust : PerTimeAdjust
                RateAdjust : RateAdjust
                TotalAdjust : TotalAdjust
            }


        /// Models an `Item` in a `Component`
        type Item =
            {
                /// The name of the item
                Name: Name
                /// The quantity of an `Item` in a `Component`
                ComponentQuantity: Quantity
                /// The quantity of an `Item` in an `Orderable`
                OrderableQuantity: Quantity
                /// The `Item` concentration in a `Component`
                ComponentConcentration: Concentration
                /// The  `Item` concentration in an `Orderable`
                OrderableConcentration: Concentration
                /// The `Item` `Dose`, i.e. quantity, total and rate of `Item` administered
                Dose: Dose
            }


        /// Models in a `Component` in and `Orderable`
        type Component =
            {
                Id : Id
                /// The name of a `Component`
                Name: Name
                // The shape of an component
                Shape : string
                /// The quantity of a `Component`
                ComponentQuantity: Quantity
                /// The quantity of a `Component` in an `Orderable`
                OrderableQuantity: Quantity
                /// The count of a `Component` in an `Orderable`
                OrderableCount: Count
                /// The quantity of a `Component` in an `Order`
                OrderQuantity: Quantity
                /// The count of a `Component` in an `Order`
                OrderCount: Count
                /// The concentration of a `Component` in an `Orderable`
                OrderableConcentration: Concentration
                // The `Component` `Dose`,
                /// i.e. quantity, total and rate of `Component` administered
                Dose: Dose
                /// The `Item`s in a `Component`
                Items: Item list
            }


        /// Models an `Orderable`
        type Orderable =
            {
                /// The name of the orderable
                Name: Name
                /// The quantity of an orderable
                OrderableQuantity: Quantity
                /// The quantity of an orderable in an order
                OrderQuantity: Quantity
                /// The orderable count in an order
                OrderCount: Count
                // The count of doses in an orderable quantity
                DoseCount: Count
                /// The dose of an orderable
                Dose: Dose
                /// The list of components in an orderable
                Components: Component list
            }


        /// There is always a `Start` or
        /// both a `StartStop`
        type StartStop =
            | Start of DateTime
            | StartStop of DateTime * DateTime


        /// Models an order
        type Order =
            {
                /// The id of an order
                Id: Id
                /// Used to adjust doses
                Adjust: Quantity
                /// That what can be ordered
                Orderable: Orderable
                /// How the orderable is prescribed
                Prescription: Prescription
                /// The route of administration of the order
                Route: string // Route
                /// The duration of an order
                Duration: Time
                /// The start stop date of the order
                StartStop: StartStop
            }


        /// Type that represents a prescription
        and Prescription =
            | Continuous
            /// A discontinuous prescription with a frequency
            | Discontinuous of Frequency
            /// A discontinuous prescription with both frequency and time
            | Timed of Frequency * Time


        type EquationMapping =
            | ProductMapping of string list
            | SumMapping of string list


        /// The different possible order types
        type OrderType =
            | AnyOrder
            | ProcessOrder
            | ContinuousOrder
            | DiscontinuousOrder
            | TimedOrder


        type MinMax = GenForm.MinMax
        type DoseLimit = GenForm.DoseLimit
        type SolutionLimit = GenForm.SolutionLimit


        /// The representation of a drug order that
        /// can be derived by a drug product inventory
        /// and the related dose rule
        type DrugOrder =
            {
                /// Identifies the specific drug order
                Id:  string
                /// The name of the order
                Name : string
                /// The list of drug products that can be used for the order
                Products : ProductComponent list
                /// The quantities of the drug order
                Quantities :  BigRational list
                /// The unit the `DrugOrder` is measured in,
                /// i.e. of the `Quantities`
                Unit : string
                /// The route by which the order is applied
                Route : string
                // The type of order
                OrderType : OrderType
                Frequencies : BigRational list
                /// The time unit to be used when using a frequency
                FreqUnit : string
                Rates : BigRational list
                /// The time unit to be used when using a rate
                RateUnit : string
                Time : MinMax
                /// The time unit for infusion time (duration)
                TimeUnit : string
                Dose : DoseLimit option
                // The amount of orderable that will be given each time
                DoseCount : BigRational option
                // The adjust quantity for the adjusted dose calculations
                Adjust : BigRational option
                // The adjust unit
                AdjustUnit : string
            }
        /// The product components that are used by the drug order
        and ProductComponent =
            {
                /// The name of the product
                Name : string
                /// The shape of the product
                Shape : string
                /// The quantities of the product
                /// Note: measured in the same unit as
                /// the `DrugOrder` unit
                Quantities : BigRational list
                /// The "divisibility" of the products
                Divisible : BigRational
                /// The time unit used for frequency
                TimeUnit : string
                /// The time unit used for rate
                RateUnit : string
                /// The list of substances contained in the product
                Substances: SubstanceItem list
            }
        and SubstanceItem =
            {
                /// The name of the substance
                Name : string
                /// The possible concentrations of the substance
                /// in the products
                Concentrations : BigRational list
                /// The unit by which the substance is
                /// measured.
                Unit : string
                /// The time unit used for the frequency
                TimeUnit : string
                Dose : DoseLimit option
                Solution : SolutionLimit option
            }


        type Scenario =
            {
                No : int
                Indication : string
                Name : string
                Shape : string
                Route : string
                Prescription : string
                Preparation : string
                Administration : string
            }


        module Exceptions =

            type Message =
                | OrderCouldNotBeSolved of string * Order


        module Events =

            type Event =
                | SolverReplaceUnit of (Name * Unit)
                | OrderSolveStarted of Order
                | OrderSolveFinished of Order
                | OrderSolveConstraintsStarted of Order * Constraint list
                | OrderSolveConstraintsFinished of Order * Constraint list
                | OrderScenario of string
                | OrderScenarioWithNameValue of Order * Name * BigRational


        module Logging =

            open Informedica.GenSolver.Lib.Types.Logging

            type OrderMessage =
                | OrderException of Exceptions.Message
                | OrderEvent of Events.Event
                interface IMessage


