namespace Informedica.GenOrder.Lib


module Patient =

    open DocumentFormat.OpenXml.Bibliography

    open Informedica.GenForm.Lib

    let patient : Patient =
        {
            Department = ""
            Diagnoses = [||]
            Gender = Gender.AnyGender
            Age = None
            Weight = None
            BSA = None
            GestAge = None
            PMAge = None
            Location = AnyLocation
        }


    type Patient with

        static member Gender_ =
            (fun (p : Patient) -> p.Gender), (fun g (p : Patient) -> { p with Gender = g})

        static member Age_ =
            (fun (p : Patient) -> p.Age), (fun a (p : Patient) -> { p with Age = a})

        static member Weight_ =
            (fun (p : Patient) -> p.Weight), (fun w (p : Patient) -> { p with Weight = w})

        static member BSA_ =
            (fun (p : Patient) -> p.BSA), (fun b (p : Patient) -> { p with BSA = b})

        static member GestAge_ =
            (fun (p : Patient) -> p.GestAge), (fun a (p : Patient) -> { p with GestAge = a})

        static member PMAge_ =
            (fun (p : Patient) -> p.PMAge), (fun a (p : Patient) -> { p with PMAge = a})



    module Optics =

        open Aether
        open Aether.Operators

        let getGender = Optic.get Patient.Gender_


        let setGender = Optic.set Patient.Gender_

        let getAge = Optic.get Patient.Age_


        let setAge = Optic.set Patient.Age_

        let getWeight = Optic.get Patient.Weight_


        let setWeight = Optic.set Patient.Weight_

        let getBSA = Optic.get Patient.BSA_


        let setBSA = Optic.set Patient.BSA_

        let getGestAge = Optic.get Patient.GestAge_


        let setGestAge = Optic.set Patient.GestAge_

        let getPMAge = Optic.get Patient.PMAge_

        let setPMAGE = Optic.set Patient.PMAge_


