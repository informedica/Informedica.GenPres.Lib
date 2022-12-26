namespace Informedica.GenOrder.Lib


module Patient =

    open DocumentFormat.OpenXml.Bibliography

    open DocumentFormat.OpenXml.Drawing.Charts
    open Informedica.GenForm.Lib

    let patient : Patient =
        {
            Department = ""
            Diagnoses = [||]
            Gender = Gender.AnyGender
            Age = None
            Weight = None
            Height = None
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

        static member Height_ =
            (fun (p : Patient) -> p.Height), (fun b (p : Patient) -> { p with Height = b})

        static member GestAge_ =
            (fun (p : Patient) -> p.GestAge), (fun a (p : Patient) -> { p with GestAge = a})

        static member PMAge_ =
            (fun (p : Patient) -> p.PMAge), (fun a (p : Patient) -> { p with PMAge = a})

        static member Department_ =
            (fun (p : Patient) -> p.Department), (fun d (p : Patient) -> { p with Department = d})


    module Optics =

        open Aether
        open Aether.Operators

        let getGender = Optic.get Patient.Gender_


        let setGender = Optic.set Patient.Gender_

        let getAge = Optic.get Patient.Age_


        let setAge = Optic.set Patient.Age_

        let getWeight = Optic.get Patient.Weight_


        let setWeight = Optic.set Patient.Weight_

        let getHeight = Optic.get Patient.Height_


        let setHeight = Optic.set Patient.Height_

        let getGestAge = Optic.get Patient.GestAge_


        let setGestAge = Optic.set Patient.GestAge_

        let getPMAge = Optic.get Patient.PMAge_

        let setPMAGE = Optic.set Patient.PMAge_

        let getDepartment = Optic.get Patient.Department_

        let setDepartment = Optic.set Patient.Department_


