namespace Informedica.GenOrder.Lib


module Patient =


    type Gender = Informedica.GenForm.Lib.Types.Gender


    let patient =
        {
            Gender = Gender.AnyGender
            Age = None
            Weight = None
            BSA = None
            GestAge = None
            PMAge = None
        }


