namespace Informedica.ZForm.Lib

module Product =


    open Informedica.GenCore.Lib.Types.ZForm


    let empty =
        {
            Name = ""
            DisplayName = ""
            Synonyms = []
            Shape = ""
            Unit = ""
            Routes = []
            Pharmacologic = []
            DivisibleBy = NoDiv
            GenericProducts = []
        }


