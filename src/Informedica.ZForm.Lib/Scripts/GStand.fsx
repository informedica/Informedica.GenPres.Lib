
#load "load.fsx"


open Informedica.ZForm.Lib


type Config = GStand.CreateConfig


let config =
    {
        Config.UseAll = true
        Config.IsRate = false
        Config.SubstanceUnit = None
        Config.TimeUnit = None
    }


let path = $"{__SOURCE_DIRECTORY__}/gstand.md"


GStand.createDoseRules config None None None None "paracetamol" "" ""
|> Seq.map (fun dr ->
    dr
    |> DoseRule.toString true
)
|> fun s -> System.IO.File.WriteAllText(path, s |> String.concat "\n")


