
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


GStand.createDoseRules config None None None (Some 00161527) "" "" ""
|> Seq.map (fun dr ->
    dr
    |> DoseRule.toString true
)
