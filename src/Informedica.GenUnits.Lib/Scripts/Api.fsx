
//#I "C:\Development\Informedica\libs\GenUnits\src\Informedica.GenUnits.Lib\scripts"
//#I __SOURCE_DIRECTORY__

#load "load-utils.fsx"
#load "load.fsx"

open MathNet.Numerics

open Informedica.GenUnits.Lib
open Informedica.Utils.Lib.BCL

Api.eval "1 mg[Mass] / 1 piece[General]"
|> (/) (Api.eval "1 mg[Mass] / 1 mg[Mass]")
|> ValueUnit.toString ValueUnit.Units.English ValueUnit.Units.Verbal.Short

Api.eval "100 mg[Mass] * 200 mg[Mass] / 5 ml[Volume] / 10 times[Count]"
|> ValueUnit.toString ValueUnit.Units.English ValueUnit.Units.Verbal.Short


