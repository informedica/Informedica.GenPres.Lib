

#load "load.fsx"


#load "../Types.fs"
#load "../Data.fs"
#load "../Utils.fs"
#load "../MetaVision.fs"


open Informedica.ZIndex.Lib
open Informedica.MetaVision.Lib

$"{Both}"

GenPresProduct.get true
|> MetaVision.createImport { MetaVision.config with IncludeAssortment = [| UMCU; ICC; ICK;  NEO |] }




open MathNet.Numerics

((1N/25N)*1000N)/(60N)
|> BigRational.ToDouble

((2N/3000N) * (60N) * (48N)) / 10N
|> BigRational.ToDouble

48N * (155N/1000N) / 50N
|> BigRational.ToDouble

(48N / 25N) / 5N


1152N/25N/100N

24N / 500N

24N / 25N / 10N

(24N - (24N/25N)) / 10N

24N/250N

48N/50N

(24N * 31N)/100N/24N

48N * (31N/200N) // 24N

186N / 50N

31N/40N

93N/250N

186N/250N