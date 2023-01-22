namespace Informedica.ZIndex.Lib


module ProductRange =

    open Informedica.Utils.Lib
    open Informedica.Utils.Lib.BCL


    // GPK;ATC;HoofdGroep;SubGroep;Generiek;Product;Etiket;Vorm;Route;Sterkte;Eenheid;StandDose;DoseEenheid;Indicaties;InFormDb
    type ProductRange =
        {
            GPK: int Option
            ATC: string
            MainGroup: string
            SubGroup: string
            Generic: string
            Product: string
            Label: string
            Shape: string
            Route: string
            Concentration: decimal Option
            ConcentrationUnit : string
            DoseMultiple: decimal Option
            DoseMultipleUnit: string
            Indications: string
        }


    let create gpk atc grp sub gen prod lbl shp route conc concU dose doseU inds =
        {
            GPK = gpk // 0
            ATC = atc
            MainGroup = grp
            SubGroup = sub
            Generic = gen
            Product = prod
            Label = lbl
            Shape = shp
            Route = route
            Concentration = conc // 9
            ConcentrationUnit = concU
            DoseMultiple = dose // 11
            DoseMultipleUnit = doseU
            Indications = inds
        }


    let data_ () =
        File.readAllLines FilePath.mapping
        |> Array.skip 1
        |> Array.map  ((String.splitAt ';') >> (fun sa ->
            create (sa[0] |> Int32.tryParse)
                   sa[1]
                   sa[2]
                   sa[3]
                   sa[4]
                   sa[5]
                   sa[6]
                   sa[7]
                   sa[8]
                   (sa[9] |> Decimal.tryParse)
                   sa[10]
                   (sa[11] |> Decimal.tryParse)
                   sa[12]
                   sa[13]
            )
        )

    let data : unit -> ProductRange [] = Memoization.memoize data_