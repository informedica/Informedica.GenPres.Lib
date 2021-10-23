namespace Informedica.GenOrder.Lib

module List =
    
    let hasExactlyOne pred xs =
        xs
        |> List.filter pred
        |> List.length = 1


    let tryFindInList pred xs = 
        xs
        |> List.collect id
        |> List.tryFind pred
