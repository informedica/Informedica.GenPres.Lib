namespace Informedica.Utils.Lib

module File =

    open System.IO

    let enumerate path =
        seq { for file in (new DirectoryInfo(path)).EnumerateFiles() do yield file }    

    let readAllLines path = File.ReadAllLines(path)

    let readAllLinesAsync path = 
        async {
            use stream = File.OpenText(path)
            
            return File.ReadAllLines(path) |> Array.toList
        }

    let writeTextToFile path text =
        File.WriteAllText(path, text) 

    let exists path =
        File.Exists(path)
