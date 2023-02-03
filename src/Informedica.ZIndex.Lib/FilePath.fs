namespace Informedica.ZIndex.Lib


module FilePath =

    open System


    [<Literal>]
    let data = "./data/"

    [<Literal>]
    let GStandPath = data + "zindex/"

    [<Literal>]
    let substanceCache = data + "cache/substance.cache"

    [<Literal>]
    let productCache = data + "cache/product.cache"

    [<Literal>]
    let ruleCache = data + "cache/rule.cache"

    [<Literal>]
    let groupCache = data + "cache/group.cache"

    [<Literal>]
    let mapping = data + "mapping/Formulary.csv"
