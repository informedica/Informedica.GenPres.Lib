
#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: FParsec"


#r "../../Informedica.Utils.Lib/bin/Debug/net6.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.GenUnits.Lib/bin/Debug/net6.0/Informedica.GenUnits.Lib.dll"
#r "../../Informedica.GenSolver.Lib/bin/Debug/net6.0/Informedica.GenSolver.Lib.dll"
#r "../../Informedica.GenForm.Lib/bin/Debug/net6.0/Informedica.GenForm.Lib.dll"

// These can be loaded all at once.

#load "../Types.fs"
#load "../Utils.fs"
#load "../Logging.fs"
#load "../Exceptions.fs"
#load "../WrappedString.fs"
#load "../ValueUnit.fs"
#load "../Variable.fs"
#load "../OrderVariable.fs"
#load "../Solver.fs"
#load "../Order.fs"
#load "../OrderLogger.fs"
#load "../DrugOrder.fs"
#load "../Api.fs"


fsi.AddPrinter<System.DateTime> (fun dt -> dt.ToShortDateString())
