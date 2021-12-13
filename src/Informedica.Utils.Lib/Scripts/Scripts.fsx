//#I __SOURCE_DIRECTORY__

#load "load.fsx"

open System
open MathNet.Numerics
open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL

BigRational.one

// ToDo: need to fix this
module Double =

    let getPrecision n f =
        try
            let n = if n < 0 then 0 else n
            if f = 0. || n = 0 then n
            else
                let s = (f |> abs |> string)
                // chech whether the string is of type "1E-05"
                if(s.Contains "E") then
                    let k = s.IndexOf("E")+2 // get the index of the number after the "-"
                    let h = s.[k..] |> int // get int 5
                    let p = h + n - 1  // precision is 5 + n -1
                    p
                else
                let s = s.Split([|'.'|])
                // calculate number of remaining decimal digits (after '.')
                let p = n - (if s.[0] = "0" then 0 else s.[0].Length)
                let p = if p < 0 then 0 else p
                printfn $"parse int: {s.[0]}"
                if (int s.[0]) > 0 then // s.[0] |> int64 > 0L if (*)
                    p
                else
                    // calculate the the first occurance of a non-zero decimal digit
                    let c = (s.[1] |> String.countFirstChar '0')
                    c + p
        with
        | e ->
            printfn "cannot get precision %i for %f" n f 
            printfn "catching error %A" e
            printfn "returning 1 as default value"
            1



Double.getPrecision 1 0.00001
Double.getPrecision 2 0.00000000666
Double.getPrecision 2 0.0101 //gives precision 3, but should it be 4?

Double.getPrecision 1 6666666666.666 // (*) can only work with int64 types 


BigRational.calcConc 50I 2N