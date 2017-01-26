module GHC.Main

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain
open GHC.Import
open GHC.Solve
open GHC.Export
open System.Collections.Generic

//-------------------------------------------------------------------------------------------------
// EVALUATION

let mutable score = 0

let evaluation solution = ()

//-------------------------------------------------------------------------------------------------
// MAIN

[<EntryPoint>]
let main argv =
(*
    //printfn "%A" argv
    // import
    let inPath = "../input.in"
    let r = import inPath
    // solution

    // evaluation
    evaluation r
    printfn "score : %d" score
    //export 
    export "../output.txt" [||]
*)
    let hs = HashSet()
    hs.Add(1) |> ignore
    hs |> seq |> printfn "%A"

    0 // return an integer exit code
