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
// MAIN

[<EntryPoint>]
let main argv =
   // import
   //let inPath = "../inputs/busy_day.in"
   //let inPath = "../inputs/mother_of_all_warehouses.in"
   let inPath = "../inputs/redundancy.in"
   let (rowNumber,colNumber,droneNumber,deadLine,maxLoad, productWeights, warehouses, orders) = 
      import inPath
   // solution
   let sol = solution droneNumber deadLine maxLoad productWeights warehouses orders
   // evaluation
   printfn "score : %d" score
   score <- 0
   //export 
   export "../output.txt" sol
   0 // return an integer exit code

// add timeout

