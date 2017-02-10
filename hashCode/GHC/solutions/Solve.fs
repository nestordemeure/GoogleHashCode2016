module GHC.Solve

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

//-------------------------------------------------------------------------------------------------



//-------------------------------------------------------------------------------------------------
// SOLUTION

/// solution
let solution droneNumber deadLine maxLoad productWeights warehouses orders = 
   let drones = droneCreates droneNumber maxLoad
   let orders = orders |> Array.sortBy (fun o -> List.Length o.products)
   let mutable result = []
   /// chaque ordre va réserver chaque produit dans la warehouse la plus proche
   for order in orders do
   /// chaque ordre, pour chaque warehouse, apelle les dronnes nécéssaire pour etre complet

