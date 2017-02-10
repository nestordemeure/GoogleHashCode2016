module GHC.Solve

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

//-------------------------------------------------------------------------------------------------
// chaque ordre va réserver chaque produit dans la warehouse la plus proche



//-------------------------------------------------------------------------------------------------
// apelle les dronnes nécéssaire pour etre complet



//-------------------------------------------------------------------------------------------------
// SOLUTION

/// solution
let solution droneNumber deadLine maxLoad productWeights (warehouses:_[]) orders = 
   let drones = droneCreates droneNumber maxLoad warehouses.[0].cell
   let orders = orders |> Array.sortBy (fun o -> List.length o.products)
   let mutable result = []
   /// chaque ordre va réserver chaque produit dans la warehouse la plus proche
   for order in orders do

   /// chaque ordre, pour chaque warehouse, apelle les dronnes nécéssaire pour etre complet
   for o = 0 to orders.Length - 1 do 
      let order = orders.[o]
      for kv in order.BookedProducts do
         let warehouse = kv.Key
         let prodList = kv.Value
         calldrones warehouse order.

