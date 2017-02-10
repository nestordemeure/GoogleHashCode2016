module GHC.Solve

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

//-------------------------------------------------------------------------------------------------
// chaque ordre va réserver chaque produit dans la warehouse la plus proche


// Reserve le produit dans dans la warehouse de la commande donné
let book (order:Order) (product:Product) (warehouse:Warehouse) =
   Array.set warehouse.stock product (warehouse.stock.[product]-1)

   match order.BookedProducts.TryGetValue(warehouse.idW) with
   | (true, e1) -> order.BookedProducts.Add(warehouse.idW, (product::e1))
   | _ -> order.BookedProducts.Add(warehouse.idW, [product])


//-------------------------------------------------------------------------------------------------
// apelle les drones nécéssaire pour etre complet



//-------------------------------------------------------------------------------------------------
// SOLUTION

/// solution
let solution droneNumber deadLine maxLoad productWeights (warehouses:_[]) orders =
   let drones = droneCreates droneNumber maxLoad warehouses.[0].cell
   let orders = orders |> Array.sortBy (fun o -> List.length o.products)
   let mutable result = []
   /// chaque ordre va réserver chaque produit dans la warehouse la plus proche
   for order in orders do

   /// chaque ordre, pour chaque warehouse, apelle les drones nécéssaire pour etre complet
   for order in orders do
      for kv in order.BookedProducts do
         let warehouseId = kv.Key
         let prodList = kv.Value
         // tant qu'on a des items à charger
            // trouver drone le plus proche
            let droneId = findDrone warehousePosition drones
            // remplir drone
            // envoyer le drone
         calldrones warehouse order.
