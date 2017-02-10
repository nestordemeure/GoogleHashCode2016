module GHC.Solve

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

//-------------------------------------------------------------------------------------------------
// chaque ordre va réserver chaque produit dans la warehouse la plus proche



//-------------------------------------------------------------------------------------------------
// apelle les drones nécéssaire pour etre complet

// tant qu'on a des items à charger
   // remplir drone 
   // envoyer le drone
let rec giveOrders adress cell (pWeights:int []) dronesList prodList consignes = 
   match prodList, dronesList with 
   | [],_ | _,[] -> consignes
   | p::pq, drone::dq when drone.loadLeft < pWeights.[p] -> 
      // le drone est pleins, on l'envois (implicite) et on en charge un nouveau
      drone.position <- adress
      drone.loadLeft <- drone.maxLoad
      drone.time <- drone.time + (distance drone.position cell) + (distance cell adress) + 1
      giveOrders pWeights dq prodList consignes
   | p::pq, drone::dq ->
      drone.time <- drone.time + 1
      // ajouter p au drone actuel


//-------------------------------------------------------------------------------------------------
// SOLUTION

/// solution
let solution droneNumber deadLine maxLoad productWeights (warehouses:_[]) orders = 
   let drones = droneCreates droneNumber maxLoad warehouses.[0].cell
   let orders = orders |> Array.sortBy (fun o -> List.length o.products)
   let mutable result = []
   /// chaque ordre va réserver chaque produit dans la warehouse la plus proche
   for order in orders do ()

   /// chaque ordre, pour chaque warehouse, apelle les drones nécéssaire pour etre complet
   for order in orders do 
      for kv in order.BookedProducts do
         let warehouseId = kv.Key
         let dronesByDistance = findDrones warehousePosition drones |> Array.toList
         let prodList = kv.Value
         result <- giveOrders order.adress warehouses.[warehouseId].cell productWeights dronesByDistance prodList result

   result

