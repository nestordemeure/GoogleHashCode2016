module GHC.Solve

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

//-------------------------------------------------------------------------------------------------
// chaque ordre va réserver chaque produit dans la warehouse la plus proche



//-------------------------------------------------------------------------------------------------
// apelle les drones nécéssaire pour etre complet

let rec consigneOfCharge warehouseId orderId charge = 
   match charge with 
   | [] -> []
   | (p,qt)::q -> 
      let before = Load (warehouseId,p,qt)
      let after = Deliver (orderId,p,qt)
      after :: (consigneOfCharge warehouseId orderId q) @ [before]


// tant qu'on a des items à charger
   // remplir drone 
   // envoyer le drone
let rec giveOrders warehouseId orderId adress cell (pWeights:int []) dronesList prodList charge consignes = 
   match prodList, dronesList with 
   | [],_ | _,[] -> consignes
   | p::pq, drone::dq when drone.loadLeft < pWeights.[p] -> 
      // le drone est pleins, on l'envois (implicite) et on en charge un nouveau
      let charge = List.countBy id charge
      drone.loadLeft <- drone.maxLoad
      drone.time <- drone.time + (distance drone.position cell) + (distance cell adress) //temps de trajet
      drone.time <- drone.time + 2*(List.length charge) // temps de chargement/déchargement
      drone.position <- adress
      let newConsignes = (consigneOfCharge warehouseId orderId charge) @ consignes // ajouter toute les consignes
      giveOrders warehouseId orderId adress cell pWeights dq prodList [] newConsignes
   | p::pq, drone::dq ->
      // ajouter p au drone actuel
      drone.loadLeft <- drone.loadLeft - pWeights.[p]
      giveOrders warehouseId orderId adress cell pWeights dronesList pq (p::charge) consignes


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
         let warehouse = warehouses.[warehouseId]
         // would be more efficent to consume drone as it goes
         let dronesByDistance = findDrones warehouse.cell drones |> Array.toList
         let prodList = kv.Value // TODO sort from big to small
         result <- giveOrders warehouseId order.idO order.adress warehouse.cell productWeights dronesByDistance prodList [] result

   List.rev result

