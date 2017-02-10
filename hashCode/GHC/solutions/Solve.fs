module GHC.Solve

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

//-------------------------------------------------------------------------------------------------
// chaque ordre va réserver chaque produit dans la warehouse la plus proche


let book (order:Order) (product:Product) (warehouse:Warehouse) =
   Array.set warehouse.stock product (warehouse.stock.[product]-1)

   match order.BookedProducts.TryGetValue(warehouse.idW) with
   | (true, e1) -> order.BookedProducts.Add(warehouse.idW, (product::e1))
   | _ -> order.BookedProducts.Add(warehouse.idW, [product])

let closestWharehouse (item:Product) (pos:Coord) (wl : Warehouse list) =
    // Get all Warehouses which contains the product
    let whWithProduct = List.filter (fun (w:Warehouse) -> w.stock.[item] > 0) wl
    // Computes the distances between the currend pos to each Warehouses
    let distances = List.map (fun (w:Warehouse) -> distance w.cell pos) whWithProduct
    // We get the min of the distances and get the corresponding Warehouse
    let min = List.min distances
    match (List.tryFindIndex (fun x -> x = min) distances) with
     | Some indice -> whWithProduct.[indice]
     | None -> raise (System.ArgumentException("Cannot find the item in any Warehouse"))


let processOneOrder (o:Order) (wl : Warehouse list) =
    for p in o.products do
        let w = closestWharehouse p o.adress wl
        book o p w


//-------------------------------------------------------------------------------------------------
// apelle les drones nécéssaire pour etre complet

let rec consigneOfCharge droneId warehouseId orderId charge = 
   match charge with 
   | [] -> []
   | (p,qt)::q -> 
      let before = Load (droneId,warehouseId,p,qt)
      let after = Deliver (droneId,orderId,p,qt)
      after :: (consigneOfCharge droneId warehouseId orderId q) @ [before]

// tant qu'on a des items à charger
   // remplir drone
   // envoyer le drone
let rec giveOrders warehouseId orderId adress cell (pWeights:int []) dronesList prodList charge consignes =
   match prodList, dronesList with
   | [],_ | _,[] -> consignes
   | p::pq, drone::dq when drone.loadLeft < pWeights.[p] -> // TODO more efficient if we search for an item small enough to be put in the drone
      // le drone est pleins, on l'envois (implicite) et on en charge un nouveau
      let charge = List.countBy id charge
      drone.loadLeft <- drone.maxLoad
      drone.time <- drone.time + (distance drone.position cell) + (distance cell adress) //temps de trajet
      drone.time <- drone.time + 2*(List.length charge) // temps de chargement/déchargement
      drone.position <- adress
      let newConsignes = (consigneOfCharge drone.idD warehouseId orderId charge) @ consignes // ajouter toute les consignes
      giveOrders warehouseId orderId adress cell pWeights dq prodList [] newConsignes
   | p::pq, drone::dq ->
      // ajouter p au drone actuel
      drone.loadLeft <- drone.loadLeft - pWeights.[p]
      giveOrders warehouseId orderId adress cell pWeights dronesList pq (p::charge) consignes


//-------------------------------------------------------------------------------------------------
// SOLUTION

/// solution
let solution droneNumber deadLine maxLoad (productWeights:_[]) (warehouses:_[]) orders = 
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
         let prodList = List.sortByDescending (fun x -> productWeights.[x]) kv.Value
         result <- giveOrders warehouseId order.idO order.adress warehouse.cell productWeights dronesByDistance prodList [] result

   List.rev result
