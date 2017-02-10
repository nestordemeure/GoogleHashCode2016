module GHC.Domain

open ExtCore.Collections
open System.Collections.Generic

open GHC.Extensions
open GHC.Extensions.Common
open System.Collections.Generic

//-------------------------------------------------------------------------------------------------

type Coord = int * int

type Product = int

type Order = {adress : Coord ; products : Product list ; BookedProducts : Dictionary<int (*warehouse id*),Product list> }

type Warehouse = { cell : Coord ; stock : Product array}

type Drone = { position : Coord ; content : Product list ; loadLeft : int ; maxLoad : int ; time : int}

let droneCreates droneNumber maxLoad warehouseDefault =
   Array.create droneNumber { position = warehouseDefault ; content = [] ; loadLeft = maxLoad; maxLoad = maxLoad; time=0}

//-------------------------------------------------------------------------------------------------

let distance (ra,ca) (rb,cb) =
   (ra-rb)*(ra-rb) + (ca-cb)*(ca-cb)
   |> float |> sqrt |> ceil |> int

type Consigne =
   | Load of int*int*int // warehouse * prodtype * quantity
   | Unload of int*int*int // warehouse * prodtype * quantity
   | Deliver of int*int*int // orderId * prodtype * quantity
   | Wait of int // turnNum
