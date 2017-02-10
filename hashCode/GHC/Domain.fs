module GHC.Domain

open ExtCore.Collections
open System.Collections.Generic

open GHC.Extensions
open GHC.Extensions.Common
open System.Collections.Generic

//-------------------------------------------------------------------------------------------------

type Coord = int * int

type Product = int

type Order = {adress : Coord ; products : Product list; }

type Warehouse = { cell : Coord ; stock : Product array; BookedProducts : Dictionary<Product,Order>}

type Drone = { position : Coord ; content : Product list ; loadLeft : int ; maxLoad : int ; time : int}

let droneCreates droneNumber maxLoad =
   [||]

//-------------------------------------------------------------------------------------------------

let distance (ra,ca) (rb,cb) =
   (ra-rb)*(ra-rb) + (ca-cb)*(ca-cb)
   |> float |> sqrt |> ceil |> int
