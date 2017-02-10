module GHC.Domain

open ExtCore.Collections
open System.Collections.Generic

open GHC.Extensions
open GHC.Extensions.Common

//-------------------------------------------------------------------------------------------------

type Coord = int * int

type Product = int

type Warehouse = { cell : Coord ; stock : Product array }

type Order = {adress : Coord ; products : Product list}

type Drone = { position : Coord ; content : Product list ; loadLeft : int }

let droneCreates droneNumber maxLoad =
   // TODO

//-------------------------------------------------------------------------------------------------

let distance (ra,ca) (rb,cb) = 
   (ra-rb)*(ra-rb) + (ca-cb)*(ca-cb)
   |> float |> sqrt |> ceil |> int

