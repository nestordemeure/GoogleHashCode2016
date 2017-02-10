module GHC.Domain

open ExtCore.Collections
open System.Collections.Generic

open GHC.Extensions
open GHC.Extensions.Common

//-------------------------------------------------------------------------------------------------

type Coord = int * int

type Product = { id : int ; weight : int }

type Warehouse = { cell : Coord ; stock : int array }

type Order = {adress : Coord ; products : int list}

//-------------------------------------------------------------------------------------------------

let distance (ra,ca) (rb,cb) = 
   (ra-rb)*(ra-rb) + (ca-cb)*(ca-cb)
   |> float |> sqrt |> ceil |> int

