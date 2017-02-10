module GHC.Import

open System.IO
open ExtCore.Collections
open ExtCore.IO

open GHC.Extensions.Common
open GHC.Extensions.Scanf
open GHC.Extensions
open GHC.Domain

//-------------------------------------------------------------------------------------------------



//-------------------------------------------------------------------------------------------------
// IMPORT

let import path =
   let text = File.ReadAllLines(path)
   let (rowNumber,colNumber,droneNumber,deadLine,maxLoad) = sscanf "%d %d %d %d %d" text.[0]
   let productWeights = text.[2] |> String.split [|' '|] |> Array.map int
   let warehouseNumber = text.[3] |> int
   let warehouses = 
      [|
         for w in [4 .. 2 ..(4 + 2*warehouseNumber - 1)] do
            let (r,c) = sscanf "%d %d" text.[w]
            let stock = text.[w+1] |> String.split [|' '|] |> Array.map int
            yield {cell = (r,c) ; stock = stock}
      |]
   let orderNumber = text.[4 + 2*warehouseNumber] |> int
   let orders =
      [|
         for o in [4 + 2*warehouseNumber + 1 .. 3 .. (4 + 2*warehouseNumber + 1) + 3*orderNumber - 1] do
            let (r,c) = sscanf "%d %d" text.[o]
            let order = text.[o+2] |> String.split [|' '|] |> Array.map int |> Array.toList
            yield {adress = (r,c) ; products = order}
      |]
   rowNumber,colNumber,droneNumber,deadLine,maxLoad, productWeights, warehouses, orders