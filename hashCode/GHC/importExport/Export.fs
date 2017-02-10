module GHC.Export

open ExtCore.Collections
open System.IO

open GHC.Extensions
open GHC.Domain

//-------------------------------------------------------------------------------------------------

/// turns a list of strings into a single string (containing the elements IN order)
let listToString sep (l : string list) =
    match l with 
    | [] -> ""
    | _ -> List.reduce (fun acc s -> acc + sep + s ) l

//-------------------------------------------------------------------------------------------------
// EXPORTATION

let stringOfConsigne c = 
   match c with 
   | Load (droneId,warehouse, prodtype, quantity) ->
      sprintf "%d L %d %d %d" droneId warehouse prodtype quantity
   | Unload (droneId,warehouse, prodtype, quantity) ->
      sprintf "%d U %d %d %d" droneId warehouse prodtype quantity
   | Deliver (droneId,orderId, prodtype, quantity) ->
      sprintf "%d D %d %d %d" droneId orderId prodtype quantity
   | Wait (droneId,turnNumber) -> 
      sprintf "%d W %d" droneId turnNumber

let export path consignes =
   let lines = List.map stringOfConsigne consignes
   File.WriteAllLines(path, lines)
