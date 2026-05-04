 (* Mem.ml *)
open Value

module IntMap = Map.Make(Int)

type t = value IntMap.t

let empty = IntMap.empty

let alloc (addr : int) (v : value) (m : t) : t =
  IntMap.add addr v m

let load (addr : int) (m : t) : value =
  try IntMap.find addr m
  with Not_found -> failwith ("Unbound memory at address: " ^ string_of_int addr)

let store (addr : int) (v : value) (m : t) : t =
  IntMap.add addr v m