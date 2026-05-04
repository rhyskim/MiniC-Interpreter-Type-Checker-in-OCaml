(* hw14_globalTEnv.ml *)
open Ast

module StringMap = Map.Make(String)

type t = def StringMap.t

let empty = StringMap.empty

let extend_fun f def env = StringMap.add f def env

let lookup_fun f env =
  try StringMap.find f env
  with Not_found -> failwith ("Unbound function: " ^ f)
