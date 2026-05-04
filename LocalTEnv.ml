(* LocalTEnv.ml *)
open Ast

module StringMap = Map.Make(String)

type t = typ StringMap.t

let empty : t = StringMap.empty

let extend (x : string) (t : typ) (env : t) : t =
  StringMap.add x t env

let lookup (x : string) (env : t) : typ =
  try StringMap.find x env
  with Not_found -> failwith ("Unbound local variable: " ^ x)
