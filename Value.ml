(* hw14_value.ml *)
type addr = int

type value =
  | VInt of int
  | VBool of bool
  | VAddr of addr
  | VTuple of value * value
  | VArray of addr list

let rec string_of_value = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VAddr a -> "&" ^ string_of_int a
  | VTuple (v1, v2) -> Printf.sprintf "(%s, %s)" (string_of_value v1) (string_of_value v2)
  | VArray addrs ->
    let inside = String.concat ", " (List.map string_of_int addrs) in
    Printf.sprintf "[|%s|]" inside
