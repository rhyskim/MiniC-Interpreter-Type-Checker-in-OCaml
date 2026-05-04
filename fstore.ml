type t = (string * string list * Ast.stmt list) list


let empty : t = []

    (* let add : string -> string list -> Ast.stmt list -> t -> t *)
    let add (name: string) (args: string list) (stmts: Ast.stmt list) (t: t) : t =
      let add_filter = 
        List.filter (fun (key, _, _) -> key <> name) t in
      (name, args, stmts) :: add_filter

  let rec find (name: string) (t: t) : (string list * Ast.stmt list) =
    match t with
    | [] -> failwith ("[Error] Unbound function: " ^ name)
    | (key, args, stmts) :: tl -> if key = name then (args, stmts) else find name tl