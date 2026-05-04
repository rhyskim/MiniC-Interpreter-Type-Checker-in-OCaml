open Ast
open Value
open AddrManager

module StringMap = Map.Make(String)

(* 환경: 변수 이름 -> 주소 *)
type env = addr StringMap.t
let empty_env = StringMap.empty

let extend_env x a env = StringMap.add x a env
let lookup_env x env = StringMap.find x env

(* 표현식 해석 *)
let rec interp_expr (e : expr) ((env, mem) : env * Mem.t) : value =
  match e with
  | Num n -> VInt n
  | Bool b -> VBool b
  | Id x -> Mem.load (lookup_env x env) mem
  | Add (e1, e2) ->
    (match interp_expr e1 (env, mem), interp_expr e2 (env, mem) with
     | VInt n1, VInt n2 -> VInt (n1 + n2)
     | _ -> failwith (Format.asprintf "[Error] Not a number: %a" pp_expr e))
  | Sub (e1, e2) ->
    (match interp_expr e1 (env, mem), interp_expr e2 (env, mem) with
     | VInt n1, VInt n2 -> VInt (n1 - n2)
     | _ -> failwith (Format.asprintf "[Error] Not a number: %a" pp_expr e))
  | Lt (e1, e2) ->
    (match interp_expr e1 (env, mem), interp_expr e2 (env, mem) with
     | VInt n1, VInt n2 -> VBool (n1 < n2)
     | _ -> failwith (Format.asprintf "[Error] Not a number: %a" pp_expr e))
  | Gt (e1, e2) ->
    (match interp_expr e1 (env, mem), interp_expr e2 (env, mem) with
     | VInt n1, VInt n2 -> VBool (n1 > n2)
     | _ -> failwith (Format.asprintf "[Error] Not a number: %a" pp_expr e))
  | Eq (e1, e2) -> VBool (interp_expr e1 (env, mem) = interp_expr e2 (env, mem))
  | And (e1, e2) ->
    (match interp_expr e1 (env, mem), interp_expr e2 (env, mem) with
     | VBool b1, VBool b2 -> VBool (b1 && b2)
     | _ -> failwith (Format.asprintf "[Error] Not a bool: %a" pp_expr e))
  | Or (e1, e2) ->
    (match interp_expr e1 (env, mem), interp_expr e2 (env, mem) with
     | VBool b1, VBool b2 -> VBool (b1 || b2)
     | _ -> failwith (Format.asprintf "[Error] Not a bool: %a" pp_expr e))
  | Tuple (e1, e2) -> VTuple (interp_expr e1 (env, mem), interp_expr e2 (env, mem))
  | First e ->
    (match interp_expr e (env, mem) with
     | VTuple (v1, _) -> v1
     | _ -> failwith (Format.asprintf "[Error] Not a tuple: %a" pp_expr e))
  | Second e ->
    (match interp_expr e (env, mem) with
     | VTuple (_, v2) -> v2
     | _ -> failwith (Format.asprintf "[Error] Not a tuple: %a" pp_expr e))
  | Array (_, e_len, e_init) ->
    (match interp_expr e_len (env, mem), interp_expr e_init (env, mem) with
     | VInt len, v when len >= 0 ->
       let rec alloc_array n acc mem =
         if n = 0 then (List.rev acc, mem)
         else
           let a = new_addr () in
           let mem' = Mem.store a v mem in
           alloc_array (n - 1) (a :: acc) mem'
       in
       let addrs, _mem = alloc_array len [] mem in
        VArray addrs

     | _ -> failwith (Format.asprintf "[Error] Invalid array creation: %a" pp_expr e))
  | Index (e1, e2) ->
    (match interp_expr e1 (env, mem), interp_expr e2 (env, mem) with
     | VArray addrs, VInt idx when idx >= 0 && idx < List.length addrs ->
       Mem.load (List.nth addrs idx) mem
     | _ -> failwith (Format.asprintf "[Error] Invalid indexing: %a" pp_expr e))
  | Ref x -> VAddr (lookup_env x env)
  | Deref e ->
    (match interp_expr e (env, mem) with
     | VAddr a -> Mem.load a mem
     | _ -> failwith (Format.asprintf "[Error] Not an address: %a" pp_expr e))

(* 문장 해석 *)
and interp_stmt (s : stmt) ((env, mem) : env * Mem.t) (fstore : Fstore.t) : env * Mem.t =
  match s with
  | DefStmt (_, x, e) ->
    let v = interp_expr e (env, mem) in
    let a = new_addr () in
    let mem' = Mem.store a v mem in
    (extend_env x a env, mem')
  | DefArrInitStmt (_, x, es) ->
    let vs = List.map (fun e -> interp_expr e (env, mem)) es in
    let addrs, mem' =
      List.fold_right (fun v (acc, m) ->
        let a = new_addr () in
        (a :: acc, Mem.store a v m)) vs ([], mem)
    in
    let arr_addr = new_addr () in
    let mem'' = Mem.store arr_addr (VArray addrs) mem' in
    (extend_env x arr_addr env, mem'')
  | UpdateStmt (x, e_idx, e_val) ->
    (match Mem.load (lookup_env x env) mem with
     | VArray addrs ->
       (match interp_expr e_idx (env, mem), interp_expr e_val (env, mem) with
        | VInt i, v when i >= 0 && i < List.length addrs ->
          let ai = List.nth addrs i in
          let mem' = Mem.store ai v mem in
          (env, mem')
        | _ -> failwith (Format.asprintf "[Error] Invalid array update: %a" pp_stmt s))
     | _ -> failwith (Format.asprintf "[Error] Not an array: %a" pp_stmt s))
  | StoreStmt (e1, e2) ->
    (match interp_expr e1 (env, mem) with
     | VAddr a -> let v = interp_expr e2 (env, mem) in (env, Mem.store a v mem)
     | _ -> failwith (Format.asprintf "[Error] Not an address: %a" pp_stmt s))
  | IfStmt (e, s1, s2) ->
    (match interp_expr e (env, mem) with
     | VBool true -> interp_stmt_list s1 (env, mem) fstore
     | VBool false -> interp_stmt_list s2 (env, mem) fstore
     | _ -> failwith (Format.asprintf "[Error] Not a bool: %a" pp_expr e))
  | LoopStmt (e, body) as loop ->
    (match interp_expr e (env, mem) with
     | VBool true ->
       let env', mem' = interp_stmt_list body (env, mem) fstore in
       interp_stmt loop (env', mem') fstore
     | VBool false -> (env, mem)
     | _ -> failwith (Format.asprintf "[Error] Not a bool: %a" pp_expr e))
  | CallStmt (x, f, args) ->
    let (params, body) = Fstore.find f fstore in
    let vs = List.map (fun e -> interp_expr e (env, mem)) args in
    let env', mem' =
      List.fold_left2 (fun (e, m) p v ->
        let a = new_addr () in
        (extend_env p a e, Mem.store a v m)) (empty_env, mem) params vs
    in
    let env'', mem'' = interp_stmt_list body (env', mem') fstore in
      (match List.rev body with
     | ReturnStmt e :: _ ->
       let v = interp_expr e (env'', mem'') in
       let a = new_addr () in
       let mem''' = Mem.store a v mem'' in
       (extend_env x a env, mem''')
     | _ -> failwith "[Error] Function did not return")
  | InputStmt x ->
    let n = read_int () in
    let a = new_addr () in
    let mem' = Mem.store a (VInt n) mem in
    (extend_env x a env, mem')
  | ReturnStmt _ -> (env, mem)

and interp_stmt_list (stmts : stmt list) ((env, mem) : env * Mem.t) (fstore : Fstore.t) : env * Mem.t =
  List.fold_left (fun (e, m) s -> interp_stmt s (e, m) fstore) (env, mem) stmts

let interp_prog (Program (defs, stmts)) : Mem.t =
  let fstore =
    List.fold_left (fun fs (FunDef (_, name, args, body)) -> Fstore.add name (List.map snd args) body fs)
      Fstore.empty defs
  in
  let _, mem = interp_stmt_list stmts (empty_env, Mem.empty) fstore in
  mem
