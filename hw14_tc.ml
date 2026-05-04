(* hw14_tc.ml *)
open Ast
open GlobalTEnv
open LocalTEnv

let rec tc_expr tenv lenv = function
  | Num _ -> TInt
  | Bool _ -> TBool
  | Id x -> lookup x lenv
  | Add (e1, e2) | Sub (e1, e2) ->
    let t1 = tc_expr tenv lenv e1 in
    let t2 = tc_expr tenv lenv e2 in
    if t1 = TInt && t2 = TInt then TInt
    else failwith (Format.asprintf "[Ill-typed] %a" pp_expr (Add (e1, e2)))
  | Lt (e1, e2) | Gt (e1, e2) ->
    let t1 = tc_expr tenv lenv e1 in
    let t2 = tc_expr tenv lenv e2 in
    if t1 = TInt && t2 = TInt then TBool
    else failwith (Format.asprintf "[Ill-typed] %a" pp_expr (Lt (e1, e2)))
  | Eq (e1, e2) ->
    let t1 = tc_expr tenv lenv e1 in
    let t2 = tc_expr tenv lenv e2 in
    if t1 = t2 then TBool
    else failwith (Format.asprintf "[Ill-typed] %a" pp_expr (Eq (e1, e2)))
  | And (e1, e2) | Or (e1, e2) ->
    let t1 = tc_expr tenv lenv e1 in
    let t2 = tc_expr tenv lenv e2 in
    if t1 = TBool && t2 = TBool then TBool
    else failwith (Format.asprintf "[Ill-typed] %a" pp_expr (And (e1, e2)))
  | Tuple (e1, e2) -> TTuple (tc_expr tenv lenv e1, tc_expr tenv lenv e2)
  | First e ->
    (match tc_expr tenv lenv e with
     | TTuple (t1, _) -> t1
     | _ -> failwith (Format.asprintf "[Ill-typed] %a" pp_expr e))
  | Second e ->
    (match tc_expr tenv lenv e with
     | TTuple (_, t2) -> t2
     | _ -> failwith (Format.asprintf "[Ill-typed] %a" pp_expr e))
  | Array (t, e_len, e_init) ->
    let t_len = tc_expr tenv lenv e_len in
    let t_init = tc_expr tenv lenv e_init in
    if t_len = TInt && t = t_init then TArray t
    else failwith (Format.asprintf "[Ill-typed] %a" pp_expr (Array (t, e_len, e_init)))
  | Index (e_arr, e_idx) ->
    (match tc_expr tenv lenv e_arr, tc_expr tenv lenv e_idx with
     | TArray t, TInt -> t
     | _ -> failwith (Format.asprintf "[Ill-typed] %a" pp_expr (Index (e_arr, e_idx))))
  | _ -> failwith "Unhandled expression pattern in tc_expr"

let rec tc_stmt tenv lenv = function
  | DefStmt (t, x, e) ->
    let t' = tc_expr tenv lenv e in
    if t = t' then extend x t lenv
    else failwith (Format.asprintf "[Ill-typed] %a" pp_stmt (DefStmt (t, x, e)))
  | IfStmt (e, s1, s2) ->
    let t = tc_expr tenv lenv e in
    if t = TBool then let _ = tc_stmt_list tenv lenv s1 in tc_stmt_list tenv lenv s2
    else failwith (Format.asprintf "[Ill-typed] %a" pp_stmt (IfStmt (e, s1, s2)))
  | DefArrInitStmt (t, x, es) ->
    let ts = List.map (tc_expr tenv lenv) es in
    if List.for_all (fun t' -> t = t') ts then extend x (TArray t) lenv
    else failwith (Format.asprintf "[Ill-typed] %a" pp_stmt (DefArrInitStmt (t, x, es)))
  | UpdateStmt (x, e_idx, e_val) ->
    let t_arr = lookup x lenv in
    let t_idx = tc_expr tenv lenv e_idx in
    let t_val = tc_expr tenv lenv e_val in
    (match t_arr with
     | TArray t when t_idx = TInt && t_val = t -> lenv
     | _ -> failwith (Format.asprintf "[Ill-typed] %a" pp_stmt (UpdateStmt (x, e_idx, e_val))))
  | _ -> failwith "Unhandled statement pattern in tc_stmt"

and tc_stmt_list tenv lenv stmts =
  List.fold_left (fun lenv s -> tc_stmt tenv lenv s) lenv stmts

let tc_prog (Program (fdefs, stmts)) =
  let _ = stmts in
  let tenv = List.fold_left (fun tenv (FunDef (rt, f, params, body)) ->
    extend_fun f (FunDef (rt, f, params, body)) tenv) GlobalTEnv.empty fdefs in
  let lenv = tc_stmt_list tenv LocalTEnv.empty in
  (tenv, lenv)
