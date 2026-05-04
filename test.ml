open Hw14_interp
open Ast
open Mem
let show_mem mem =
  mem
  |> IntMap.bindings
  |> List.map (fun (a, v) -> string_of_int a ^ ":" ^ Value.string_of_value v)
  |> String.concat ", "

let () =
  let prog1 =
    Program ([],
      [ DefStmt (TInt, "x", Num 10)
      ; DefStmt (TInt, "y", Add (Id "x", Num 20))
      ])
  in
  let mem1 = interp_prog prog1 in
  print_endline ("[simple arith and vars] " ^ show_mem mem1)

let () =
  let prog2 =
    Program ([],
      [ DefStmt (TBool, "cond", Bool true)
      ; IfStmt (Id "cond",
          [ DefArrInitStmt (TInt, "a", [Num 1]) ],
          [ DefArrInitStmt (TInt, "a", [Num 100]) ])
      ; LoopStmt (Lt (Index (Id "a", Num 0), Num 3),
          [ UpdateStmt ("a", Num 0, Add (Index (Id "a", Num 0), Num 1)) ])
      ])
  in
  let mem2 = interp_prog prog2 in
  print_endline ("[if stmt and loop] " ^ show_mem mem2)

let () =
  let prog3 =
    Program ([],
      [ DefStmt (TTuple (TInt, TBool), "t", Tuple (Num 1, Bool true))
      ; DefArrInitStmt (TInt, "arr", [Num 10; Num 20; Num 30])
      ; UpdateStmt ("arr", Num 1, Num 99)
      ])
  in
  let mem3 = interp_prog prog3 in
  print_endline ("[tuple and array] " ^ show_mem mem3)

let () =
  let prog4 =
    Program (
      [ FunDef (TInt, "add_one", [(TInt, "n")],
          [ ReturnStmt (Add (Id "n", Num 1)) ]) ],
      [ CallStmt ("res", "add_one", [Num 41]) ])
  in
  let mem4 = interp_prog prog4 in
  print_endline ("[function call and return] " ^ show_mem mem4)
