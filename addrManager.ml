let i = ref 0

let new_addr () =
  let addr = !i in
  let _ = i := addr + 1 in
  addr

let init () = i := 0

let ret_addr = -1