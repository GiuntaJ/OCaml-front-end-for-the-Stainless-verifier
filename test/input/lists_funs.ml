let list = ['a'];;
(*let list_length = List.length list;;*)
let list_cons = List.cons 'b' list;;
let list_head = List.hd list;;
let list_tail = List.tl list;;
let list_nth = List.nth list 0;;
let list_rev = List.rev list;;
let list_append = List.append list list;;
let list_concat = List.concat [['a']; ['b']];;
let list_flatten = List.flatten [['a']; ['b']];;
let list_map = List.map (fun x -> 'b') list;;
let list_forall = List.for_all (fun x -> x = 'a') list;;
let list_exists = List.exists (fun x -> x = 'b') list;;
let list_mem = List.mem 'a' list;;
let list_find_opt = List.find_opt (fun x -> x = 'b') list;;
let list_filter = List.filter (fun x -> x = 'b') list;;