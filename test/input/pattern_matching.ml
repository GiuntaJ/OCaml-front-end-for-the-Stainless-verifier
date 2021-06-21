type case_class = 
  | A 
  | B of int 
  | C of char * int;;
let rec match_with_case_classes e = 
  match e with
  | A -> 0
  | B (e0) -> e0
  | C (e0, e1) -> e1;;
let rec match_with_lists e = 
  match e with
    [] -> 1
  | hd::tl when hd > 10 -> match_with_lists tl
  | _ -> 0;;
let match_with_options e = match e with
    Some x -> true 
  | None -> false;;