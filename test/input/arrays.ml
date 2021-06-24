let array = [|0; if true then 1 else 2|];;
let array_assignment = array.(0) <- 1;;
let index = 10;;
let array_access = array.(index);;

let x = 'a';;
let match_with_array = match x with 
      'a' -> [|1; 2|]
    | 'b' -> [|0; 2|]
    | _ -> [|0; 2|];;
let match_without_array = match x with 
    'a' -> 0
  | 'b' -> 1
  | _ -> 2;;

let if_then_else_with_array = if 1 < 0 then [|1; 2|] else [|1; 3|];;
let if_then_else_without_array = if 1 < 0 then 1 else 2;;

let tuple_with_array = (1, [|1; 2|]);;
let tuple_without_array = (1, 2);;