let function_def = function
    | h::t when h > 10 -> t
    | [] -> []
    | _ -> [];;
let fun_def = fun x -> if x > 10 then false else true;;
let fun_def2 = fun x y -> if x > 10 && y then false else true;;