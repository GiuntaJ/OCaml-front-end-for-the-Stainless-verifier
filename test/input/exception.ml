exception Failure_simple;;
exception Failure_string of string;;
exception Failure_complex of int * string;;

let simple_failure a = if a then raise (Failure_simple);;
let string_failure b = if b then raise (Failure_string "abc");;
let complex_failure c = if c then raise (Failure_complex (1, "abc"));;
