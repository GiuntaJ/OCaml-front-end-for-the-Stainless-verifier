let one_generic_type x = x;;
let multiple_generic_types x y = (x, y);;
let apply_generic_types (x : 'a list) (y : 'b list list) = x;;
let tuple_generic_types (x : ('a * 'b) list) (y : (('c * 'd) * 'e * 'f)) = x;;
(* TODO check why not return type added *)
let function_generic_types (x : 'a -> 'a list -> 'b list) = x;;
(* TODO check why not return type added *)