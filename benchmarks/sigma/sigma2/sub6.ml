(* 2014210036 κΉλν*)

let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 1
let rec sigma f a b =
	if a<b then (f a)+(sigma f (a+1) b) else f a
