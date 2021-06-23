(* 2006-11377 hw1-2 *)

type expr = 
  NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

type formula = 
  TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr

let rec eval form = 
	match form with
	| TRUE -> true
	| FALSE -> false
	| NOT f -> not (eval f)
	| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
	| ORELSE (f1, f2) -> (eval f1) || (eval f2)
	| IMPLY (f1, f2) -> (not (eval f1)) || (eval f2)
	| LESS (e1, e2) -> 
		let rec cal exp = 
			match exp with
			| NUM n -> n
			| PLUS (e1, e2) -> (cal e1) + (cal e2)
			| MINUS (e1, e2) -> (cal e1) - (cal e2)
		in
		if (cal e1) < (cal e2) then true
		else false