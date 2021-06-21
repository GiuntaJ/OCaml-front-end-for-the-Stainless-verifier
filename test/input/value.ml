let value_non_rec = 'a';;
let rec value_rec = 'b';;
let fun_non_rec x y = x :: y;;
let rec fun_rec n = 
  if n <= 1 then 1
  else fun_rec (n - 1);;
let first = 'a' and second = "b" and third = 3;;

let tes : int = 0 in tes;;