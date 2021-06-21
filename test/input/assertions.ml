let assert1 = assert(if 0 < 1 then true else false);;
let assert2 = assert(if 0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 = 10 then false else true);;
let assert_in_fun n = 
  assert(n > 10);