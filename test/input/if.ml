let if_then_one_line = if 1 > 0 then ();;
let if_then_else_one_line = if 1 < 0 then 1 else 0;;
let if_then_else_multi_lines = if false then 
    print_endline "then clause that is too long for one line"
    else print_endline "else clause";;
let if_else_if = if false then if true then 9 else 10 else if false then 11 else if false then 12 else 13;;
