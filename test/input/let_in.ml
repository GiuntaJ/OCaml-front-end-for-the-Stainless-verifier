let x_in = 10 and y_in = 11 in y_in + x_in;;
let x_in = 10 in 
    let x_in_in = 11 in
    x_in_in + x_in;;
let fun_x_in (e : int) = e + 10 in 
    let fun_x_in_in (e : int) = e + 11 in
    fun_x_in 10 + fun_x_in_in 11 ;;