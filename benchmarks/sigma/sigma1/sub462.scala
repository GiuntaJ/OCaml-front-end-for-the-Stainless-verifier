import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub462 {
  val sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) =>
      {
        val _2 = {
          def help(((a, b, f, r))) = {
            
              if (
                a == b
              ) {
                f(a) + r 
              } else if (
                a > b
              ) {
                0 
              } else {
                help(a + 1, b, f, r + f(a))
              }
          }
          help(a, b, f, 0)
        }
    }
  }
  /*
  let uni : int -> int = fun x -> x
  
  let _ = print_endline(string_of_int(sigma(9, 10, uni)))
  
  let _ = 
  let print_bool x = 
  print_endline (string_of_bool x) in 
  print_bool (385 = sigma (1, 10, (fun x -> x * x))); 
  print_bool (0 = sigma (3, 1, fun x -> x * x)); 
  print_bool (27 = sigma(3, 3, fun x -> x * x * x)); 
  print_bool (385 = sigma(-10, -1, fun x -> x * x)) 
  */
}