import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub417 {
  def sigma(((a, b, f))): Int63 = {
    (a, b) match {
      case (x, y) => {
        
          if (
            x == y
          ) {
            f(x) 
          } else if (
            x > y
          ) {
            0 
          } else {
            f(x) + sigma(x + 1, y, f)
          }
      }
    }
  }
  
  /*let a21 = sigma (0, 3, function x -> x)
  let a22 = sigma (3, 3, function x -> x)
  let a23 = sigma (7, 10, function x -> x*x)
  let a24 = sigma (11, 10, function x -> x*x)
  
  let _ = print_int(a21)
  let _ = print_endline("")
  let _ = print_int(a22)
  let _ = print_endline("")
  let _ = print_int(a23)
  let _ = print_endline("")
  let _ = print_int(a24)
  let _ = print_endline("")*/
}