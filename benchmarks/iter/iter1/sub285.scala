import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub285 {
  def iter(((n, f)), x) = {
    
      if (
        n eq 0
      ) {
        x 
      } else if (
        n eq 1
      ) {
        f(x) 
      } else {
        iter(n - 1, f, f(x))
      }
  }
      
  /*
  let a = iter(3, fun x -> not x) true
  let b = iter(5, fun x -> 2 * x) 1
  let c = iter(0, fun x -> 2*x) 8
  let d = iter(3, fun x -> x+1) 10
  let e = iter(3, fun x -> x+.1.0) 10.0
  let _ = print_endline (string_of_bool a)
  let _ = print_endline (string_of_int b)
  let _ = print_endline (string_of_int c)
  let _ = print_endline (string_of_int d)
  let _ = print_endline (string_of_float e)
  */
}