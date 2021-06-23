import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub115 {
  
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      { if (n eq 0) ( (x) => { x } ) else ( (x) => { iter(n - 1, f, f(x)) } )
    }
  }
  
  /*
  let _ = print_endline(string_of_int(iter(3, function x->2+x) 0))
  let _ = print_endline(string_of_int(iter(5, function x->2+x) 0))
  
  let _ = print_endline (string_of_float(iter(3, function x->x*.x) 1.2))
  */
}