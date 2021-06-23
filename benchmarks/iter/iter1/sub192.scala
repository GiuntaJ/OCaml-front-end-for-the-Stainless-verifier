import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub192 {
  /* ex 3 */
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      { if (n > 0) ( (x) => { f(iter(n - 1, f, x)) } ) else ( (x) => { x } )
    }
  }
  
  /* ex 3 test */
  /* let _ = print_endline(string_of_int (iter(5, fun x -> 2*x) 10)) */
}