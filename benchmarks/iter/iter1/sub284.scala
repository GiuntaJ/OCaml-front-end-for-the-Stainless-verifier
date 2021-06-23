import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub284 {
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      { if (n == 0) ( (x) => { x } ) else ( (x) => { iter(n - 1, f, f(x)) } )
    }
  }
  /*
  let _ = print_int ((iter(10, fun x -> x+2) 0))
  let _ = print_newline()
  */
}