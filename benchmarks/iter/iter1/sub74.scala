import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub74 {
  
  def iter(((n, f))) = {
    if (n == 0) ( (x) => { x } ) else ( (x) => { f(iter(n - 1, f, x)) } )
  }
  
    /*
  let _ = print_int (iter (10, fun x -> 2*x) 1)
  */
}