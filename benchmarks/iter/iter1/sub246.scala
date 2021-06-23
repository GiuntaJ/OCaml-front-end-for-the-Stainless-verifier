import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub246 {
  def iter(((n, f))) = {
    (n, f) match {
      case (0, f) => { ( (x) => { x } ) }
      case (n, f) => { ( (x) => { f(iter(n - 1, f, x)) } ) }
    }
  } 
}