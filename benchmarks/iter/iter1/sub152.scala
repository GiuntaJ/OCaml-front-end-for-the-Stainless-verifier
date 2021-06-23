import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub152 {
  def iter(((n, f))) = {
    n match {
      case 0 => { ( (x) => { x } ) }
      case _ => { ( (x) => { f(iter(n - 1, f, x)) } ) }
    }
  }
}