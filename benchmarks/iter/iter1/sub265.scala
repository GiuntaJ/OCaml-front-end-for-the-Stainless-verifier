import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub265 {
  def iter(((n, f)), i) = {
    n match {
      case 0 => { i }
      case _ => { iter(n - 1, f, f(i)) }
    }
  }
}
