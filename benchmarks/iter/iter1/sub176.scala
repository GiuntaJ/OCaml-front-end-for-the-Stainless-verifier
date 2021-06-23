import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub176 {
  def iter(((n, f)), x) = {
    n match {
      case 0 => { x }
      case _n => { f(iter(_n - 1, f, x)) }
    }
  }
}