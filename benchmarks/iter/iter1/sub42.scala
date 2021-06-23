import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub42 {
  def iter(((n, f)), a) = {
    n match {
      case 0 => { a }
      case _ => { iter(n - 1, f, f(a)) }
    }
  }
}