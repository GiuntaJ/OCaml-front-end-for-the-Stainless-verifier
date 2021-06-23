import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub108 {
  def iter(((n, f)), k) = {
    n match {
      case 0 => { k }
      case 1 => { f(k) }
      case _ => { iter(n - 1, f, f(k)) }
    }
  }
}