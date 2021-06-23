import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub201 {
  def iter(((n, fn)), x) = { if (n > 1) iter(n - 1, fn, fn(x)) else fn(x) }
}