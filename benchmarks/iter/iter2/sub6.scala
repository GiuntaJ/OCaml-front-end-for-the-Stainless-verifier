import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub6 {
  /* 3 */
  def iter(((n, f)), x): Int63 = { if (n == 0) x else f(iter(n - 1, f, x)) }
}