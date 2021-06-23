import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub211 {
  def iter(((n, f)), value) = { if (n == 0) value else iter(n - 1, f, f(value))
  }
}