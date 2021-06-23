import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub162 {
  def iter(((n, f)), v) = { if (n <= 0) v else iter(n - 1, f, f(v)) }
}