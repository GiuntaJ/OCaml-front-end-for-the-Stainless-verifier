import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub219 {
  def iter(((n: Int63, f)), id) = { if (n == 0) id else f(iter(n - 1, f, id)) }
}