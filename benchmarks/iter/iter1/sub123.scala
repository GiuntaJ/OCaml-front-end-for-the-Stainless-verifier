import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub123 {
  def iter(((n, ft)), a) = { if (n == 0) a else ft(iter(n - 1, ft, a)) }
}