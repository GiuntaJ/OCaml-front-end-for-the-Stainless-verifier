import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub165 {
  def iter(((n, f: Int63 => Int63)), x) = {
    if (n == 0) x else iter(n - 1, f, f(x))
  }
}