import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub262 {
  def iter(((n: Int63, fn: Int63 => Int63)), x: Int63): Int63 = {
    if (n <= 0) fn(x) else fn(iter(n - 1, fn, x))
  }
}
