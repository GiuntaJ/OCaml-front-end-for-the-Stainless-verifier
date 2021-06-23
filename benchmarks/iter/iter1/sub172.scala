import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub172 {
  def iter(((n: Int63, f: Int63 => Int63)), a: Int63): Int63 = {
    if (n == 0) a else f(iter(n - 1, f, a))
  }
}