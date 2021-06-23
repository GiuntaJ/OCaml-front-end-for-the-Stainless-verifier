import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub72 {
  def iter(((n, f)), k) = {
    if (n == 0) ( (x) => { x } )(k) else f(iter(n - 1, f, k))
  }
}