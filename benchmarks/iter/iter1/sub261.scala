import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub261 {
  def iter(((n, f)), i) = {
    if (n <= 0) ( (x) => { x } )(i) else f(iter(n - 1, f, i))
  }
}