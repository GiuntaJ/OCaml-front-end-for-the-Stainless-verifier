import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub139 {
  def iter(((n, f))) = {
    val _2 = {
      def comp(f, g, x) = { f(g(x)) }
      if (n <= 0) ( (x) => { x } ) else comp(f, iter(n - 1, f))
    }
  }
}