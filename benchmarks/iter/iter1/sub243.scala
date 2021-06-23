import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub243 {
  def iter(((n, f))) = {
    val _2 = {
      def compose(test1, test2, x) = { test1(test2(x)) }
      if (n <= 0) ( (x) => { x } ) else compose(iter(n - 1, f), f)
    }
  }
}