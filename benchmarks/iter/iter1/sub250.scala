import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub250 {
  def iter(((n, f))) = {
    val _2 = {
      def sub_iter(n) = {
        if (n == 0) ( (x) => { x } ) else ( (x) => { f(sub_iter(n - 1, x)) } )
      }
      ( (x) => { sub_iter(n, x) } )
    }
  }
}