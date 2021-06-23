import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub178 {
  def iter(((n, f)), x) = {
    val _2 = {
      def aux(((n, result))) = {
        n match {
          case 0 => { result }
          case _n => { aux(_n - 1, f(result)) }
        }
      }
      aux(n, x)
    }
  }
}