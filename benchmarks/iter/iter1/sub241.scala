import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub241 {
  
  def iter(((n, f)), x) = {
    val _2 = {
      def iter_f(y, n) = {
        n match {
          case 0 => { y }
          case _ => { iter_f(f(y), n - 1) }
        }
      }
      iter_f(x, n)
    }
  }
}
