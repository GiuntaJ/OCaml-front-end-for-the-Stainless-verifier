import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub428 {
  
  def sigma(((a, b, f))) = {
    val _2 = {
      def sum(x, s) = { if (x > b) s else sum(x + 1, s + f(x)) }
      sum(a, 0)
    }
  }
}
