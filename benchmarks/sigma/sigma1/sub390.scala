import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub390 {
  def sigma(((a, b, f))) = {
    val _2 = {
      def sigma2(((a, b, f, sum))) = {
        if (a > b) sum else sigma2(a + 1, b, f, sum + f(a))
      }
      sigma2(a, b, f, 0)
    }
  }
}