import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub435 {
  def sigma(((a, b, f))) = {
    val _2 = {
      def sigma2(a, b, f, r) = { if (a > b) r else sigma2(a + 1, b, f, r + f(a))
      }
      sigma2(a, b, f, 0)
    }
  }
}