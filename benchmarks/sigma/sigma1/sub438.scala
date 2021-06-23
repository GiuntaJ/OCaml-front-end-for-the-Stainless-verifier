import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub438 {
  def sigma(((x, y, f))): Int63 = {
    val _2 = {
      def sigma_0(acc, a, b, f) = {
        if (a > b) acc else f(a) + sigma_0(acc, a + 1, b, f)
      }
      sigma_0(0, x, y, f)
    }
  }
      
}