import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub271 {
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))): Int63 = {
    val _2 = {
      def sigmaInternal(n, result) = {
        if (n > b) result else sigmaInternal(n + 1, result + f(n))
      }
      sigmaInternal(a, 0)
    }
  }
}