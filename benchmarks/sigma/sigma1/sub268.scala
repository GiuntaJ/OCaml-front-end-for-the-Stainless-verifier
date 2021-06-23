import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub268 {
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))): Int63 = {
    val _2 = {
      def tsigma(res, n) = { if (n > b) res else tsigma(res + f(n), n + 1) }
      tsigma(0, a)
    }
  }	/* will return 0 if a > b */
}