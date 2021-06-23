import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub193 {
  def sigma(((a, b, f))) = {
    val _2 = {
      def aux(((cur, acc))) = { if (cur > b) acc else aux(cur + 1, acc + f(cur))
      }
      aux(a, 0)
    }
  }
}