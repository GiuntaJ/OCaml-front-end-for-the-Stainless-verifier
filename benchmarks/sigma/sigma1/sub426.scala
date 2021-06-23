import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub426 {
  /* HW1-Exercise 2*/
  def sigma(((a, b, f))) = {
    val _2 = {
      def sum(i, total) = { if (i > b) total else sum(i + 1, total + f(i)) }
      sum(a, 0)
    }
  }
}