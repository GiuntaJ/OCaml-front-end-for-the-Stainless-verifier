import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub52 {
  /* complete */
  sealed case class Invalid_Input() extends Exception {}
  
  def sigma(((a, b, f))) = {
    val _2 = {
      def iter(n) = { if (n == b) f(n) else f(n) + iter(n + 1) }
      if (a > b) assert(false, "Invalid_Input") else iter(a)
    }
  }
}