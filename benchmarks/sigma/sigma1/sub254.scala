import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub254 {
  /* not tested */
  
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))) = {
    val _2 = {
      def sigma_sub(((cur, n, f))) = {
        if (cur eq n) f(n) else f(cur) + sigma_sub(cur + 1, n, f)
      }
      if (a > b) 0 else sigma_sub(a, b, f)
    }
  }
}