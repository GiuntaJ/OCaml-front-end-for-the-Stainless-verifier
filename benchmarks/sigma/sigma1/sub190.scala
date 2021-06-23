import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub190 {
  def sigma(((n1: Int63, n2: Int63, f: Int63 => Int63))) = {
    val _2 = {
      def sigma_sub(((cur, n, f))) = {
        if (cur eq n) f(n) else f(cur) + sigma_sub(cur + 1, n, f)
      }
      if (n1 > n2) 0 else sigma_sub(n1, n2, f)
    }
  }
}