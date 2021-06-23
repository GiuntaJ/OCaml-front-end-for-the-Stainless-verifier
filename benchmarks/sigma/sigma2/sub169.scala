import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub169 {
  def abs(n: Int63): Int63 = { if (n < 0) -(n) else n }
  
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        val _2 = {
          val n = abs(a)
          if (n <= b) f(n) + sigma(f, n + 1, b) else 0
        }
    }
  }
}