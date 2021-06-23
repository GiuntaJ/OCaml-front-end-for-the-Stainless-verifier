import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub125 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        val _2 = {
          def sigma2(f, n, s) = { if (n <= b) sigma2(f, n + 1, s + f(n)) else s
          }
          sigma2(f, a, 0)
        }
    }
  }
}