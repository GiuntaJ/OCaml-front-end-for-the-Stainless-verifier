import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub259 {
  val sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, func) =>
      {
        val _2 = {
          def sigma_r(c, d, result): Int63 = {
            if (c <= d) sigma_r(c + 1, d, result + func(c)) else result
          }
          sigma_r(a, b, 0)
        }
    }
  }
}