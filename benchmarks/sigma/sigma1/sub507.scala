import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub507 {
  val sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) =>
      {
        val _2 = {
          def _sigma(_a: Int63, _b: Int63, r: Int63): Int63 = {
            if (_a > b) r else _sigma(_a + 1, _b, r + f(_a))
          }
          _sigma(a, b, 0)
        }
    }
  }
}