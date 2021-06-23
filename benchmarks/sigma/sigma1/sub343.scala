import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub343 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = (
    (t) =>
      {
        val _4 = {
          val ((a, b, f)) = t
          if (a > b) 0 else f(a) + sigma(a + 1, b, f)
        }
    }
  )
}