import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub168 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      { if (b > a) sigma(f, a + 1, b) + sigma(f, a, a) else f(a)
    }
  }
}