import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub141 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (func, a, b) =>
      { if (func(a) == func(b)) func(a) else func(a) + sigma(func, a + 1, b)
    }
  }
}