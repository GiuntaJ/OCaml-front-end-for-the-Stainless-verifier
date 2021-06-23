import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub129 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        val _2 = {
          def sum(i, z) = { if (i == b + 1) z else sum(i + 1, z + f(i)) }
          sum(a, 0)
        }
    }
  }
}
