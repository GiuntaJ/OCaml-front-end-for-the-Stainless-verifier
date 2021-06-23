import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub238 {
  def sigma(((a, b, func))) = {
    if (a - b > 0) 0 else func(a) + sigma(a + 1, b, func)
  }
}