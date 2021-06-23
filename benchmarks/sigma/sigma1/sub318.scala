import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub318 {
  def sigma(((a, b, funx))) = {
    if (a > b) 0 else funx(a) + sigma(a + 1, b, funx)
  }
}