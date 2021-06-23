import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub213 {
  def sigma(((a, b, f))) = { if (b - a < 0) 0 else f(a) + sigma(a + 1, b, f) }
}