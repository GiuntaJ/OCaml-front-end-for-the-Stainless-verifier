import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub368 {
  def sigma_0(((a, b, f))) = { if (b < a) 0 else f(a) + sigma_0(a + 1, b, f) }
  def sigma(((a, b, f))) = { sigma_0(a, b, f) }
}