import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub339 {
  def sigma(((a, b, f))) = { if (a eq b) f(b) else sigma(a + 1, b, f) + f(a) }
}