import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub407 {
  def sigma(((a, b, f))): Int63 = {
    if (a == b) f(a) else sigma(a + 1, b, f) + f(a)
  }
}