import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub397 {
  def sigma(((a, b, f))) = { if (a <= b) f(b) + sigma(a, b - 1, f) else 0 }
}