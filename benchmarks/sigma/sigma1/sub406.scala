import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub406 {
  def sigma(((x, y, f))) = { if (x > y) 0 else f(y) + sigma(x, y - 1, f) }
}