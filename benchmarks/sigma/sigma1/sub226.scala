import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub226 {
  /* KIHWAN KANG HW01-1 */
  
  def sigma(((i, n, f))) = { if (i > n) 0 else f(i) + sigma(i + 1, n, f) }
}