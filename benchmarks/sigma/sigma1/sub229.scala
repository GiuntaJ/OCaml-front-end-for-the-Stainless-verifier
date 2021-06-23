import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub229 {
  val sum: Int63 = 0
  def sigma(((a, b, f))) = { if (a > b) sum else sum + f(a) + sigma(a + 1, b, f)
  }
}
