import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub20 {
  /*2006-11720 Kim Eunsol*/
  def sigma(((a, b, f))) = { if (a < b) f(a) + sigma(a + 1, b, f) else f(b) }
}