import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub348 {
  /*컴공 2014-10618 이세영 1-2*/
  def sigma(((a, b, f))) = { if (b < a) 0 else sigma(a + 1, b, f) + f(a) }
}