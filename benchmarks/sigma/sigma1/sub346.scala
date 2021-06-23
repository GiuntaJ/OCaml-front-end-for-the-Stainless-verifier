import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub346 {
  /*
      PL 1-2
      2008-11609 박성원
  */
  
  def sigma(((a, b, f))) = { if (a > b) 0 else f(a) + sigma(a + 1, b, f) }
}