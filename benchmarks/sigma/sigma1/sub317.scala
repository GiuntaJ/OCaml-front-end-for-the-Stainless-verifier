import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub317 {
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        val _0 = {
          val aa = a + 1
          f(a) + sigma(aa, b, f)
        }
      }
  }
}