import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub155 {
  def sigma(((a, b, f))) = {
    
      if (
        a < 0 || b < 0
      ) {
        assert(false, "Invalid_argument with negative argument ") 
      } else if (
        a == b
      ) {
        f(b) 
      } else if (
        a > b
      ) {
        0 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
}