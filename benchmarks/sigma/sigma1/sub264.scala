import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub264 {
  def sigma(((a, b, f))) = {
    
      if (
        a eq b
      ) {
        f(a) 
      } else if (
        a < b
      ) {
        f(a) + sigma(a + 1, b, f) 
      } else {
        0
      }
  }
}
