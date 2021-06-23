import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub332 {
  def sigma(((a, b, n))) = {
    
      if (
        a < b
      ) {
        n(a) + sigma(a + 1, b, n) 
      } else if (
        a > b
      ) {
        0 
      } else {
        n(b)
      }
  }
  
}
