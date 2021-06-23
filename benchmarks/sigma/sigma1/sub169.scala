import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub169 {
  def sigma(((a, b, x))) = {
    
      if (
        b == a
      ) {
        x(b) 
      } else if (
        a > b
      ) {
        0 
      } else {
        x(a) + sigma(a + 1, b, x)
      }
  }
  
}
