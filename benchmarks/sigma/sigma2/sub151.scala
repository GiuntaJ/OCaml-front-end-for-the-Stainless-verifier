import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub151 {
  def sigma(f, a, b) = {
    
      if (
        a < b
      ) {
        f(b) + sigma(f, a, b - 1) 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        f(a) + sigma(f, a - 1, b)
      }
  }
}
