import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub102 {
  def sigma(((n1, n2, f))) = {
    
      if (
        n1 > n2
      ) {
        0 
      } else if (
        n1 == n2
      ) {
        f(n1) 
      } else {
        f(n1) + sigma(n1 + 1, n2, f)
      }
  }
}