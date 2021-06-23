import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub288 {
  def sigma(((a, b, f))) = {
    
      if (
        b > a
      ) {
        sigma(a, b - 1, f) + f(b) 
      } else if (
        b < a
      ) {
        0 
      } else {
        f(a)
      }
  }
}
