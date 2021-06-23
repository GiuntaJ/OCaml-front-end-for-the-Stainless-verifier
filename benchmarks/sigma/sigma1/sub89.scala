import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub89 {
  def sigma(((a, b, f))) = {
    
      if (
        b > a
      ) {
        f(b) + sigma(a, b - 1, f) 
      } else if (
        b == a
      ) {
        f(b) 
      } else {
        assert(
          false,
          "Invalid_argument with         1st argu must be equal to or less than 2nd argu!")
      }
  }
}