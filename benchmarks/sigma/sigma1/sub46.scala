import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub46 {
  sealed case class Error(param0: String) extends Exception {}
  def sigma(((a, b, f))) = {
    
      if (
        b == a
      ) {
        f(a) 
      } else if (
        b < a
      ) {
        assert(false, "Error with a is bigger than b. ") 
      } else {
        f(b) + sigma(a, b - 1, f)
      }
  }
}