import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub19 {
  sealed case class Error(param0: String) extends Exception {}
  def sigma(((a, b, f))) = {
    
      if (
        a < b
      ) {
        f(a) + sigma(a + 1, b, f) 
      } else if (
        a eq b
      ) {
        f(a) 
      } else {
        assert(false, "Error with a should not be greater than b!")
      }
  }
}