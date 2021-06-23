import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub66 {
  sealed case class BoundError() extends Exception {}
  
  def sigma(((a, b, f))) = {
    
      if (
        a < b
      ) {
        sigma(a + 1, b, f) + f(a) 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        assert(false, "BoundError")
      }
  }
}