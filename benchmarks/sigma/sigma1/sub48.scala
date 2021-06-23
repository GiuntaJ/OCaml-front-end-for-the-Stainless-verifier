import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub48 {
  sealed case class Error(param0: String) extends Exception {}
  
  def sigma(((a, b, func))) = {
    
      if (
        a < b
      ) {
        func(a) + sigma(a + 1, b, func) 
      } else if (
        a == b
      ) {
        func(b) 
      } else {
        assert(false, "Error with invalid")
      }
  }
}