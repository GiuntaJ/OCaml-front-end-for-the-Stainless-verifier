import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub49 {
  sealed case class Error(param0: String) extends Exception {}
  
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        assert(false, "Error with Invalid argument ") 
      } else if (
        a == b
      ) {
        f(b) 
      } else {
        sigma(a + 1, b, f) + f(a)
      }
  }
}