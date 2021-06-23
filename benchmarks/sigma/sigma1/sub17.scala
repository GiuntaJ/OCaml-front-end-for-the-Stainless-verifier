import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub17 {
  sealed case class Error(param0: String) extends Exception {}
  
  /* EX1 : sigma */
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        assert(false, "Error with invalid input ") 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
}