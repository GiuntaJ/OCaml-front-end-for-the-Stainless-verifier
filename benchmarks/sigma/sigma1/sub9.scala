import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub9 {
  sealed case class BAD() extends Exception {}
  
  def sigma(((a, b, f))) = {
    
      if (
        a < 0 || b < 0 || a > b
      ) {
        assert(false, "BAD") 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
}
