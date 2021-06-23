import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub123 {
  sealed case class ERROR(param0: String) extends Exception {}
  
  def sigma(((a, b, f))) = {
    
      if (
        a < b
      ) {
        f(a) + sigma(a + 1, b, f) 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        0
      }
  }
}