import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub58 {
  /* C:\Users\owner\Desktop\Homework 1(1).ml */
  
  sealed case class Error(param0: String) extends Exception {}
  
  def sigma(((a, b, f))) = {
    
      if (
        a eq b
      ) {
        f(a) 
      } else if (
        a < b
      ) {
        f(a) + sigma(a + 1, b, f) 
      } else {
        assert(false, "Error with FAIL!")
      }
  }
}
