import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub3 {
  sealed case class RangeError() extends Exception {}
  /*2011120109 Jung-su Han Homework*/
  
  /* Problem 2 */
  
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        
          if (
            a > b
          ) {
            assert(false, "RangeError") 
          } else if (
            a == b
          ) {
            f(a) 
          } else {
            f(a) + sigma(f, a + 1, b)
          }
    }
  }
  
  /* defines exception for range being large number to smaller number */
}