import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub257 {
  sealed case class Todo() extends Exception {}
  
  def sigma(((a, b, f))): Int63 = {
    
      if (
        a == b
      ) {
        f(b) 
      } else if (
        a > b
      ) {
        0 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
  	
  	
}