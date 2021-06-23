import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub51 {
  /* 4190.310 Programming Language		*
   * Homework #1 - Exercise 1 (씨그마)	*
   * 2008-11744 Jongwook Choi 			*/
  
  sealed case class Error(param0: String) extends Exception {}
  
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        assert(false, "Error with Assertion 'a <= b' failed ") 
      } else if (
        a eq b
      ) {
        f(a) 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
  
}
