import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub16 {
  /* School of Computer Science & Engineering
   * 2009-23151
   * 조성근
   * HW 1 - Exercise 1
   */
  
  sealed case class Error(param0: String) extends Exception {}
  
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        assert(false, "Error with arg0 > arg1 ") 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
   
}