import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub16 {
  /* School of Computer Science & Engineering
   * 2009-23151
   * 조성근
   * HW 1 - Exercise 2
   */
  
  sealed case class Error(param0: String) extends Exception {}
  
  def iter(((n, f)), a) = {
    
      if (
        n == 0
      ) {
        a 
      } else if (
        n < 0
      ) {
        assert(false, "Error with n is negative ") 
      } else {
        f(iter(n - 1, f, a))
      }
  }
}