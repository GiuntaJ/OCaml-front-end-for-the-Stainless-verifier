import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub115 {
  /* hw1-2 */
  /* 2010-11687 Keunjun Choi */
  
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        sigma(a + 1, b, f) + f(a)
      }
  }	
}