import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub83 {
  /* 2009-11824 Jieun-Jeong HW1-1 */
  
  def sigma(((a, b, ftn))) = {
    
      if (
        a > b
      ) {
        assert(false, "Invalid_argument with a is bigger than b ") 
      } else if (
        a eq b
      ) {
        ftn(a) 
      } else {
        ftn(a) + sigma(a + 1, b, ftn)
      }
  }
}