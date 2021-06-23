import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub76 {
  /* 컴퓨터공학부/2009-11679/김정명/1 */
  
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        assert(false, "Invalid_argument with sigma ") 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        sigma(a + 1, b, f) + f(a)
      }
  }
}