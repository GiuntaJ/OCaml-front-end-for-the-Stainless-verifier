import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub43 {
  /* 2009-11824 Jieun-Jeong HW1-2 */
  
  def iter(((n, ftn)), x) = {
    
      if (
        n < 0
      ) {
        assert(false, "Invalid_argument with n is nonnegative ") 
      } else if (
        n eq 0
      ) {
        x 
      } else {
        iter(n - 1, ftn, ftn(x))
      }
  }
}