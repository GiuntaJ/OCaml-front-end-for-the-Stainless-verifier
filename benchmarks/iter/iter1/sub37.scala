import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub37 {
  
  /* ex 2 */
  
  def iter(((n, f)), x) = {
    
      if (
        n < 0
      ) {
        assert(false, "Invalid_argument with iter ") 
      } else if (
        n eq 0
      ) {
        x 
      } else {
        f(iter(n - 1, f, x))
      }
  }
}
