import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub77 {
  
  /* ex 1 */
  
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        assert(false, "Invalid_argument with sigma ") 
      } else if (
        a < b
      ) {
        f(a) + sigma(a + 1, b, f) 
      } else {
        f(a)
      }
  }
}
