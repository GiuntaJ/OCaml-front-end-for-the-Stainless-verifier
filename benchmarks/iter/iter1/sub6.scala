import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub6 {
  sealed case class Error(param0: String) extends Exception {}
  
  def iter(((n, f)), x) = {
    
      if (
        n < 0
      ) {
        assert(false, "Error with n should not be negative ") 
      } else if (
        n == 0
      ) {
        x 
      } else {
        f(iter(n - 1, f, x))
      }
  }
}