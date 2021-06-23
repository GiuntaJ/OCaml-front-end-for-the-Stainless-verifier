import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub46 {
  def iter(((n, f)), a) = {
    
      if (
        n < 0
      ) {
        assert(false, "Invalid_argument with iter ") 
      } else if (
        n == 0
      ) {
        a 
      } else if (
        n == 1
      ) {
        f(a) 
      } else {
        f(iter(n - 1, f, a))
      }
  }
}