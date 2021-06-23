import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub167 {
  def iter(((n: Int63, f: A => A)), x) = {
    
      if (
        n == 0
      ) {
        x 
      } else if (
        n < 0
      ) {
        assert(false, "Exit") 
      } else {
        f(iter(n - 1, f, x))
      }
  }
}