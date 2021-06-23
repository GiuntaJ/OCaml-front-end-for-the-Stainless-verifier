import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub145 {
  def iter(((n, f)), a) = {
    
      if (
        n eq 0
      ) {
        a 
      } else if (
        n > 0
      ) {
        f(iter(n - 1, f, a)) 
      } else {
        a
      }
  }
}