import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub131 {
  def iter(((n, funx)), a) = {
    
      if (
        n < 1
      ) {
        0 
      } else if (
        n == 1
      ) {
        funx(a) 
      } else {
        iter(n - 1, funx, funx(a))
      }
  }
}