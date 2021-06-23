import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub69 {
  def iter(((n, f))) = {
    
      if (
        n < 0
      ) {
        assert(false, "Invalid_argument with worng input ") 
      } else if (
        n == 0
      ) {
        ( (x) => { x } ) 
      } else {
        ( (x) => { iter(n - 1, f, f(x)) } )
      }
  }
}