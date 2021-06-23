import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub77 {
  sealed case class ERR_invalid_iter_val() extends Exception {}
  
  def iter(((n, f))) = {
    
      if (
        n eq 0
      ) {
        ( (x) => { x } ) 
      } else if (
        n eq 1
      ) {
        f 
      } else if (
        n > 1
      ) {
        ( (x) => { iter(n - 1, f, f(x)) } ) 
      } else {
        assert(false, "ERR_invalid_iter_val")
      }
  }
}