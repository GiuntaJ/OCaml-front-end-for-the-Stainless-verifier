import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub45 {
  sealed case class InvalidRange() extends Exception {}
  
  def iter(((n, f))) = {
    
      if (
        n eq 0
      ) {
        ( (x) => { x } ) 
      } else if (
        n > 0
      ) {
        ( (x) => { f(iter(n - 1, f, x)) } ) 
      } else {
        assert(false, "InvalidRange")
      }
  }
}
