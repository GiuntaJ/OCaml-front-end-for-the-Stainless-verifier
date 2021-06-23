import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub73 {
  sealed case class InvalidInput(param0: String) extends Exception {}
  
  def iter(((n, func))) = {
    
      if (
        n < 0
      ) {
        assert(false, "InvalidInput with n is negative ") 
      } else if (
        n eq 0
      ) {
        ( (x) => { x } ) 
      } else {
        ( (x) => { func(iter(n - 1, func, x)) } )
      }
  }
}