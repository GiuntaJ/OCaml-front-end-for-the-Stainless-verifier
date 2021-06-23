import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub81 {
  sealed case class ERROR(param0: String) extends Exception {}
  
  def iter(((n, f))) = {
    
      if (
        n < 0
      ) {
        assert(false, "ERROR with invalid number ") 
      } else if (
        n == 0
      ) {
        ( (x) => { x } ) 
      } else {
        ( (x) => { iter(n - 1, f, f(x)) } )
      }
  }
}