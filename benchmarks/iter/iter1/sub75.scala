import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub75 {
  /* hw1-3 */
  /* 2010-11687 Keunjun Choi */
  
  sealed case class ERROR(param0: String) extends Exception {}
  def iter(((n, f))) = {
    
      if (
        n < 0
      ) {
        assert(false, "ERROR with n>=0 ") 
      } else if (
        n == 0
      ) {
        ( (x) => { x } ) 
      } else {
        ( (x) => { f(iter(n - 1, f, x)) } )
      }
  }
}
