import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub276 {
  sealed case class Not_minus(param0: String) extends Exception {}
  def iter(((n, f))) = {
    
      if (
        n < 0
      ) {
        assert(false, "Not_minus with n cannot be less than 0 ") 
      } else {
        n == 0 match {
          case true => { ( (x) => { x } ) }
          case false => { ( (x) => { iter(n - 1, f, f(x)) } ) }
        }
      }
  }
}