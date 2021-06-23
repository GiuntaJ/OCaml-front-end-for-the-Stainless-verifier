import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub7 {
  sealed case class Error(param0: String) extends Exception {}
  
  def iter(((n, f)), r) = {
    
      if (
        n == 0
      ) {
        r 
      } else if (
        n > 0
      ) {
        iter(n - 1, f, f(r)) 
      } else {
        assert(false, "Error with invalid n")
      }
  }
}