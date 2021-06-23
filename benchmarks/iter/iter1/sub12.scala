import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub12 {
  sealed case class Error(param0: String) extends Exception {}
  
  def iter(((n, f)), x) = {
    
      if (
        n < 0
      ) {
        assert(false, "Error with n is negative number ") 
      } else {
        n match {
          case 0 => { x }
          case 1 => { f(x) }
          case _ => { f(iter(n - 1, f, x)) }
        }
      }
  }
}