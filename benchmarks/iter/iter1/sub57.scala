import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub57 {
  sealed case class Error(param0: Int63) extends Exception {}
  
  def iter00(((n, f)), a) = {
    
      if (
        n == 0
      ) {
        a 
      } else if (
        n == 1
      ) {
        f(a) 
      } else if (
        n > 1
      ) {
        f(iter00(n - 1, f, a)) 
      } else {
        assert(false, "Error with n")
      }
  }
  
  def iter(((n, f))) = { iter00(n, f) }
  	
}