import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub74 {
  sealed case class Problem() extends Exception {}
  
  /* problem 3*/
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n == 0
          ) {
            ( (x) => { x } ) 
          } else if (
            n > 0
          ) {
            ( (x) => { f(iter(n - 1, f, x)) } ) 
          } else {
            assert(false, "Problem")
          }
    }
  }
}