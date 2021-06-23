import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub28 {
  /* problem 3*/
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n < 0
          ) {
            assert(false, "Failure with cannot iterate minus times ") 
          } else {
            n match {
              case 0 => { ( (x) => { x } ) }
              case _ => { ( (x) => { f(iter(n - 1, f, x)) } ) }
            }
          }
    }
  }
}