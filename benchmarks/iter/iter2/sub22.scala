import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub22 {
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n < 0
          ) {
            assert(false, "Failure with n is negative number. ") 
          } else if (
            n == 0
          ) {
            ( (x) => { x } ) 
          } else {
            ( (x) => { iter(n - 1, f, f(x)) } )
          }
    }
  }
}