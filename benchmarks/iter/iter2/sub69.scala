import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub69 {
  /*3*/
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n > 1
          ) {
            ( (x) => { f(iter(n - 1, f, x)) } ) 
          } else if (
            n == 1
          ) {
            ( (x) => { f(x) } ) 
          } else {
            ( (x) => { x } )
          }
    }
  } 
}