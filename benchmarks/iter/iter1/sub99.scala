import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub99 {
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      {
        
          if (
            n < 0
          ) {
            assert(false, "Exit") 
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