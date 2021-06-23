import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub228 {
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      {
        
          if (
            n eq 0
          ) {
            ( (x) => { x } ) 
          } else if (
            n eq 1
          ) {
            f 
          } else {
            ( (x) => { iter(n - 1, f, f(x)) } )
          }
    }
  }
}
