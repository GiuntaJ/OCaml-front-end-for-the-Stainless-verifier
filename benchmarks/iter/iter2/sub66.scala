import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub66 {
  /* problem 3*/
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n == 0
          ) {
            ( (x) => { x } ) 
          } else {
            val _3 = {
              val g = iter(n - 1, f)
              ( (x) => { f(g(x)) } )
            }
          }
    }
  }
}