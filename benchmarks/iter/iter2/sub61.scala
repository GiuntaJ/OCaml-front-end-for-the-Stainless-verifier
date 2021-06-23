import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub61 {
  def compose(f, g, x) = { f(g(x)) }
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n == 0
          ) {
            ( (x) => { x } ) 
          } else if (
            n > 1
          ) {
            compose(f, iter(n - 1, f)) 
          } else {
            f
          }
    }
  }
}