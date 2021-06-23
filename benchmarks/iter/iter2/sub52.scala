import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub52 {
  /*problem 3*/
    def comp(f, g, x) = { g(f(x)) }
    def iter(((n, f))) = {
    
      if (
        n == 0
      ) {
        ( (x) => { x } ) 
      } else if (
        n == 1
      ) {
        f 
      } else {
        comp(f, iter(n - 1, f))
      }
  }
}