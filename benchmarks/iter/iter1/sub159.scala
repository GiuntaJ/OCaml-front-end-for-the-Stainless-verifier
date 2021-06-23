import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub159 {
  def iter(((n, f))) = {
    
      if (
        n > 0
      ) {
        val _3 = {
          val g = iter(n - 1, f)
          ( (x) => { f(g(x)) } )
        } 
      } else {
        ( (x) => { x } )
      }
  }
}