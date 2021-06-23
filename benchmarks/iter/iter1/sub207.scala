import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub207 {
  def iter(((a, f))) = {
    
      if (
        a == 0 || a < 0
      ) {
        ( (x) => { x } ) 
      } else {
        ( (x) => { iter(a - 1, f, f(x)) } )
      }
  }
}
