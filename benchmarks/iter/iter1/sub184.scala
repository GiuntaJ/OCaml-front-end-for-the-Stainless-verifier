import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub184 {
  def compose(f, g, x) = { f(g(x)) }
  
  def iter(((a, f))) = {
    
      if (
        a == 0
      ) {
        ( (x) => { x } ) 
      } else if (
        a == 1
      ) {
        f 
      } else if (
        a < 0
      ) {
        ( (x) => { x } ) 
      } else {
        compose(f, iter(a - 1, f))
      }
  }
  
}
