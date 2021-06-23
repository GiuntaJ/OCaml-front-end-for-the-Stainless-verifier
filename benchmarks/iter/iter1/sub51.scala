import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub51 {
  def iter(((n, f))) = {
    
      if (
        n ne 0
      ) {
        ( (x) => { iter(n - 1, f, f(x)) } ) 
      } else if (
        n == 0
      ) {
        ( (x) => { x } ) 
      } else {
        assert(
          false,
          "Invalid_argument with         1st argu must be equal to or bigger than 0!")
      }
  }
}