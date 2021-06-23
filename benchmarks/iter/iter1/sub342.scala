import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub342 {
  def iter(((int_i: Int63, f: A => A))): A => A = {
    
      if (
        int_i <= 0
      ) {
        ( (x) => { x } ) 
      } else {
        ( (x) => { f(iter(int_i - 1, f, x)) } )
      }
  }
}