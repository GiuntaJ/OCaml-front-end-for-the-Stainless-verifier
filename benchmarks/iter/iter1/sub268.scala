import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub268 {
  def iter(((n, f))) = {
    val _2 = {
      def combine_functions(f1, f2, n) = {
        
          if (
            n == 0
          ) {
            ( (x) => { x } ) 
          } else if (
            n == 1
          ) {
            f1 
          } else {
            combine_functions(( (x) => { f1(f2(x)) } ), f2, n - 1)
          }
      }
      combine_functions(f, f, n)
    }
  }
}