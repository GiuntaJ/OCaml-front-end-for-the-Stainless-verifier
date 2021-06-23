import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub19 {
  sealed case class Error(param0: String) extends Exception {}
  def iter(((n, f))) = {
    val _2 = {
      def compose(f, g, x) = { f(g(x)) }
      
        if (
          n < 0
        ) {
          assert(false, "Error with negative value! ") 
        } else if (
          n eq 0
        ) {
          ( (x) => { x } ) 
        } else {
          compose(f, iter(n - 1, f))
        }
    }
  }
}
