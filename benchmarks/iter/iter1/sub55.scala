import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub55 {
  def iter(((n, f))) = {
    val _2 = {
      def iden(f, a) = { a }
      val _3 = {
        def plus(f, g, a) = { f(g(a)) }
        
          if (
            n == 0
          ) {
            iden(f) 
          } else if (
            n < 0
          ) {
            assert(false, "Failure with iter ") 
          } else if (
            n == 1
          ) {
            f 
          } else {
            plus(f, iter(n - 1, f))
          }
      }
    }
  }
}