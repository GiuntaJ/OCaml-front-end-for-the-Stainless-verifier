import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub76 {
  def iter(((n, f))) = {
    val _2 = {
      def identity(s) = { s }
      val _3 = {
        def newiter(x) = { iter(n - 1, f, f(x)) }
        
          if (
            n < 0
          ) {
            invalid_arg("iter") 
          } else if (
            n == 0
          ) {
            identity 
          } else {
            newiter
          }
      }
    }
  }
}
