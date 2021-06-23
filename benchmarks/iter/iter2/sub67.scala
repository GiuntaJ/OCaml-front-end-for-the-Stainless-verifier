import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub67 {
  sealed case class NO_NEGATIVE_INTEGERS() extends Exception {}
  
  /* problem 3*/
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def identity(x) = { x + 0 }
          val _3 = {
            def compose(f, g, x) = { f(g(x)) }
            
              if (
                n eq 0
              ) {
                identity 
              } else if (
                n < 0
              ) {
                assert(false, "NO_NEGATIVE_INTEGERS") 
              } else {
                compose(f, iter(n - 1, f))
              }
          }
        }
    }
  }
}