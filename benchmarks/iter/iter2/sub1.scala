import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub1 {
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def iters(((n, f)), x) = {
            
              if (
                n == 0
              ) {
                ( (n) => { n } )(x) 
              } else if (
                n == 1
              ) {
                f(x) 
              } else {
                f(iters(n - 1, f, x))
              }
          }
          iters(n, f)
        }
    }
  }
}