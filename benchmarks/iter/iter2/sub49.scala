import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub49 {
  /* problem 3*/
  
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def iter_inter(((n, f)), x) = {
            
              if (
                n == 0
              ) {
                x 
              } else if (
                n == 1
              ) {
                f(x) 
              } else {
                f(x) * iter_inter(n - 1, f, x)
              }
          }
          iter_inter(n, f)
        }
    }
  }
}