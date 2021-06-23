import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub47 {
  
  /* problem 3*/
  
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n == 0
          ) {
            f 
          } else {
            val _3 = {
              def a(n, f, x) = { if (n > 0) f(a(n - 1, f, x)) else x }
              a(n, f)
            }
          }
    }
  }
}