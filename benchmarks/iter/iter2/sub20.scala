import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub20 {
  /* problem 3*/
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case ((n, f), a) =>
      {
        
          if (
            n == 0
          ) {
            a 
          } else if (
            n == 1
          ) {
            f(a) 
          } else {
            iter(n - 1, f, iter(1, f, a))
          }
    }
  }
}