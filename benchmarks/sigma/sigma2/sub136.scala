import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub136 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        
          if (
            a > b
          ) {
            0 
          } else if (
            b == a
          ) {
            f(a) 
          } else {
            f(b) + sigma(f, a, b - 1)
          }
    }
  }
  
}
