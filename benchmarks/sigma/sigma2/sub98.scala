import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub98 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        
          if (
            a > b
          ) {
            f(a) + sigma(f, b, a - 1) 
          } else if (
            a == b
          ) {
            f(b) 
          } else {
            f(b) + sigma(f, a, b - 1)
          }
    }
  }
   /* TODO */
}