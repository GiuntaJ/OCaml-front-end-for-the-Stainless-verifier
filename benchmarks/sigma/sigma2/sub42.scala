import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub42 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        
          if (
            a == 0 || b == 0
          ) {
            0 
          } else if (
            a == b
          ) {
            a 
          } else {
            f(b) + sigma(f, a, b - 1)
          }
    }
  }
}