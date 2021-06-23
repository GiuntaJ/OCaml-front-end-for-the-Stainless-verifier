import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub15 {
  /* Problem 2 */
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        
          if (
            b == 1
          ) {
            1 
          } else if (
            a ne 1
          ) {
            sigma(f, 1, b) - sigma(f, 1, a) 
          } else {
            f(b) + sigma(f, a, b - 1)
          }
    }
  }
}