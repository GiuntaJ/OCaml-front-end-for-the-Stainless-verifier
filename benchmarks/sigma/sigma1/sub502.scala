import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub502 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, g) =>
      {
        
          if (
            a > b
          ) {
            0 
          } else if (
            a eq b
          ) {
            g(a) 
          } else {
            sigma(a + 1, b, g) + g(a)
          }
    }
  }
}