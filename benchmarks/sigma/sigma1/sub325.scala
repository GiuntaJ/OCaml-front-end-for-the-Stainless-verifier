import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub325 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, func) =>
      {
        
          if (
            a > b
          ) {
            0 
          } else if (
            a eq b
          ) {
            func(a) 
          } else {
            sigma(a + 1, b, func) + func(a)
          }
    }
  }
}