import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub481 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a: Int63, b: Int63, f: Int63 => Int63) =>
      {
        
          if (
            a > b
          ) {
            0 
          } else if (
            a == b
          ) {
            f(a) 
          } else {
            f(b) + sigma(a, b - 1, f)
          }
    }
  }
}