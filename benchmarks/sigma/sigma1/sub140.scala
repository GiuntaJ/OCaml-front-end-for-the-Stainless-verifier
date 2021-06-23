import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub140 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) =>
      {
        
          if (
            b == a
          ) {
            f(b) 
          } else if (
            b > a
          ) {
            f(b) + sigma(a, b - 1, f) 
          } else {
            0
          }
    }
  }
}