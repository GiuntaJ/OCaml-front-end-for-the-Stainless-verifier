import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub81 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) =>
      {
        
          if (
            a > b
          ) {
            assert(false, "Invalid_argument with error ") 
          } else if (
            a == b
          ) {
            f(a) 
          } else {
            f(a) + sigma(a + 1, b, f)
          }
    }
  }
}
