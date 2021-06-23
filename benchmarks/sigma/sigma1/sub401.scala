import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub401 {
  /* 2015-11380 박찬양 HW1-2 */
  
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, func) =>
      {
        
          if (
            a == b
          ) {
            func(a) 
          } else if (
            a > b
          ) {
            0 
          } else {
            func(a) + sigma(a + 1, b, func)
          }
    }
  }
}