import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub464 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (min, max, func) =>
      {
        
          if (
            min > max
          ) {
            0 
          } else if (
            min == max
          ) {
            func(min) 
          } else {
            func(min) + sigma(min + 1, max, func)
          }
    }
  }
}