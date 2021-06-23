import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub113 {
  def sigma(((int1, int2, func))) = {
    
      if (
        int1 > int2
      ) {
        0 
      } else if (
        int1 eq int2
      ) {
        func(int1) 
      } else {
        func(int1) + sigma(int1 + 1, int2, func)
      }
  }
}