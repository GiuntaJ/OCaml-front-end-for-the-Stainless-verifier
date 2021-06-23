import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub134 {
  def sigma(((i1, i2, f))) = {
    
      if (
        i1 > i2
      ) {
        0 
      } else if (
        i1 == i2
      ) {
        f(i1) 
      } else {
        f(i1) + sigma(i1 + 1, i2, f)
      }
  }
}