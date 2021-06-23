import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub216 {
  def sigma(((a, b, func))) = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a == b
      ) {
        func(a) 
      } else {
        sigma(a, b - 1, func) + func(b)
      }
  }
}