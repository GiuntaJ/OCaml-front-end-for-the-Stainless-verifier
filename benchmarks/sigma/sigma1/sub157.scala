import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub157 {
  def sigma(((a, b, n))) = {
    
      if (
        a <= b
      ) {
        val _3 = {
          def sum(((a, b, n))) = { if (a == b) n(a) else n(a) + sum(a + 1, b, n)
          }
          sum(a, b, n)
        } 
      } else {
        0
      }
  }
}