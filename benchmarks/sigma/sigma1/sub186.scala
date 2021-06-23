import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub186 {
  def sigma(((i, j, k))) = {
    (i, j, k) match {
      case (p, q, f) => {
        
          if (
            p == q
          ) {
            f(p) 
          } else if (
            p > q
          ) {
            0 
          } else {
            f(p) + sigma(p + 1, q, f)
          }
      }
    }
  }
}