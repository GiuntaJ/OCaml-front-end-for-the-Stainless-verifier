import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub151 {
  def sigma(((s, e, f))) = {
    
      if (
        s > e
      ) {
        0 
      } else if (
        s == e
      ) {
        f(s) 
      } else {
        f(s) + sigma(s + 1, e, f)
      }
  }
}