import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub424 {
  /*2016-11690*/
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a < b
      ) {
        f(a) + sigma(a + 1, b, f) 
      } else {
        f(a)
      }
  }
}