import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub43 {
  def sigma(((i_start, i_end, f))) = {
    
      if (
        i_start == i_end
      ) {
        f(i_start) 
      } else {
        f(i_start) + sigma(i_start + 1, i_end, f)
      }
  }
}