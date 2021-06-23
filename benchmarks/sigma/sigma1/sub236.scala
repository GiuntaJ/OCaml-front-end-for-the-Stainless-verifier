import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub236 {
  /* sigma : int * int * (int -> int) -> int  */
  
  def sigma(((a, b, func))) = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a eq b
      ) {
        func(a) 
      } else {
        func(a) + sigma(a + 1, b, func)
      }
  }
  
}
