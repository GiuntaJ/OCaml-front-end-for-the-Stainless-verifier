import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub321 {
  /* hw1 ex2 "sigma" */
  
  
  def sigma(((a, b, f))): Int63 = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
}