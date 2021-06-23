import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub109 {
  /* HW1 exercise2 2009-11697 Kim HyunJoon */
  /* Sigma */
  
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) =>
      {
        
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
}