import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub186 {
  /* Name : Jungwon Seo / Student ID : 2012210051 */
  
  sealed case class Problem() extends Exception {}
  
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        
          if (
            a > b
          ) {
            assert(false, "Problem") 
          } else if (
            a == b
          ) {
            f(a) 
          } else {
            f(b) + sigma(f, a, b - 1)
          }
    }
  }
}