import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub1 {
  /* exception */
  sealed case class Improper_input() extends Exception {}
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        
          if (
            a > b
          ) {
            assert(false, "Improper_input") 
          } else if (
            a == b
          ) {
            f(b) 
          } else {
            f(a) + sigma(f, a + 1, b)
          }
    }
  }
}