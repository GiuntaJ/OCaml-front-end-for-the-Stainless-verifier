import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub30 {
  sealed case class Error(param0: String) extends Exception {}
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) =>
      {
        
          if (
            a > b
          ) {
            assert(false, "Error with a is bigger than b ") 
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