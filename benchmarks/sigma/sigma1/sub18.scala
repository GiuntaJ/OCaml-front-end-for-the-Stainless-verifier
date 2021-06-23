import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub18 {
  sealed case class E() extends Exception {}
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        assert(false, "E") 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
}