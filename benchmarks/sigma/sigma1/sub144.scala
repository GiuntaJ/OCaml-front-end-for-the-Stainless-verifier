import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub144 {
  sealed case class Large_a() extends Exception {}
  
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))) = {
    
      if (
        a > b
      ) {
        assert(false, "Large_a") 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
}