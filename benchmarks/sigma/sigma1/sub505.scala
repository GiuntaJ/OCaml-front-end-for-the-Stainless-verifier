import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub505 {
  def sigma2(((a: Int63, b: Int63, f: Int63 => Int63, r: Int63))): Int63 = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a == b
      ) {
        f(a) + r 
      } else {
        sigma2(a + 1, b, f, f(a) + r)
      }
  }
  
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))): Int63 = {
    sigma2(a, b, f, 0)
  }
}