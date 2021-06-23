import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub249 {
  def incr(n: Int63): Int63 = { n + 1 }
  
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        f(a) + sigma(incr(a), b, f)
      }
  }
}