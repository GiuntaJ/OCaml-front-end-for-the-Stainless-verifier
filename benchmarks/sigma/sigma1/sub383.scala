import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub383 {
  /* 2014-19180 You JooSeung Question 2*/
  
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))): Int63 = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a ne b
      ) {
        f(a) + sigma(a + 1, b, f) 
      } else {
        f(a)
      }
  }
}