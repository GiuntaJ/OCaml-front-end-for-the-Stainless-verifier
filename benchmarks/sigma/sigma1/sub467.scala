import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub467 {
  
  /*
      컴퓨터공학부 2012-11270 장선웅
      hw 1 - Exercise 2
  */
  
  
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a == b
      ) {
        f(b) 
      } else {
        sigma(a + 1, b, f) + f(a)
      }
  }
}
