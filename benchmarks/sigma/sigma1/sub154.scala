import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub154 {
  def sigma(((num1, num2, f))) = {
    
      if (
        num1 == num2
      ) {
        f(num1) 
      } else if (
        num1 < num2
      ) {
        f(num1) + sigma(num1 + 1, num2, f) 
      } else {
        0
      }
  }
}
