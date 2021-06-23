import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub242 {
  /*컴퓨터공학부 2010-11779 박진영 1.1*/
  def sigma(((a, b, f))) = {
    val _2 = {
      def cal(((a, b, f, rs))) = {
        if (a > b) rs else cal(a + 1, b, f, rs + f(a))
      }
      if (a > b) 0 else cal(a, b, f, 0)
    }
  }
    
  
    
}