import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub375 {
  /*컴퓨터공학부 2014-16775 김민지
  programming language hw 1-2*/
  
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))): Int63 = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a eq b
      ) {
        f(b) 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
}