import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub443 {
  /* 
  2011-10634
  JooHyun Jo / Major in Economics
  problem 2 for HW1
  */
  
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))): Int63 = {
    if (a > b) 0 else f(a) + sigma(a + 1, b, f)
  }
}
