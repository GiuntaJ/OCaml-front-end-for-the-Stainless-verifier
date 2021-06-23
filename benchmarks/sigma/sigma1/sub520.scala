import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub520 {
  def sigma(((int1: Int63, int2: Int63, f: Int63 => Int63))): Int63 = {
    
      if (
        int1 eq int2
      ) {
        f(int2) 
      } else if (
        int2 > int1
      ) {
        sigma(int1 + 1, int2, f) + f(int1) 
      } else {
        0
      }
  }
}