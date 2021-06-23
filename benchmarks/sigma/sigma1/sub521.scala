import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub521 {
  def sigma(((startnum: Int63, endnum: Int63, fnt: Int63 => Int63))): Int63 = {
    
      if (
        startnum > endnum
      ) {
        0 
      } else {
        fnt(startnum) + sigma(startnum + 1, endnum, fnt)
      }
  }
}