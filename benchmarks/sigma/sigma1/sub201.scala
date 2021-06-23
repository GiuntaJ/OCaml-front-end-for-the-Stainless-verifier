import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub201 {
  def sigma(((sn, en, f))) = {
    
      if (
        sn > en
      ) {
        0 
      } else if (
        sn == en
      ) {
        f(sn) 
      } else {
        f(en) + sigma(sn, en - 1, f)
      }
  }
}