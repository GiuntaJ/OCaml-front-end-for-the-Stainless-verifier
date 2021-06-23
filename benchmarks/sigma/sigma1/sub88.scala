import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub88 {
  /* 2009-13384, CHO Hyunik */
  
  
  def sigma(((a, b, f))) = {
    a > b match {
      case true => { 0 }
      case _ => { f(a) + sigma(a + 1, b, f) }
    }
  }
}