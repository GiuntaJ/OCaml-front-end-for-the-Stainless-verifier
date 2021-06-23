import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub92 {
  /* 200511843 LEE JONGHO */
  
  def sigma(((a, b, f))) = {
    a match {
      case b => { f(a) }
      case _ => { sigma(a + 1, b + 0, f) + f(a) }
    }
  }
}