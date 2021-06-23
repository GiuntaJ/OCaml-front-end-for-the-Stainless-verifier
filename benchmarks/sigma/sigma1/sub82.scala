import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub82 {
  def sigma(((a, b, f))) = {
    a - b match {
      case 0 => { f(a) }
      case _ => { f(a) + sigma(a + 1, b, f) }
    }
  }
}