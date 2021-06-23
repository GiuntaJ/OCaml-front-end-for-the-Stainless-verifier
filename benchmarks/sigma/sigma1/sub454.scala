import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub454 {
  /* let rec sigma ((a: int), (b: int), (f: int -> int)): int = */
  def sigma(((a, b, f))) = {
    a > b match {
      case true => { 0 }
      case false => { f(a) + sigma(a + 1, b, f) }
    }
  }
}