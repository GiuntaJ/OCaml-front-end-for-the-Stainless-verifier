import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub451 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) => { if (a eq b) f(a) else f(a) + sigma(a + 1, b, f) }
  }   
}