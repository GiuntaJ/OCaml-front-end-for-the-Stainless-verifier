import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub170 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, p, q) => { if (p == q) f(p) else f(p) + sigma(f, p + 1, q) }
  }
}