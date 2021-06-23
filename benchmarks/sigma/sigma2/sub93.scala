import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub93 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, n1, n2) => { if (n1 <= n2) f(n1) + sigma(f, n1 + 1, n2) else 0 }
  }
}