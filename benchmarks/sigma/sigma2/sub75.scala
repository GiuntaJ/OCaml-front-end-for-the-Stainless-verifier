import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub75 {
  def sigma(f: Int63 => Int63, a: Int63, b: Int63) = {
    if (a == b) f(a) else f(a) + sigma(f, a + 1, b)
  }
}