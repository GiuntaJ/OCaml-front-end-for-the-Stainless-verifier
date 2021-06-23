import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub433 {
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))) = {
    if (a <= b) f(a) + sigma(a + 1, b, f) else 0
  }
}