import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub422 {
  def sigma(((a: Int63, b: Int63, f: A => A))): A = {
    if (a > b) 0 else sigma(a + 1, b, f) + f(a)
  }
}