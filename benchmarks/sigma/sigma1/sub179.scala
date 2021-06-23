import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub179 {
  def sigma(((a, b, f))) = { if (a > b) 0 else sigma(a + 1, b, f) + f(a) }
  
  def test_func(a: Int63): Int63 = { a * 2 }
}
