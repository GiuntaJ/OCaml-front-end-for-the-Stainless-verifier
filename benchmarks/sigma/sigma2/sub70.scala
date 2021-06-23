import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub70 {
  def sigma(test: Int63 => Int63, x: Int63, y: Int63): Int63 = {
    if (x <= y) test(x) + sigma(test, x + 1, y) else 0
  }
}