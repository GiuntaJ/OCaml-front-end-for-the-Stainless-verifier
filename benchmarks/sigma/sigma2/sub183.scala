import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub183 {
  def sigma(f: Int63 => Int63, x: Int63, y: Int63): Int63 = {
    if (y - x == 0) f(x) else f(x) + sigma(f, x + 1, y)
  }
}