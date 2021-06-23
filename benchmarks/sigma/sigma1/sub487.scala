import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub487 {
  def sigma(t: (Int63, Int63, (Int63 => Int63))): Int63 = {
    val _2 = {
      val ((n, m, f)) = t
      if (n <= m) f(n) + sigma(n + 1, m, f) else 0
    }
  }
}