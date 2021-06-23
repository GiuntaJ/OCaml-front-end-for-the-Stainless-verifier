import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub484 {
  def sigma(x: (Int63, Int63, (Int63 => Int63))): Int63 = {
    val _2 = {
      val ((a, b, f)) = x
      if (a > b) 0 else sigma(a + 1, b, f) + f(a)
    }
  }
}