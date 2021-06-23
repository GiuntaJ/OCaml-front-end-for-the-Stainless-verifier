import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub153 {
  def sigma(tmp: Int63 => Int63, a, b) = {
    if (a > b) 0 else tmp(a) + sigma(tmp, a + 1, b)
  }
}