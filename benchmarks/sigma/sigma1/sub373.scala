import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub373 {
  def sigma_0(((a: Int63, b: Int63, f: Int63 => Int63, subSum: Int63))): Int63 = {
    if (a == b) f(a) + subSum else sigma_0(a + 1, b, f, subSum + f(a))
  }
  
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))): Int63 = {
    if (a > b) 0 else sigma_0(a, b, f, 0)
  }
}